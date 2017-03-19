{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}


module InfoVis.Parallel.Process.Select (
  selecter
) where


import Control.Arrow ((***), second)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar, swapMVar)
import Control.DeepSeq (($!!))
import Control.Distributed.Process (Process, ReceivePort, SendPort, liftIO, receiveChan, sendChan)
import Control.Monad (forever, void)
import Data.Bit (Bit, fromBool)
import Data.Hashable (Hashable)
import Data.List (nub)
import Data.Vector.Unboxed.Bit (intersection, invert, listBits, union, symDiff)
import InfoVis.Parallel.Process.Util (Debug(..), currentHalfFrame, frameDebug)
import InfoVis.Parallel.Rendering.Types (DisplayList(..), DisplayType(LinkType))
import InfoVis.Parallel.Types (Coloring(..))
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..), Configuration(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), SelectionAction(..), messageTag, nextMessageIdentifier)
import InfoVis.Parallel.Types.Presentation (Presentation(..))
import InfoVis.Parallel.Types.World (World(..))
import Linear.Affine (Point(..), (.-.), (.+^), (.-^))
import Linear.Conjugate (conjugate)
import Linear.Metric (quadrance)
import Linear.Quaternion (Quaternion(..), rotate)
import Linear.V3 (V3(..))
import Linear.Util.Graphics (fromVertex3)
import Linear.Vector (zero)
import Text.Printf (printf)

import qualified Data.HashMap.Strict as H (HashMap, empty, fromListWith, lookupDefault, map, unionWith)
import qualified Data.HashSet as S (fromList, toList)
import qualified Data.Map.Strict as M ((!), fromListWith, keys, size)
import qualified Data.Vector.Unboxed as U (Vector, accum, length, replicate)


type Spatial a = (a, H.HashMap (Point V3 Int) [(Int, Point V3 a)])


consolidate :: (Eq a, Hashable a) => [a] -> [a]
consolidate = S.toList . S.fromList


newSpatial :: (Hashable a, RealFloat a) => a -> DisplayList b Int  -> (b, Spatial a)
newSpatial delta DisplayList{..} =
  (
    listIdentifier
  , (
      delta
    , H.map consolidate
        $ H.fromListWith (++)
        [
          (xyz, [(vertexIdentifier, realToFrac <$> fromVertex3 vertex)])
        |
          (vertexIdentifier, vertex) <- zip listVertexIdentifiers listVertices
        , let xyz = floor . (/ realToFrac delta) <$> fromVertex3 vertex
        ]
    )
  )


mergeSpatials :: Eq a => [Spatial a] -> Spatial a
mergeSpatials spatials
  | length (nub $ fst <$> spatials) == 1 = (fst $ head spatials, foldl (H.unionWith (++)) H.empty $ snd <$> spatials)
  | otherwise                            = error "Incongruent spatial indices."


lookupSpatial :: (Hashable a, RealFrac a) => Spatial a -> a -> Point V3 a -> [(Int, Point V3 a)]
lookupSpatial (delta, spatial) d p =
  let
    P (V3 x0 y0 z0) = floor   . (/ realToFrac delta) <$> p .-^ V3 d d d
    P (V3 x1 y1 z1) = ceiling . (/ realToFrac delta) <$> p .+^ V3 d d d
  in
    concat
      [
        H.lookupDefault [] (P (V3 x y z)) spatial
      |
        x <- [x0..x1]
      , y <- [y0..y1]
      , z <- [z0..z1]
      ]


selecter :: Configuration -> ReceivePort SelecterMessage -> SendPort DisplayerMessage -> Process ()
selecter configuration@Configuration{..} control listener =
  do
    frameDebug DebugInfo  "Starting selector."
    let
      Just AdvancedSettings{..} = advanced
      waitForAugment =
        do
          message <- receiveChan control
          frameDebug DebugMessage $ "SE RC 1\t" ++ messageTag message
          case message of
            AugmentSelection{} -> return message
            _                  -> waitForAugment
    AugmentSelection _ links <- waitForAugment
    timeVar <- liftIO $ newMVar 0
    selecterVar <- liftIO $ newMVar zero
    relocationVar <- liftIO $ newMVar (zero, Quaternion 1 zero)
    persistentColoringsRef <- liftIO . newMVar $ U.replicate (1 + maximum (concatMap listVertexIdentifiers links)) $ fromBool False
    transientColoringsRef  <- liftIO . newMVar $ U.replicate (1 + maximum (concatMap listVertexIdentifiers links)) $ fromBool False
    let
      delta = selectorSize presentation * baseSize world / 2
      spatials = fmap mergeSpatials . M.fromListWith (++) $ second (: []) . newSpatial delta <$> links
      average new old = fmap (trackAveraging *) old + fmap ((1 - trackAveraging) *) new
      refresh selecterState' =
        do
          itime <- liftIO $ readMVar timeVar
          f0 <- currentHalfFrame
          selecterPosition' <- liftIO $ readMVar  selecterVar
          (relocation, reorientation) <- liftIO $ readMVar relocationVar
          persistentColorings <- liftIO $ readMVar persistentColoringsRef
          transientColorings <- liftIO $ readMVar transientColoringsRef
          let
            time = M.keys spatials !! itime
            ((persistentColorings', transientColorings'), changes) =
              selecter'
                configuration
                (spatials M.! time)
                (persistentColorings, transientColorings)
                (selecterPosition', selecterState')
                (relocation, reorientation)
          void .liftIO $ swapMVar persistentColoringsRef persistentColorings'
          void .liftIO $ swapMVar transientColoringsRef transientColorings'
          mid2 <- nextMessageIdentifier
          let
            changes' = if null changes then [] else [((LinkType, ""), changes)]
          frameDebug DebugMessage $ "SE SC 2\t" ++ messageTag (Select mid2 (snd time) selecterPosition' changes')
          sendChan listener $!! Select mid2 (snd time) selecterPosition' changes'
          f1 <- currentHalfFrame
          frameDebug DebugTiming $ "SELECT\t" ++ printf "%.3f" (f1 - f0)
    forever
      $ do
        message <- receiveChan control
        case message of
          RelocateSelection{..} -> do
                                     void
                                       . liftIO
                                       . modifyMVar_ relocationVar
                                       . (return .)
                                       $ (const relocationDisplacement *** const relocationRotation) -- FIXME: Should this be averaged, too?
                                     mid3 <- nextMessageIdentifier
                                     frameDebug DebugMessage $ "SE SC 3\t" ++ messageTag (Relocate mid3 relocationDisplacement relocationRotation)
                                     sendChan listener $!! Relocate mid3 relocationDisplacement relocationRotation
                                     refresh Highlight
          UpdateSelection{..}   -> do
                                     void
                                       . liftIO
                                       . modifyMVar_ selecterVar
                                       . (return .)
                                       $ average (selecterPosition .+^ selectorOffset world)
                                     case selecterState of
                                       Forward  -> liftIO . modifyMVar_ timeVar $ \i -> return (minimum [i + 1, M.size spatials - 1])
                                       Backward -> liftIO . modifyMVar_ timeVar $ \i -> return (maximum [i - 1, 0                  ])
                                       _        -> return ()
                                     refresh selecterState
          _                     -> return ()


selecter' :: Configuration
          -> Spatial Double
          -> (U.Vector Bit, U.Vector Bit)
          -> (Point V3 Double, SelectionAction)
          -> (V3 Double, Quaternion Double)
          -> ((U.Vector Bit, U.Vector Bit), [(Int, Coloring)])
selecter' Configuration{..} spatial (persistentColorings, transientColorings) (P location, press) (location', orientation') =
  let
    merge = foldl (flip (:))
    zeroBits = U.replicate (U.length transientColorings) (fromBool False)
    location'' = (P $ conjugate orientation' `rotate` location) .-^ location'
    d = realToFrac $ selectorSize presentation * baseSize world / 2
    inBox p0 p1 = quadrance (p0 .-. p1) <= d^(2::Int)
    g = fst <$> filter (inBox location'' . snd) (lookupSpatial spatial d location'')
    selections = U.accum (const . const $ fromBool True) zeroBits ((, undefined) <$> g)
    persistentColorings' =
      case press of
        Deselection     -> persistentColorings `intersection` invert selections
        Selection       -> persistentColorings `union` selections
        Clear           -> zeroBits
        _               -> persistentColorings
    dp = persistentColorings `symDiff` persistentColorings'
    dt = transientColorings  `symDiff` selections
    dd = dp `union` dt
    newHighlight = listBits $ selections `intersection` dd
    newNohighlight = invert selections `intersection` dd
    newSelect = listBits $ newNohighlight `intersection` persistentColorings'
    newNormal = listBits $ newNohighlight `intersection` invert persistentColorings'
  in
    (
      (persistentColorings', selections)
    ,                fmap (, HighlightColoring) newHighlight
        `merge` fmap (, SelectColoring   ) newSelect
        `merge` fmap (, NormalColoring   ) newNormal
    )

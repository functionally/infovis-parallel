{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}


module InfoVis.Parallel.Process.Select (
  selecter
) where


import Control.Arrow (second)
import Control.Distributed.Process (Process, ReceivePort, SendPort)
import Control.Monad (void)
import Data.Bit (Bit, fromBool)
import Data.Hashable (Hashable)
import Data.List (nub)
import Data.Vector.Unboxed.Bit (intersection, invert, listBits, union, symDiff)
import InfoVis.Parallel.Process.Util (Debugger, debugTime, makeTimer, receiveChan', runProcess, sendChan')
import InfoVis.Parallel.Rendering.Types (DisplayList(..), DisplayType(LinkType))
import InfoVis.Parallel.Types (Coloring(..))
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..), Configuration(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), SelectionAction(..))
import InfoVis.Parallel.Types.Presentation (Presentation(..))
import InfoVis.Parallel.Types.World (World(..))
import Linear.Affine (Point(..), (.-.), (.+^), (.-^))
import Linear.Conjugate (conjugate)
import Linear.Metric (quadrance)
import Linear.Quaternion (Quaternion(..), rotate)
import Linear.V3 (V3(..))
import Linear.Util.Graphics (fromVertex3)
import Linear.Vector (zero)

import qualified Data.HashMap.Strict as H (HashMap, empty, fromListWith, lookupDefault, map, toList, unionWith)
import qualified Data.HashSet as S (fromList, toList)
import qualified Data.Map.Strict as M ((!), fromListWith, keys, size)
import qualified Data.Vector.Unboxed as U (Vector, accum, length, replicate)


type Spatial a = (a, H.HashMap (Point V3 Int) [(Int, Point V3 a)])


nubPair :: (Eq a, Hashable a) => [(a, b)] -> [(a, b)]
nubPair = H.toList . H.fromListWith (const id)


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


selecter :: Debugger -> Configuration -> ReceivePort SelecterMessage -> SendPort DisplayerMessage -> Process ()
selecter frameDebug configuration@Configuration{..} control listener =
  runProcess "selector" 4 frameDebug $ \nextMessageIdentifier ->
    do
      currentHalfFrame <- makeTimer
      let
        receiveSelecter = receiveChan' frameDebug control
        sendListener = sendChan' frameDebug nextMessageIdentifier listener
        Just AdvancedSettings{..} = advanced
        waitForAugment =
          do
            message <- receiveSelecter "SE RC 1"
            case message of
              AugmentSelection{} -> return message
              _                  -> waitForAugment
      AugmentSelection links _ <- waitForAugment
      let
        delta = selectorSize presentation * baseSize world / 2
        spatials = fmap mergeSpatials . M.fromListWith (++) $ second (: []) . newSpatial delta <$> links
        average new old = fmap (trackAveraging *) old + fmap ((1 - trackAveraging) *) new
      let
        loop timeVar selecterVar relocationVar persistentColoringsRef transientColoringsRef =
          do
            f0 <- currentHalfFrame
            message <- receiveSelecter "SE RC 3"
            case message of
              RelocateSelection{..} -> do
                                         let
                                           relocationVar' = (relocationDisplacement, relocationRotation)
                                           time = M.keys spatials !! timeVar
                                           ((persistentColorings', transientColorings'), changes) =
                                             selecter'
                                               configuration
                                               (spatials M.! time)
                                               (persistentColoringsRef, transientColoringsRef)
                                               (selecterVar, Highlight)
                                               relocationVar'
                                           changes' = if null changes then [] else [((LinkType, ""), changes)]
                                         sendListener "SE SC 2" $ Relocate relocationDisplacement relocationRotation
                                         sendListener "SE SC 3" $ Select (snd time) selecterVar changes'
                                         void $ debugTime frameDebug currentHalfFrame ["SELECT"] f0
                                         loop timeVar selecterVar relocationVar' persistentColorings' transientColorings'
              UpdateSelection{..}   -> do
                                         let
                                           selecterVar' = average selecterVar $ selecterPosition .+^ selectorOffset world
                                           timeVar' =
                                             case selecterState of
                                               Forward  -> minimum [timeVar + 1, M.size spatials - 1]
                                               Backward -> maximum [timeVar - 1, 0                  ]
                                               _        -> timeVar
                                           time = M.keys spatials !! timeVar'
                                           ((persistentColorings', transientColorings'), changes) =
                                             selecter'
                                               configuration
                                               (spatials M.! time)
                                               (persistentColoringsRef, transientColoringsRef)
                                               (selecterVar', selecterState)
                                               relocationVar
                                           changes' = if null changes then [] else [((LinkType, ""), changes)]
                                         sendListener "SE SC 4" $ Select (snd time) selecterVar changes'
                                         void $ debugTime frameDebug currentHalfFrame ["SELECT"] f0
                                         loop timeVar' selecterVar' relocationVar persistentColorings' transientColorings'
              _                     -> loop timeVar selecterVar relocationVar persistentColoringsRef transientColoringsRef
      loop 0 zero (zero, Quaternion 1 zero) 
        (U.replicate (1 + maximum (concatMap listVertexIdentifiers links)) $ fromBool False)
        (U.replicate (1 + maximum (concatMap listVertexIdentifiers links)) $ fromBool False)


selecter' :: Configuration
          -> Spatial Double
          -> (U.Vector Bit, U.Vector Bit)
          -> (Point V3 Double, SelectionAction)
          -> (V3 Double, Quaternion Double)
          -> ((U.Vector Bit, U.Vector Bit), [(Int, Coloring)])
selecter' Configuration{..} spatial (persistentColorings, transientColorings) (P location, press) (location', orientation') =
  let
    merge = foldl (flip (:))
    zeroBits = U.replicate (U.length transientColorings) $ fromBool False
    location'' = (P $ conjugate orientation' `rotate` location) .-^ location'
    d = realToFrac $ selectorSize presentation * baseSize world / 2
    inBox p0 p1 = quadrance (p0 .-. p1) <= d^(2::Int)
    g = fst <$> filter (inBox location'' . snd) (lookupSpatial spatial d location'')
    selections = U.accum (const . const $ fromBool True) zeroBits $ (, undefined) <$> g
    persistentColorings' =
      case press of
        Deselection     -> persistentColorings `intersection` invert selections
        Selection       -> persistentColorings `union`               selections
        Clear           -> zeroBits
        _               -> persistentColorings
    dp = persistentColorings `symDiff` persistentColorings'
    dt = transientColorings  `symDiff` selections
    dd = dp                  `union`   dt
    newHighlight   = listBits $ selections        `intersection` dd
    newNohighlight =            invert selections `intersection` dd
    newSelect      = listBits $ newNohighlight    `intersection` persistentColorings'
    newNormal      = listBits $ newNohighlight    `intersection` invert persistentColorings'
  in
    if press == Reset
      then (
             (persistentColorings', selections)
           , nubPair
               $  fmap (, HighlightColoring) (listBits selections         )
               ++ fmap (, SelectColoring   ) (listBits persistentColorings)
               ++ fmap (, NormalColoring   ) (listBits $ invert zeroBits  )
           )
      else (
             (persistentColorings', selections)
           ,           fmap (, HighlightColoring) newHighlight
               `merge` fmap (, SelectColoring   ) newSelect
               `merge` fmap (, NormalColoring   ) newNormal
           )

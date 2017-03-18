{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}


module InfoVis.Parallel.Process.Select (
  selecter
) where


import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.DeepSeq (($!!))
import Control.Distributed.Process (Process, ReceivePort, SendPort, liftIO, receiveChan, sendChan)
import Control.Monad (forever, void, when)
import Data.Bit (Bit, fromBool)
import Data.Hashable (Hashable)
import Data.Vector.Unboxed.Bit (intersection, invert, listBits, union, symDiff)
import InfoVis.Parallel.Process.Util (Debug(..), collectChanMessages, currentHalfFrame, frameDebug)
import InfoVis.Parallel.Rendering.Types (DisplayList(..), DisplayType)
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
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)
import Text.Printf (printf)

import qualified Data.HashMap.Strict as H -- FIXME
import qualified Data.HashSet as S -- FIXME
import qualified Data.Vector.Unboxed as U (Vector, accum, length, replicate)


type Spatial a b = (a, H.HashMap (Point V3 Int) [(Int, Point V3 a)])


consolidate :: (Eq a, Hashable a) => [a] -> [a]
consolidate = S.toList . S.fromList


newSpatial :: (Hashable a, RealFloat a) => a -> [DisplayList b Int]  -> Spatial a b
newSpatial delta dis =
  (
    delta
  , H.map consolidate
      $ H.fromListWith (++)
      [
        (xyz, [(vertexIdentifier, realToFrac <$> fromVertex3 vertex)])
      |
        DisplayList{..} <- dis
      , (vertexIdentifier, vertex) <- zip listVertexIdentifiers listVertices
      , let xyz = floor . (/ realToFrac delta) <$> fromVertex3 vertex
      ]
  )


lookupSpatial :: (Hashable a, RealFrac a) => Spatial a b -> a -> Point V3 a -> [(Int, Point V3 a)]
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
    AugmentSelection mid1 gridsLinks@(_, _, links) <- receiveChan control
    let
      delta = selectorSize presentation * baseSize world / 2
      spatial = newSpatial delta links
    frameDebug DebugMessage $ "SE RC 1\t" ++ messageTag (AugmentSelection mid1 gridsLinks)
    selecterVar <- liftIO $ newMVar zero
    relocationVar <- liftIO $ newMVar (zero, Quaternion 1 zero)
    persistentColoringsRef <- liftIO . newMVar $ U.replicate (1 + maximum (concatMap listVertexIdentifiers links)) $ fromBool False
    transientColoringsRef  <- liftIO . newMVar $ U.replicate (1 + maximum (concatMap listVertexIdentifiers links)) $ fromBool False
    forever
      $ do
        let
          collector   RelocateSelection{} _ = True
          collector x@UpdateSelection{}   y = selecterState x == selecterState y
          collector _                    _ = False
          refresh selecterState' =
            do
              t0 <- liftIO $ toNanoSecs <$> getTime Monotonic
              f0 <- currentHalfFrame
              selecterPosition' <- liftIO $ readMVar  selecterVar
              (relocation, reorientation) <- liftIO $ readMVar relocationVar
              persistentColorings <- liftIO $ readMVar persistentColoringsRef
              transientColorings <- liftIO $ readMVar transientColoringsRef
              let
                ((persistentColorings', transientColorings'), changes) =
                  selecter'
                    configuration
                    spatial
                    (persistentColorings, transientColorings)
                    (selecterPosition', selecterState')
                    (relocation, reorientation)
              void .liftIO $ swapMVar persistentColoringsRef persistentColorings'
              void .liftIO $ swapMVar transientColoringsRef transientColorings'
              mid2 <- nextMessageIdentifier
              frameDebug DebugMessage $ "SE SC 2\t" ++ messageTag (Select mid2 selecterPosition' changes)
              sendChan listener $!! Select mid2 selecterPosition' changes
              f1 <- currentHalfFrame
              frameDebug DebugTiming $ "SELECT\t" ++ printf "%.3f" (f1 - f0)
              t1 <- liftIO $ toNanoSecs <$> getTime Monotonic
              let
                delay = maximum [1000000 `div` 60 - fromIntegral (t1 - t0) `div` 1000, 0]
              when (delaySelection && delay > 0)
                . liftIO $ threadDelay delay -- FIXME: We don't want to update the selection more than once per half frame.
        messages <- collectChanMessages maximumTrackingCompression control collector
        sequence_
          [
            case message of
              RelocateSelection{..} -> do
                                         void . liftIO $ swapMVar relocationVar (relocationDisplacement, relocationRotation)
                                         mid3 <- nextMessageIdentifier
                                         frameDebug DebugMessage $ "SE SC 3\t" ++ messageTag (Relocate mid3 relocationDisplacement relocationRotation)
                                         sendChan listener $!! Relocate mid3 relocationDisplacement relocationRotation
                                         refresh Highlight
              UpdateSelection{..}   -> do
                                         void . liftIO . swapMVar selecterVar $ selecterPosition .+^ selectorOffset world
                                         refresh selecterState
              _                     -> return ()
          |
            message <- messages
          ]


selecter' :: Configuration
          -> Spatial Double (DisplayType, String)
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
        Highlight       -> persistentColorings
        Deselection     -> persistentColorings `intersection` invert selections
        Selection       -> persistentColorings `union` selections
        Clear           -> zeroBits
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

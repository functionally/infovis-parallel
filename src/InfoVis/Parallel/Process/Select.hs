{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Process.Select (
  selecter
) where


import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.DeepSeq (($!!))
import Control.Distributed.Process (Process, ReceivePort, SendPort, liftIO, receiveChan, sendChan)
import Control.Monad (forever, void, when)
import Data.Bit (Bit, fromBool)
import Data.Vector.Unboxed.Bit (intersection, invert, listBits, union, symDiff)
import InfoVis.Parallel.Process.DataProvider (GridsLinks)
import InfoVis.Parallel.Process.Util (Debug(..), collectChanMessages, frameDebug)
import InfoVis.Parallel.Rendering.Types (DisplayList(..), inBox)
import InfoVis.Parallel.Types (Coloring(..))
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..), Configuration(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), SelectionAction(..), messageTag)
import InfoVis.Parallel.Types.Presentation (Presentation(..))
import InfoVis.Parallel.Types.World (World(..))
import Linear.Affine (Point(..), (.+^), (.-^))
import Linear.Conjugate (conjugate)
import Linear.Quaternion (Quaternion(..), rotate)
import Linear.V3 (V3(..))
import Linear.Util.Graphics (toVertex3)
import Linear.Vector (zero)
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)

import qualified Data.Vector.Unboxed as U (Vector, accum, length, replicate)


selecter :: Configuration -> ReceivePort SelecterMessage -> SendPort DisplayerMessage -> Process ()
selecter configuration control listener =
  do
    frameDebug DebugInfo  "Starting selector."
    let
      Just AdvancedSettings{..} = advanced configuration
    AugmentSelection gridsLinks@(_, _, links) <- receiveChan control
    frameDebug DebugMessage $ "SE RC 1\t" ++ messageTag (AugmentSelection gridsLinks)
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
              selecterPosition' <- liftIO $ readMVar  selecterVar
              (relocation, reorientation) <- liftIO $ readMVar relocationVar
              persistentColorings <- liftIO $ readMVar persistentColoringsRef
              transientColorings <- liftIO $ readMVar transientColoringsRef
              let
                ((persistentColorings', transientColorings'), changes) =
                  selecter'
                    configuration
                    gridsLinks
                    (persistentColorings, transientColorings)
                    (selecterPosition', selecterState')
                    (relocation, reorientation)
              void .liftIO $ swapMVar persistentColoringsRef persistentColorings'
              void .liftIO $ swapMVar transientColoringsRef transientColorings'
              frameDebug DebugMessage $ "SE SC 2\t" ++ messageTag (Select selecterPosition' changes)
              sendChan listener $!! Select selecterPosition' changes
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
                                         frameDebug DebugMessage $ "SE SC 2\t" ++ messageTag (Relocate relocationDisplacement relocationRotation)
                                         sendChan listener $!! Relocate relocationDisplacement relocationRotation
                                         refresh Highlight
              UpdateSelection{..}   -> do
                                         void . liftIO . swapMVar selecterVar $ selecterPosition .+^ selectorOffset (world configuration)
                                         refresh selecterState
              _                     -> return ()
          |
            message <- messages
          ]


selecter' :: Configuration
          -> GridsLinks
          -> (U.Vector Bit, U.Vector Bit)
          -> (Point V3 Double, SelectionAction)
          -> (V3 Double, Quaternion Double)
          -> ((U.Vector Bit, U.Vector Bit), [(Int, Coloring)])
selecter' Configuration{..} (_, _, links) (persistentColorings, transientColorings) (P location, press) (location', orientation') =
  let
    merge = foldl (flip (:))
    skipDuplicates (e : es@(e': _)) = if e == e' then skipDuplicates es else e : skipDuplicates es
    skipDuplicates es = es
    zeroBits = U.replicate (U.length transientColorings) (fromBool False)
    location'' = toVertex3 $ realToFrac <$> (P $ conjugate orientation' `rotate` location) .-^ location'
    d = realToFrac $ selectorSize presentation * baseSize world / 2
    g DisplayList{..} = skipDuplicates $ fst <$> filter (inBox d location'' . snd) (zip listVertexIdentifiers listVertices)
    selections = U.accum (const . const $ fromBool True) zeroBits ((, undefined) <$> concatMap g links)
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

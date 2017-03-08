{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Process.Select (
  selecter
) where


import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.Distributed.Process (Process, ReceivePort, SendPort, getSelfPid, liftIO, receiveChan, say, sendChan)
import Control.Monad (forever, void, when)
import Data.Bit (Bit, fromBool)
import Data.Vector.Unboxed.Bit (intersection, invert, listBits, union, symDiff)
import Graphics.Rendering.OpenGL (Vertex3(..))
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import InfoVis.Parallel.Process.DataProvider (GridsLinks)
import InfoVis.Parallel.Process.Util (collectChanMessages)
import InfoVis.Parallel.Types (Coloring(..))
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Display (DisplayList(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), SelectionAction(..))
import InfoVis.Parallel.Types.Scaffold (Presentation(..), World(..))
import Linear.Affine (Point(..), (.-.), (.+^))
import Linear.Conjugate (conjugate)
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Linear.Vector (zero)
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)

import qualified Data.Vector.Unboxed as U (Vector, accum, length, replicate)
import qualified Linear.Quaternion as Q (rotate)


selecter :: ReceivePort SelecterMessage -> SendPort DisplayerMessage -> Process ()
selecter control listener =
  do
    pid <- getSelfPid
    say $ "Starting selector <" ++ show pid ++ ">."
    ResetSelecter configuration <- receiveChan control
    AugmentSelecter gridsLinks@(_, links) <- receiveChan control
    selecterVar <- liftIO $ newMVar zero
    relocationVar <- liftIO $ newMVar (zero, Quaternion 1 zero)
    persistentColoringsRef <- liftIO . newMVar $ U.replicate (1 + maximum (concatMap listVertexIdentifiers links)) $ fromBool False
    transientColoringsRef  <- liftIO . newMVar $ U.replicate (1 + maximum (concatMap listVertexIdentifiers links)) $ fromBool False
    forever
      $ do
        let
          collector   RelocateSelecter{} _ = True
          collector x@UpdateSelecter{}   y = selecterState x == selecterState y
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
                    (P relocation, reorientation)
              void .liftIO $ swapMVar persistentColoringsRef persistentColorings'
              void .liftIO $ swapMVar transientColoringsRef transientColorings'
              listener `sendChan` Select selecterPosition' changes
              t1 <- liftIO $ toNanoSecs <$> getTime Monotonic
              let
                delay = maximum [1000000 `div` 60 - fromIntegral (t1 - t0) `div` 1000, 0]
              when (delay > 0)
                . liftIO $ threadDelay delay -- FIXME: We don't want to update the selection more than once per half frame.
        messages <- collectChanMessages control collector
        sequence_
          [
            case message of
              RelocateSelecter{..} -> do
                                        void . liftIO $ swapMVar relocationVar (relocationDisplacement, relocationRotation)
                                        listener `sendChan` Relocate relocationDisplacement relocationRotation
                                        refresh Highlight
              UpdateSelecter{..}   -> do
                                        void . liftIO . swapMVar selecterVar $ selecterPosition .+^ selectorOffset (world configuration)
                                        refresh selecterState
              _                    -> return ()
          |
            message <- messages
          ]


selecter' :: Configuration Double
          -> GridsLinks
          -> (U.Vector Bit, U.Vector Bit)
          -> (Point V3 Double, SelectionAction)
          -> (Point V3 Double, Quaternion Double)
          -> ((U.Vector Bit, U.Vector Bit), [(Int, Coloring)])
selecter' Configuration{..} (_, links) (persistentColorings, transientColorings) (P location, press) (P location', orientation') =
  let
    merge = foldl (flip (:))
    skipDuplicates (e : es@(e': _)) = if e == e' then skipDuplicates es else e : skipDuplicates es
    skipDuplicates es = es
    zeroBits = U.replicate (U.length transientColorings) (fromBool False)
    V3 x y z = realToFrac <$> (P $ conjugate orientation' `Q.rotate` location) .-. P location'
    d = realToFrac $ selectorSize presentation * baseSize world / 2
    h w w' = abs (w - w' + d) <= d
    f (Vertex3 x' y' z') = h x x' && h y y' && h z z'
    g DisplayList{..} = skipDuplicates $ fst <$> filter (f . snd) (zip listVertexIdentifiers listVertices)
    selections = U.accum (const . const $ fromBool True) zeroBits $ fmap (, undefined) $ concatMap g links
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

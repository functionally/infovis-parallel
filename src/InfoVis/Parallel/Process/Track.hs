{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Process.Track (
  trackPov
, trackRelocation
, trackSelection
) where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, readMVar, swapMVar, takeMVar)
import Control.Distributed.Process (Process, ProcessId, expect, getSelfPid, liftIO, say, send, spawnLocal)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad (forever, void, when)
import Graphics.Rendering.Handa.Util (degree)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import InfoVis.Parallel.Types.Configuration (Input(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), SelectionAction(..), TrackerMessage(..))
import Linear.Affine (Point(..))
import Linear.Epsilon (Epsilon)
import Linear.Quaternion (Quaternion(..), axisAngle)
import Linear.V3 (V3(..))
import Linear.Vector (basis, zero)
import Network.UI.Kafka (Sensor, TopicConnection, consumerLoop)
import Network.UI.Kafka.Types (Button(IndexButton), Event(..), Toggle(..))


fromDegrees :: (Floating a, Num a) => a -> a
fromDegrees = (/ degree)


fromEuler :: (Epsilon a, Num a, RealFloat a) => V3 a -> Quaternion a
fromEuler (V3 phi theta psi) =
  let
    [ex, ey, ez] = basis
  in
    ez `axisAngle` psi * ey `axisAngle` theta * ex `axisAngle` phi


consumerLoopProcess :: TopicConnection -> (Sensor -> Event -> Process ()) -> Process ()
consumerLoopProcess topicConnection processor = -- FIXME: Catch exceptions and support termination.
  do
    forward <- liftIO newEmptyMVar
    (_, consuming) <- liftIO . consumerLoop topicConnection $ (putMVar forward .) . (,)
    void . spawnLocal . forever $ liftIO (takeMVar forward) >>= uncurry processor
    void . liftIO . forkIO $ void consuming

    
trackVectorQuaternion :: Serializable a => (V3 Double -> Quaternion Double -> a) -> [ProcessId] -> TopicConnection -> Sensor -> Process ()
trackVectorQuaternion messager listeners topicConnection target = -- FIXME: Support reset, termination, and faults.
  do
    locationVar <- liftIO $ newMVar zero
    orientationVar <- liftIO . newMVar $ Quaternion 1 zero
    let
      processInput sensor (LocationEvent (x, y, z)) =
        when (sensor == target)
          $ do
            let
              location = V3 x y z
            void . liftIO $ swapMVar locationVar location
            orientation <- liftIO $ readMVar orientationVar
            mapM_ (`send` messager location orientation) listeners
      processInput sensor (OrientationEvent (w, x, y, z)) =
        when (sensor == target)
          $ do
            location <- liftIO $ readMVar locationVar
            let
              orientation = Quaternion w $ V3 x y z
            void . liftIO $ swapMVar orientationVar orientation
            mapM_ (`send` messager location orientation) listeners
      processInput _ _ =
        return ()
    consumerLoopProcess topicConnection processInput


trackPov :: [ProcessId] -> Process ()
trackPov listeners = -- FIXME: Support reset, termination, and faults.
  do
    pid <- getSelfPid
    say $ "Starting point-of-view tracker <" ++ show pid ++ ">."
    ResetTracker Input{..} <- expect
    let
      trackStatic (location, orientation) =
        do
          let
            location' = P location
            orientation' = fromEuler $ fromDegrees <$> orientation
          mapM_ (`send` Track location' orientation') listeners
      trackDynamic = trackVectorQuaternion (Track . P) listeners kafka
    either trackStatic trackDynamic povInput


trackRelocation :: ProcessId -> Process ()
trackRelocation selecterPid = -- FIXME: Support reset, termination, and faults.
  do
    pid <- getSelfPid
    say $ "Starting relocation tracker <" ++ show pid ++ ">."
    ResetTracker Input{..} <- expect
    trackVectorQuaternion RelocateSelecter [selecterPid] kafka relocationInput


trackSelection :: [ProcessId] -> Process ()
trackSelection listeners =
  do
    pid <- getSelfPid
    say $ "Starting selection tracker <" ++ show pid ++ ">."
    ResetTracker Input{..} <- expect
    locationVar <- liftIO $ newMVar zero
    let
      processInput' sensor (x, y, z) =
        when (sensor == selectorInput)
          $ do
            let
              location = P $ V3 x y z
            void . liftIO $ swapMVar locationVar location
            mapM_ (`send` UpdateSelecter location Highlight) listeners
      processInput sensor (LocationEvent xyz) = processInput' sensor xyz
      processInput sensor (PointerEvent xyz) = processInput' sensor xyz
      processInput sensor (ButtonEvent (IndexButton i, Down)) =
        case (sensor, i) `lookup` [(selectButton, Selection), (deselectButton, Deselection), (clearButton, Clear)] of
          Nothing              -> return ()
          Just selectionAction -> do
                                    location <- liftIO $ readMVar locationVar
                                    mapM_ (`send` UpdateSelecter location selectionAction) listeners
      processInput _ _ =
        return ()
    consumerLoopProcess kafka processInput

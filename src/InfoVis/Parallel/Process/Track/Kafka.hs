{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Process.Track.Kafka (
  trackPov
, trackRelocation
, trackSelection
) where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, readMVar, swapMVar, takeMVar)
import Control.DeepSeq (NFData, ($!!))
import Control.Distributed.Process (Process, SendPort, getSelfPid, liftIO, say, sendChan, spawnLocal)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad (forever, void, when)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Input (Input(InputKafka))
import InfoVis.Parallel.Types.Input.Kafka (InputKafka(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), SelectionAction(..))
import Linear.Affine (Point(..))
import Linear.Quaternion (Quaternion(..))
import Linear.Util (fromEulerd)
import Linear.V3 (V3(..))
import Linear.Vector (zero)
import Network.UI.Kafka (Sensor, TopicConnection, consumerLoop)
import Network.UI.Kafka.Types (Button(IndexButton), Event(..), Toggle(..))


consumerLoopProcess :: TopicConnection -> (Sensor -> Event -> Process ()) -> Process ()
consumerLoopProcess topicConnection processor = -- FIXME: Catch exceptions and support termination.
  do
    forward <- liftIO newEmptyMVar
    (_, consuming) <- liftIO . consumerLoop topicConnection $ (putMVar forward .) . (,)
    void . spawnLocal . forever $ liftIO (takeMVar forward) >>= uncurry processor
    void . liftIO . forkIO $ void consuming

    
trackVectorQuaternion :: (NFData a, Serializable a) => (V3 Double -> Quaternion Double -> a) -> SendPort a -> TopicConnection -> Sensor -> Process ()
trackVectorQuaternion messager listener topicConnection target = -- FIXME: Support reset, termination, and faults.
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
            sendChan listener $!! messager location orientation
      processInput sensor (OrientationEvent (w, x, y, z)) =
        when (sensor == target)
          $ do
            location <- liftIO $ readMVar locationVar
            let
              orientation = Quaternion w $ V3 x y z
            void . liftIO $ swapMVar orientationVar orientation
            sendChan listener $!! messager location orientation
      processInput _ _ =
        return ()
    consumerLoopProcess topicConnection processInput


trackPov :: Configuration -> SendPort DisplayerMessage -> Process ()
trackPov Configuration{..} listener = -- FIXME: Support reset, termination, and faults.
  do
    pid <- getSelfPid
    say $ "Starting point-of-view tracker <" ++ show pid ++ ">."
    let
      InputKafka Input{..} = input
      trackStatic (location, orientation) =
        sendChan listener $!! Track (P location) (fromEulerd orientation)
      trackDynamic = trackVectorQuaternion (Track . P) listener kafka
    either trackStatic trackDynamic povInput


trackRelocation :: Configuration -> SendPort SelecterMessage -> Process ()
trackRelocation Configuration{..} listener = -- FIXME: Support reset, termination, and faults.
  do
    pid <- getSelfPid
    say $ "Starting relocation tracker <" ++ show pid ++ ">."
    let
      InputKafka Input{..} = input
    trackVectorQuaternion RelocateSelection listener kafka relocationInput


trackSelection :: Configuration -> SendPort SelecterMessage -> Process ()
trackSelection Configuration{..} listener =
  do
    pid <- getSelfPid
    say $ "Starting selection tracker <" ++ show pid ++ ">."
    locationVar <- liftIO $ newMVar zero
    let
      InputKafka Input{..} = input
      processInput' sensor (x, y, z) =
        when (sensor == selectorInput)
          $ do
            let
              location = P $ V3 x y z
            void . liftIO $ swapMVar locationVar location
            sendChan listener $!! UpdateSelection location Highlight
      processInput sensor (LocationEvent xyz) = processInput' sensor xyz
      processInput sensor (PointerEvent xyz) = processInput' sensor xyz
      processInput sensor (ButtonEvent (IndexButton i, Down)) =
        case (sensor, i) `lookup` [(selectButton, Selection), (deselectButton, Deselection), (clearButton, Clear)] of
          Nothing              -> return ()
          Just selectionAction -> do
                                    location <- liftIO $ readMVar locationVar
                                    sendChan listener $!! UpdateSelection location selectionAction
      processInput _ _ =
        return ()
    consumerLoopProcess kafka processInput

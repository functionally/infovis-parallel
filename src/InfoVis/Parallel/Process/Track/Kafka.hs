{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Process.Track.Kafka (
  trackPov
, trackRelocation
, trackSelection
) where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, readMVar, swapMVar, takeMVar)
import Control.Distributed.Process (Process, SendPort, expect, getSelfPid, liftIO, say, sendChan, spawnLocal)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad (forever, void, when)
import InfoVis.Parallel.Types.Input (Input(InputKafka))
import InfoVis.Parallel.Types.Input.Kafka (InputKafka(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), SelectionAction(..), TrackerMessage(..))
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

    
trackVectorQuaternion :: Serializable a => (V3 Double -> Quaternion Double -> a) -> SendPort a -> TopicConnection -> Sensor -> Process ()
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
            listener `sendChan` messager location orientation
      processInput sensor (OrientationEvent (w, x, y, z)) =
        when (sensor == target)
          $ do
            location <- liftIO $ readMVar locationVar
            let
              orientation = Quaternion w $ V3 x y z
            void . liftIO $ swapMVar orientationVar orientation
            listener `sendChan` messager location orientation
      processInput _ _ =
        return ()
    consumerLoopProcess topicConnection processInput


trackPov :: SendPort DisplayerMessage -> Process ()
trackPov listener = -- FIXME: Support reset, termination, and faults.
  do
    pid <- getSelfPid
    say $ "Starting point-of-view tracker <" ++ show pid ++ ">."
    ResetTracker (InputKafka Input{..}) <- expect
    let
      trackStatic (location, orientation) =
        listener `sendChan` Track (P location) (fromEulerd orientation)
      trackDynamic = trackVectorQuaternion (Track . P) listener kafka
    either trackStatic trackDynamic povInput


trackRelocation :: SendPort SelecterMessage -> Process ()
trackRelocation listener = -- FIXME: Support reset, termination, and faults.
  do
    pid <- getSelfPid
    say $ "Starting relocation tracker <" ++ show pid ++ ">."
    ResetTracker (InputKafka Input{..}) <- expect
    trackVectorQuaternion RelocateSelecter listener kafka relocationInput


trackSelection :: SendPort SelecterMessage -> Process ()
trackSelection listener =
  do
    pid <- getSelfPid
    say $ "Starting selection tracker <" ++ show pid ++ ">."
    ResetTracker (InputKafka Input{..}) <- expect
    locationVar <- liftIO $ newMVar zero
    let
      processInput' sensor (x, y, z) =
        when (sensor == selectorInput)
          $ do
            let
              location = P $ V3 x y z
            void . liftIO $ swapMVar locationVar location
            listener `sendChan` UpdateSelecter location Highlight
      processInput sensor (LocationEvent xyz) = processInput' sensor xyz
      processInput sensor (PointerEvent xyz) = processInput' sensor xyz
      processInput sensor (ButtonEvent (IndexButton i, Down)) =
        case (sensor, i) `lookup` [(selectButton, Selection), (deselectButton, Deselection), (clearButton, Clear)] of
          Nothing              -> return ()
          Just selectionAction -> do
                                    location <- liftIO $ readMVar locationVar
                                    listener `sendChan` UpdateSelecter location selectionAction
      processInput _ _ =
        return ()
    consumerLoopProcess kafka processInput

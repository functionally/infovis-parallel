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
import Control.Distributed.Process (Process, SendPort, liftIO, sendChan, spawnLocal)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad (forever, void, when)
import InfoVis.Parallel.Process.Util (Debug(..), Debugger, runProcess)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Input (Input(InputKafka))
import InfoVis.Parallel.Types.Input.Kafka (InputKafka(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), MessageTag(..), SelecterMessage(..), SelectionAction(..))
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

    
trackVectorQuaternion :: (MessageTag a, NFData a, Serializable a) => Debugger -> Process Int -> (Int -> V3 Double -> Quaternion Double -> a) -> SendPort a -> TopicConnection -> Sensor -> Process ()
trackVectorQuaternion frameDebug nextMessageIdentifier messager listener topicConnection target = -- FIXME: Support reset, termination, and faults.
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
            mid1 <- nextMessageIdentifier
            frameDebug DebugMessage $ "TV SC 1\t" ++ messageTag (messager mid1 location orientation)
            sendChan listener $!! messager mid1 location orientation
      processInput sensor (OrientationEvent (w, x, y, z)) =
        when (sensor == target)
          $ do
            location <- liftIO $ readMVar locationVar
            let
              orientation = Quaternion w $ V3 x y z
            void . liftIO $ swapMVar orientationVar orientation
            mid2 <- nextMessageIdentifier
            frameDebug DebugMessage $ "TV SC 2\t" ++ messageTag (messager mid2 location orientation)
            sendChan listener $!! messager mid2 location orientation
      processInput _ _ =
        return ()
    consumerLoopProcess topicConnection processInput


trackPov :: Debugger -> Configuration -> SendPort DisplayerMessage -> Process ()
trackPov frameDebug Configuration{..} listener = -- FIXME: Support reset, termination, and faults.
  runProcess "point-of-view tracker" 5 frameDebug $ \nextMessageIdentifier ->
    do
      let
        InputKafka Input{..} = input
        trackStatic (location, orientation) =
          do
            mid1 <- nextMessageIdentifier
            frameDebug DebugMessage $ "TP SC 1\t" ++ messageTag (Track mid1 (P location) (fromEulerd orientation))
            sendChan listener $!! Track mid1 (P location) (fromEulerd orientation)
        trackDynamic = trackVectorQuaternion frameDebug nextMessageIdentifier ((. P) . Track) listener kafka
      either trackStatic trackDynamic povInput


trackRelocation :: Debugger -> Configuration -> SendPort SelecterMessage -> Process ()
trackRelocation frameDebug Configuration{..} listener = -- FIXME: Support reset, termination, and faults.
  runProcess "relocation tracker" 6 frameDebug $ \nextMessageIdentifier ->
    do
      let
        InputKafka Input{..} = input
      trackVectorQuaternion frameDebug nextMessageIdentifier RelocateSelection listener kafka relocationInput


trackSelection :: Debugger -> Configuration -> SendPort SelecterMessage -> Process ()
trackSelection frameDebug Configuration{..} listener =
  runProcess "selection tracker" 7 frameDebug $ \nextMessageIdentifier ->
    do
      locationVar <- liftIO $ newMVar zero
      let
        InputKafka Input{..} = input
        processInput' sensor (x, y, z) =
          when (sensor == selectorInput)
            $ do
              let
                location = P $ V3 x y z
              void . liftIO $ swapMVar locationVar location
              mid1 <- nextMessageIdentifier
              frameDebug DebugMessage $ "TS SC 1\t" ++ messageTag (UpdateSelection mid1 location Highlight)
              sendChan listener $!! UpdateSelection mid1 location Highlight
        processInput sensor (LocationEvent xyz) = processInput' sensor xyz
        processInput sensor (PointerEvent xyz) = processInput' sensor xyz
        processInput sensor (ButtonEvent (IndexButton i, Down)) =
          case (sensor, i) `lookup` [(selectButton, Selection), (deselectButton, Deselection), (clearButton, Clear), (forwardButton, Forward), (backwardButton, Backward), (resetButton, Reset)] of
            Nothing              -> return ()
            Just selectionAction -> do
                                      location <- liftIO $ readMVar locationVar
                                      mid2 <- nextMessageIdentifier
                                      frameDebug DebugMessage $ "TS SC 2\t" ++ messageTag (UpdateSelection mid2 location selectionAction)
                                      sendChan listener $!! UpdateSelection mid2 location selectionAction
        processInput _ _ =
          return ()
      consumerLoopProcess kafka processInput

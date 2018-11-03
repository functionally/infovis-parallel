{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.Track.VRPN (
  trackPov
, trackRelocation
, trackSelection
) where


import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar -- FIXME
import Control.Concurrent.STM.TVar -- FIXME
import Control.Distributed.Process (Process, SendPort, liftIO)
import Control.Monad (liftM2, liftM3, void, when)
import InfoVis.Parallel.Process.Util (Debugger, runProcess, sendChan', waitForTermination)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Input (Input(InputVRPN))
import InfoVis.Parallel.Types.Input.VRPN (InputVRPN(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), SelectionAction(..))
import Linear.Affine (Point(..))
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Linear.Vector (zero)
import Network.VRPN (ButtonCallback, PositionCallback, Device(Button, Tracker), mainLoop, openDevice)


trackPov :: SendPort DisplayerMessage -> Debugger -> Configuration -> Process ()
trackPov listener frameDebug Configuration{..} = -- FIXME: Support reset, termination, and faults.
  runProcess "point-of-view tracker" 5 frameDebug $ \nextMessageIdentifier ->
    do
      (locationVar, orientationVar, updatedVar) <- liftIO . atomically $ liftM3 (,,) (newTVar zero) (newTVar zero) newEmptyTMVar
      let
        InputVRPN Input{..} = input
        callback :: PositionCallback Int Double
        callback _ sensor (lx, ly, lz) (ox, oy, oz, ow) =
          when (sensor == headSensor)
            . liftIO
            . atomically
            $ do
              writeTVar locationVar . P $ V3 lx ly lz
              writeTVar orientationVar $ Quaternion ow $ V3 ox oy oz
              void $ tryPutTMVar updatedVar ()
        device :: Device Int Int Int Double
        device = Tracker headDevice (Just callback) Nothing Nothing
      remote <- liftIO $ openDevice device
      let
        loop =
          do
            liftIO $ mainLoop remote
            updated <- liftIO . atomically $ tryTakeTMVar updatedVar
            when (updated == Just ())
              $ do
                (location, orientation) <- liftIO . atomically $ liftM2 (,) (readTVar locationVar) (readTVar orientationVar)
                sendChan' frameDebug nextMessageIdentifier listener "TP SC 1" $ Track location orientation
            loop
      loop


trackRelocation :: SendPort SelecterMessage -> Debugger -> Configuration -> Process ()
trackRelocation _ frameDebug _ = -- FIXME: Support reset, termination, and faults.
  runProcess "relocation tracker" 6 frameDebug . const $ waitForTermination frameDebug


trackSelection :: SendPort SelecterMessage -> Debugger -> Configuration -> Process ()
trackSelection listener frameDebug Configuration{..} = -- FIXME: Support reset, termination, and faults.
  runProcess "selection tracker" 7 frameDebug $ \nextMessageIdentifier ->
    do
      (locationVar, buttonVar, updatedVar) <- liftIO . atomically $ liftM3 (,,) (newTVar zero) (newTVar Highlight) newEmptyTMVar
      let
        InputVRPN Input{..} = input
        updateButton button =
          liftIO
            . atomically 
            $ do
              writeTVar buttonVar button
              void $ tryPutTMVar updatedVar ()
        callback :: PositionCallback Int Double
        callback _ sensor (lx, ly, lz) _ =
          when (sensor == selectSensor)
            . liftIO
            . atomically
            $ do
              writeTVar locationVar . P $ V3 lx ly lz
              void $ tryPutTMVar updatedVar ()
        callback' :: ButtonCallback Int
        callback' _ button pressed
          | (button, pressed) == (selectButton  , True ) = updateButton Selection  
          | (button, pressed) == (deselectButton, True ) = updateButton Deselection
          | (button, pressed) == (clearButton   , True ) = updateButton Clear      
          | (button, pressed) == (forwardButton , True ) = updateButton Forward    
          | (button, pressed) == (backwardButton, True ) = updateButton Backward   
          | (button, pressed) == (resetButton   , True ) = updateButton Reset      
          | otherwise                                    = updateButton Highlight  
        device :: Device Int Int Int Double
        device = Tracker selectDevice (Just callback) Nothing Nothing
        device' :: Device Int Int Int Double
        device' = Button buttonDevice (Just callback')
      remotes <- liftIO $ mapM openDevice [device, device']
      let
        loop =
          do
            liftIO $ mapM_ mainLoop remotes
            updated <- liftIO . atomically $ tryTakeTMVar updatedVar
            when (updated == Just ())
              $ do
                (location, button) <- liftIO . atomically $ liftM2 (,) (readTVar locationVar) (readTVar buttonVar)
                sendChan' frameDebug nextMessageIdentifier listener "TS SC 1" $ UpdateSelection location button
            loop
      loop

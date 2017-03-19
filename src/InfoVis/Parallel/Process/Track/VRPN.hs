{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.Track.VRPN (
  trackPov
, trackRelocation
, trackSelection
) where


import Control.Concurrent.MVar (newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.DeepSeq (($!!))
import Control.Distributed.Process (Process, SendPort, liftIO, sendChan)
import Control.Monad (void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import InfoVis.Parallel.Process.Util (Debug(..), frameDebug)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Input (Input(InputVRPN))
import InfoVis.Parallel.Types.Input.VRPN (InputVRPN(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), MessageTag(..), SelecterMessage(..), SelectionAction(..), nextMessageIdentifier)
import Linear.Affine (Point(..))
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Linear.Vector (zero)
import Network.VRPN (ButtonCallback, PositionCallback, Device(Button, Tracker), mainLoop, openDevice)


trackPov :: Configuration -> SendPort DisplayerMessage -> Process ()
trackPov Configuration{..} listener = -- FIXME: Support reset, termination, and faults.
  do
    frameDebug DebugInfo "Starting point-of-view tracker."
    locationVar <- liftIO $ newIORef zero
    orientationVar <- liftIO $ newIORef zero
    updatedVar <- liftIO newEmptyMVar
    let
      InputVRPN Input{..} = input
      callback :: PositionCallback Int Double
      callback _ sensor (lx, ly, lz) (ox, oy, oz, ow) =
        when (sensor == headSensor)
          $ do
            writeIORef locationVar . P $ V3 lx ly lz
            writeIORef orientationVar $ Quaternion ow $ V3 ox oy oz
            void $ tryPutMVar updatedVar ()
      device :: Device Int Int Int Double
      device = Tracker headDevice (Just callback) Nothing Nothing
    remote <- liftIO $ openDevice device
    let
      loop =
        do
          liftIO $ mainLoop remote
          updated <- liftIO $ tryTakeMVar updatedVar
          when (updated == Just ())
            $ do
              location <- liftIO $ readIORef locationVar
              orientation <- liftIO $ readIORef orientationVar
              mid1 <- nextMessageIdentifier
              frameDebug DebugMessage $ "TP SC 1\t" ++ messageTag (Track mid1 location orientation)
              sendChan listener $!! Track mid1 location orientation
          loop
    loop


trackRelocation :: Configuration -> SendPort SelecterMessage -> Process ()
trackRelocation _ _ = -- FIXME: Support reset, termination, and faults.
  frameDebug DebugInfo "Starting relocation tracker."


trackSelection :: Configuration -> SendPort SelecterMessage -> Process ()
trackSelection Configuration{..} listener = -- FIXME: Support reset, termination, and faults.
  do
    frameDebug DebugInfo "Starting selection tracker."
    locationVar <- liftIO $ newIORef zero
    buttonVar <- liftIO $ newIORef Highlight
    updatedVar <- liftIO newEmptyMVar
    let
      InputVRPN Input{..} = input
      callback :: PositionCallback Int Double
      callback _ sensor (lx, ly, lz) _ =
        when (sensor == selectSensor)
          $ do
            writeIORef locationVar . P $ V3 lx ly lz
            void $ tryPutMVar updatedVar ()
      callback' :: ButtonCallback Int
      callback' _ button pressed
        | (button, pressed) == (selectButton  , True ) = void $ writeIORef buttonVar Selection   >> tryPutMVar updatedVar ()
        | (button, pressed) == (deselectButton, True ) = void $ writeIORef buttonVar Deselection >> tryPutMVar updatedVar ()
        | (button, pressed) == (clearButton   , True ) = void $ writeIORef buttonVar Clear       >> tryPutMVar updatedVar ()
        | (button, pressed) == (forwardButton , True ) = void $ writeIORef buttonVar Forward     >> tryPutMVar updatedVar ()
        | (button, pressed) == (backwardButton, True ) = void $ writeIORef buttonVar Backward    >> tryPutMVar updatedVar ()
        | otherwise                                    = void $ writeIORef buttonVar Highlight   >> tryPutMVar updatedVar ()
      device :: Device Int Int Int Double
      device = Tracker selectDevice (Just callback) Nothing Nothing
      device' :: Device Int Int Int Double
      device' = Button buttonDevice (Just callback')
    remotes <- liftIO $ mapM openDevice [device, device']
    let
      loop =
        do
          liftIO $ mapM_ mainLoop remotes
          updated <- liftIO $ tryTakeMVar updatedVar
          when (updated == Just ())
            $ do
              location <- liftIO $ readIORef locationVar
              button <- liftIO $ readIORef buttonVar
              mid1 <- nextMessageIdentifier
              frameDebug DebugMessage $ "TS SC 1\t" ++ messageTag (UpdateSelection mid1 location button)
              sendChan listener $!! UpdateSelection mid1 location button
          loop
    loop

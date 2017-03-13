{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.Track.VRPN (
  trackPov
, trackRelocation
, trackSelection
) where


import Control.Concurrent.MVar (newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Distributed.Process (Process, SendPort, expect, getSelfPid, liftIO, say, sendChan)
import Control.Monad (void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Input (Input(InputVRPN))
import InfoVis.Parallel.Types.Input.VRPN (InputVRPN(..))
import InfoVis.Parallel.Types.Message (CommonMessage(..), DisplayerMessage(..), SelecterMessage(..), SelectionAction(..))
import Linear.Affine (Point(..))
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Linear.Vector (zero)
import Network.VRPN (ButtonCallback, PositionCallback, Device(Button, Tracker), mainLoop, openDevice)


trackPov :: SendPort DisplayerMessage -> Process ()
trackPov listener = -- FIXME: Support reset, termination, and faults.
  do
    pid <- getSelfPid
    say $ "Starting point-of-view tracker <" ++ show pid ++ ">."
    locationVar <- liftIO $ newIORef zero
    orientationVar <- liftIO $ newIORef zero
    updatedVar <- liftIO newEmptyMVar
    Reconfigure Configuration{..} <- expect
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
              listener `sendChan` Track location orientation
          loop
    loop


trackRelocation :: SendPort SelecterMessage -> Process ()
trackRelocation _ = -- FIXME: Support reset, termination, and faults.
  do
    pid <- getSelfPid
    say $ "Starting relocation tracker <" ++ show pid ++ ">."
    Reconfigure Configuration{..} <- expect
    let
      InputVRPN Input{..} = input
    return ()


trackSelection :: SendPort SelecterMessage -> Process ()
trackSelection listener = -- FIXME: Support reset, termination, and faults.
  do
    pid <- getSelfPid
    say $ "Starting selection tracker <" ++ show pid ++ ">."
    locationVar <- liftIO $ newIORef zero
    buttonVar <- liftIO $ newIORef Highlight
    updatedVar <- liftIO newEmptyMVar
    Reconfigure Configuration{..} <- expect
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
        |          pressed                             = void $ writeIORef buttonVar Highlight   >> tryPutMVar updatedVar ()
        | otherwise                                    = return ()
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
              listener `sendChan` UpdateSelection location button
          loop
    loop

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
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import InfoVis.Parallel.Types.Configuration (Input(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), SelectionAction(..), TrackerMessage(..))
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
    ResetTracker Input{..} <- expect
    locationVar <- liftIO $ newIORef zero
    orientationVar <- liftIO $ newIORef zero
    updatedVar <- liftIO newEmptyMVar
    let
      callback :: PositionCallback Int Double
      callback _ 0 (lx, ly, lz) (ox, oy, oz, ow) =
        do
          writeIORef locationVar . P $ V3 lx ly lz
          writeIORef orientationVar $ Quaternion ow $ V3 ox oy oz
          void $ tryPutMVar updatedVar ()
      callback _ _ _ _ = return ()
      device :: Device Int Int Int Double
      device = Tracker "Tracker0@192.168.56.100" (Just callback) Nothing Nothing
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
    ResetTracker Input{..} <- expect
    return ()


trackSelection :: SendPort SelecterMessage -> Process ()
trackSelection listener = -- FIXME: Support reset, termination, and faults.
  do
    pid <- getSelfPid
    say $ "Starting selection tracker <" ++ show pid ++ ">."
    ResetTracker Input{..} <- expect
    locationVar <- liftIO $ newIORef zero
    buttonVar <- liftIO $ newIORef Highlight
    updatedVar <- liftIO newEmptyMVar
    let
      callback :: PositionCallback Int Double
      callback _ 1 (lx, ly, lz) _ =
        do
          writeIORef locationVar . P $ V3 lx ly lz
          void $ tryPutMVar updatedVar ()
      callback _ _ _ _ = return ()
      callback' :: ButtonCallback Int
      callback' _ button pressed =
        void
          $ case (button, pressed) of
            (1, True ) -> writeIORef buttonVar Selection   >> tryPutMVar updatedVar ()
            (2, True ) -> writeIORef buttonVar Deselection >> tryPutMVar updatedVar ()
            (0, True ) -> writeIORef buttonVar Clear       >> tryPutMVar updatedVar ()
            (_, False) -> writeIORef buttonVar Highlight   >> tryPutMVar updatedVar ()
            _          -> return False
      device :: Device Int Int Int Double
      device = Tracker "Tracker0@192.168.56.100" (Just callback) Nothing Nothing
      device' :: Device Int Int Int Double
      device' = Button "Tracker0@192.168.56.100" (Just callback')
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
--            liftIO . putStrLn $ "TRCK\t" ++ show location
              listener `sendChan` UpdateSelecter location button
          loop
    loop

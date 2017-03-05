{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Process (
  soloMain
, masterMain
, __remoteTable
, displayerProcess__tdict
) where


import Control.Concurrent (forkOS)
import Control.Concurrent.MVar (newMVar, swapMVar)
import Control.Distributed.Process (NodeId, Process, ProcessId, expect, getSelfPid, liftIO, say, send, spawn, spawnLocal)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Monad (forever, void)
import InfoVis.Parallel.Process.DataProvider (provider)
import InfoVis.Parallel.Process.Display (displayer)
import InfoVis.Parallel.Process.Track (trackPov, trackRelocation, trackSelection)
import InfoVis.Parallel.Types.Configuration (Configuration(..), peersList)
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelectionAction(Highlight), TrackerMessage(..))
import Linear.Quaternion (Quaternion(..))
import Linear.Vector (zero)


providerProcess :: Configuration Double -> [ProcessId] -> [ProcessId] -> Process ()
providerProcess configuration trackers displayers =
  do
    pid <- getSelfPid
    say $ "Starting data provider <" ++ show pid ++ ">."
    gridsLinks <- liftIO $ provider configuration
    mapM_ (`send` AugmentTracker   gridsLinks) trackers
    mapM_ (`send` AugmentDisplayer gridsLinks) displayers


trackerProcesses :: Configuration Double -> [ProcessId] -> Process ()
trackerProcesses Configuration{..} listeners =
  do
    pid <- getSelfPid
    say $ "Starting trackers <" ++ show pid ++ ">."
    trackers <- mapM (spawnLocal . ($ listeners)) [trackPov, trackRelocation, trackSelection]
    mapM_ (`send` ResetTracker input) trackers
    AugmentTracker _ <- expect
    return ()


displayerProcess :: (Configuration Double, Int) -> Process ()
displayerProcess (configuration, displayIndex) =
  do
    pid <- getSelfPid
    say $ "Starting displayer <" ++ show pid ++ ">."
    povVar <- liftIO $ newMVar (zero, Quaternion 1 zero)
    relocationVar <- liftIO $ newMVar (zero, Quaternion 1 zero)
    selectionVar <- liftIO $ newMVar (zero, Highlight)
    forever
      $ do
        message <- expect
        case message of
          AugmentDisplayer{..} -> void . liftIO . forkOS $ displayer configuration displayIndex augmentations (povVar, relocationVar, selectionVar)
          Track{..}            -> void . liftIO $ swapMVar povVar (eyePosition, eyeOrientation)
          Relocate{..}         -> void . liftIO $ swapMVar relocationVar (centerDisplacement, centerRotation)
          Select{..}           -> void . liftIO $ swapMVar selectionVar (selectPosition, selectState)
          _                    -> return ()


remotable ['displayerProcess]


masterMain :: Configuration Double -> [NodeId] -> Process ()
masterMain configuration peers =
  do
    pid <- getSelfPid
    peerPids <-
      sequence
        [
          do
            say $ "Spawning display " ++ show i ++ " <" ++ show peer ++ ">."
            spawn peer ($(mkClosure 'displayerProcess) (configuration, i))
        |
          (peer, i) <- zip peers [0 .. length (peersList configuration)]
        ]
    say $ "Spawning tracker <" ++ show pid ++ ">."
    trackerPid <- spawnLocal $ trackerProcesses configuration peerPids
    say $ "Spawning data provider <" ++ show pid ++ ">."
    void . spawnLocal $ providerProcess configuration [trackerPid] peerPids
    expect :: Process ()


soloMain :: Configuration Double -> [NodeId] -> Process ()
soloMain configuration [] =
  do
    peerPid <- spawnLocal (displayerProcess (configuration, 0))
    trackerPid <- spawnLocal $ trackerProcesses configuration [peerPid]
    void . spawnLocal $ providerProcess configuration [trackerPid] [peerPid]
    expect :: Process ()
soloMain _ (_ : _) = error "Some peers specified."

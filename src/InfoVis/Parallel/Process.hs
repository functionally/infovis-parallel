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
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Distributed.Process (NodeId, Process, ProcessId, expect, getSelfPid, liftIO, say, send, spawn, spawnLocal)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Monad (forever, void)
import InfoVis.Parallel.Process.DataProvider (provider)
import InfoVis.Parallel.Process.Display (displayer)
import InfoVis.Parallel.Process.Select (selecter)
import InfoVis.Parallel.Process.Track (trackPov, trackRelocation, trackSelection)
import InfoVis.Parallel.Process.Util (collectMessages)
import InfoVis.Parallel.Types.Configuration (Configuration(..), peersList)
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), TrackerMessage(..))


providerProcess :: Configuration Double -> ProcessId -> [ProcessId] -> Process ()
providerProcess configuration selecterPid displayers =
  do
    pid <- getSelfPid
    say $ "Starting data provider <" ++ show pid ++ ">."
    gridsLinks <- liftIO $ provider configuration
    selecterPid `send` AugmentSelecter gridsLinks
    mapM_ (`send` AugmentDisplayer gridsLinks) displayers


trackerProcesses :: Configuration Double -> ProcessId -> [ProcessId] -> Process ()
trackerProcesses Configuration{..} selecterPid listeners =
  do
    pid <- getSelfPid
    say $ "Starting trackers <" ++ show pid ++ ">."
    povTrackerPid <- spawnLocal $ trackPov listeners
    relocationTrackerPid <- spawnLocal $ trackRelocation selecterPid
    selectionTrackerPid <- spawnLocal $ trackSelection [selecterPid]
    mapM_ (`send` ResetTracker input) [povTrackerPid, relocationTrackerPid, selectionTrackerPid]


displayerProcess :: (Configuration Double, Int) -> Process ()
displayerProcess (configuration, displayIndex) =
  do
    pid <- getSelfPid
    say $ "Starting displayer <" ++ show pid ++ ">."
    messageVar <- liftIO $ newEmptyMVar
    let
      waitForGrid priors =
        do
          message <- expect
          case message of
            AugmentDisplayer{..} -> return (priors, augmentations)
            _                    -> waitForGrid $ message : priors
      collector Track{}    _ = True
      collector Relocate{} _ = True
      collector _         _ = False
    (priors, gridsLinks) <- waitForGrid []
    void . liftIO . forkOS $ displayer configuration displayIndex gridsLinks messageVar
    mapM_ (liftIO . putMVar messageVar) $ reverse priors
    forever
      $ do
        messages <- collectMessages collector
        mapM_ (liftIO . putMVar messageVar) messages


remotable ['displayerProcess]


masterMain :: Configuration Double -> [NodeId] -> Process ()
masterMain configuration peers =
  do
    pid <- getSelfPid
    displayerPids <-
      sequence
        [
          do
            say $ "Spawning display " ++ show i ++ " <" ++ show peer ++ ">."
            spawn peer ($(mkClosure 'displayerProcess) (configuration, i))
        |
          (peer, i) <- zip peers [0 .. length (peersList configuration)]
        ]
    say $ "Spawning selecter <" ++ show pid ++ ">."
    selecterPid <- spawnLocal $ selecter displayerPids
    selecterPid `send` ResetSelecter configuration
    say $ "Spawning tracker <" ++ show pid ++ ">."
    void . spawnLocal $ trackerProcesses configuration selecterPid displayerPids
    say $ "Spawning data provider <" ++ show pid ++ ">."
    void . spawnLocal $ providerProcess configuration selecterPid displayerPids
    expect :: Process ()


soloMain :: Configuration Double -> [NodeId] -> Process ()
soloMain configuration [] =
  do
    displayerPids <- (: []) <$> spawnLocal (displayerProcess (configuration, 0))
    selecterPid <- spawnLocal $ selecter displayerPids
    selecterPid `send` ResetSelecter configuration
    void . spawnLocal $ trackerProcesses configuration selecterPid displayerPids
    void . spawnLocal $ providerProcess configuration selecterPid displayerPids
    expect :: Process ()
soloMain _ (_ : _) = error "Some peers specified."

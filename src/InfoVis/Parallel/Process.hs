{-# LANGUAGE CPP             #-}
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
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Distributed.Process (NodeId, Process, ProcessId, ReceivePort, SendPort, expect, getSelfPid, liftIO, newChan, receiveChan, say, send, sendChan, spawn, spawnLocal)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Monad (forever, unless, void, when)
import InfoVis.Parallel.Process.DataProvider (provider)
import InfoVis.Parallel.Process.Display (displayer)
import InfoVis.Parallel.Process.Select (selecter)
import InfoVis.Parallel.Process.Util (collectChanMessages)
import InfoVis.Parallel.Types.Configuration (Configuration(..), peersList)
import InfoVis.Parallel.Types.Input (Input(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), MasterMessage(..), SelecterMessage(..), TrackerMessage(..))

#ifdef INFOVIS_KAFKA
import qualified InfoVis.Parallel.Process.Track.Kafka as Kafka (trackPov, trackRelocation, trackSelection)
#endif

#ifdef INFOVIS_VRPN
import qualified InfoVis.Parallel.Process.Track.VRPN as VRPN (trackPov, trackRelocation, trackSelection)
#endif


trackPov :: Input ->  SendPort DisplayerMessage -> Process ()
trackPov NoInput = error "trackPov: No input specified."
#ifdef INFOVIS_KAFKA
trackPov InputKafka{} = Kafka.trackPov
#endif
#ifdef INFOVIS_VRPN
trackPov InputVRPN{} = VRPN.trackPov
#endif

trackRelocation :: Input ->  SendPort SelecterMessage -> Process ()
trackRelocation NoInput = error "trackRelocation: No input specified."
#ifdef INFOVIS_KAFKA
trackRelocation InputKafka{} = Kafka.trackRelocation
#endif
#ifdef INFOVIS_VRPN
trackRelocation InputVRPN{} = VRPN.trackRelocation
#endif

trackSelection :: Input ->  SendPort SelecterMessage -> Process ()
trackSelection NoInput = error "trackSelection: No input specified."
#ifdef INFOVIS_KAFKA
trackSelection InputKafka{} = Kafka.trackSelection
#endif
#ifdef INFOVIS_VRPN
trackSelection InputVRPN{} = VRPN.trackSelection
#endif

syncDisplays :: Bool
#ifdef INFOVIS_SYNC
syncDisplays = True
#else
syncDisplays = False
#endif


multiplexerProcess :: ReceivePort DisplayerMessage -> ReceivePort DisplayerMessage -> [ProcessId] -> Process ()
multiplexerProcess control content displayerPids =
  do
    pid <- getSelfPid
    say $ "Starting multiplexer <" ++ show pid ++ ">."
    let
      waitForGrid priors =
        do
          message <- receiveChan content
          case message of
            AugmentDisplayer{..} -> return $ message : reverse priors
            _                    -> waitForGrid $ message : priors
      collector Track{}    _ = True
      collector Relocate{} _ = True
      collector _          _ = False
      sendSequence messages =
        sequence_
          [
            do
              mapM_ (`send` message) displayerPids
              when syncDisplays
                . void $ receiveChan control
              mapM_ (`send` DisplayDisplayer) displayerPids
          |
            message <- messages
          ]
    prior : priors <- waitForGrid []
    mapM_ (`send` prior) displayerPids
    sendSequence priors
    forever (sendSequence =<< collectChanMessages content collector)


trackerProcesses :: Configuration Double -> SendPort SelecterMessage -> SendPort DisplayerMessage -> Process ()
trackerProcesses Configuration{..} selecterSend multiplexer =
  do
    pid <- getSelfPid
    say $ "Starting trackers <" ++ show pid ++ ">."
    povTrackerPid <- spawnLocal $ trackPov input multiplexer
    relocationTrackerPid <- spawnLocal $ trackRelocation input selecterSend
    selectionTrackerPid <- spawnLocal $ trackSelection input selecterSend
    mapM_ (`send` ResetTracker input) [povTrackerPid, relocationTrackerPid, selectionTrackerPid]


displayerProcess :: (Configuration Double, ProcessId, Int) -> Process ()
displayerProcess (configuration, masterPid, displayIndex) =
  do
    pid <- getSelfPid
    say $ "Starting displayer <" ++ show pid ++ ">."
    AugmentDisplayer gridsLinks <- expect
    readyVar <- liftIO newEmptyMVar
    messageVar <- liftIO newEmptyMVar
    void . liftIO . forkOS $ displayer configuration displayIndex gridsLinks messageVar readyVar
    forever
      $ do
        message <- expect
        liftIO $ putMVar messageVar message
        unless (message == DisplayDisplayer)
          $ do 
            liftIO $ takeMVar readyVar
            when syncDisplays
              $ masterPid `send` Ready


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
            spawn peer ($(mkClosure 'displayerProcess) (configuration, pid, i))
        |
          (peer, i) <- zip peers [0 .. length (peersList configuration)]
        ]
    commonMain configuration displayerPids


soloMain :: Configuration Double -> [NodeId] -> Process ()
soloMain configuration [] =
  do
    pid <- getSelfPid
    displayerPids <- spawnLocal (displayerProcess (configuration, pid, 0))
    commonMain configuration [displayerPids]
soloMain _ (_ : _) = error "Some peers specified."


commonMain :: Configuration Double -> [ProcessId] -> Process ()
commonMain configuration displayerPids =
  do
    pid <- getSelfPid
    say $ "Starting master <" ++ show pid ++ ">."
    (contentSend, contentReceive) <- newChan
    (controlSend, controlReceive) <- newChan
    void . spawnLocal $ multiplexerProcess controlReceive contentReceive displayerPids
    (selecterSend, selecterReceive) <- newChan
    void . spawnLocal $ selecter selecterReceive contentSend
    selecterSend `sendChan` ResetSelecter configuration
    void . spawnLocal $ trackerProcesses configuration selecterSend contentSend
    void . spawnLocal $ provider configuration selecterSend contentSend
    let
      waitForAllReady counter =
        do
          Ready <- expect
          if counter >= length displayerPids
            then do
                   controlSend `sendChan` DisplayDisplayer
                   waitForAllReady 1
            else waitForAllReady $ counter + 1
    waitForAllReady 1 :: Process ()

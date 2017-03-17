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
import Control.DeepSeq (($!!))
import Control.Distributed.Process (NodeId, Process, ProcessId, ReceivePort, SendPort, expect, liftIO, newChan, receiveChan, send, sendChan, spawn, spawnLocal)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Monad (forever, void, when)
import Data.Maybe (fromJust)
import InfoVis.Parallel.Process.DataProvider (provider)
import InfoVis.Parallel.Rendering.Display (displayer)
import InfoVis.Parallel.Process.Select (selecter)
import InfoVis.Parallel.Process.Track (trackPov, trackRelocation, trackSelection)
import InfoVis.Parallel.Process.Util (Debug(..), collectChanMessages, initializeDebug, frameDebug)
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..), Configuration(..), peersList)
import InfoVis.Parallel.Types.Message (CommonMessage(..), DisplayerMessage(..), MasterMessage(..), SelecterMessage(..), messageTag)


multiplexerProcess :: Configuration -> ReceivePort CommonMessage -> ReceivePort DisplayerMessage -> [ProcessId] -> Process ()
multiplexerProcess Configuration{..} control content displayerPids =
  do
    frameDebug DebugInfo  "Starting multiplexer."
    let
      Just AdvancedSettings{..} = advanced
      waitForGrid priors =
        do
          message <- receiveChan content
          frameDebug DebugMessage $ "MX RC 1\t" ++ messageTag RefreshDisplay
          case message of
            AugmentDisplay{..} -> return $ message : reverse priors
            _                  -> waitForGrid $ message : priors
      collector Track{}    _ = True
      collector Relocate{} _ = True
      collector _          _ = False
      sendSequence messages =
        sequence_
          [
            do
              frameDebug DebugMessage $ "MX SE 1\t" ++ messageTag message
              mapM_ (`send` message) displayerPids
              when synchronizeDisplays
                $ do
                   void $ receiveChan control
                   frameDebug DebugMessage $ "MX RC 2\t" ++ messageTag RefreshDisplay
                   mapM_ (`send` RefreshDisplay) displayerPids
          |
            message <- messages
          ]
    prior : priors <- waitForGrid []
    frameDebug DebugMessage $ "MX SE 3\t" ++ messageTag prior
    mapM_ (`send` prior) displayerPids
    sendSequence priors
    forever (sendSequence =<< collectChanMessages maximumTrackingCompression content collector)


trackerProcesses :: Configuration -> SendPort SelecterMessage -> SendPort DisplayerMessage -> Process ()
trackerProcesses configuration@Configuration{..} selecterSend multiplexer =
  do
    frameDebug DebugInfo  "Starting trackers."
    void . spawnLocal $ trackPov        configuration multiplexer
    void . spawnLocal $ trackRelocation configuration selecterSend
    void . spawnLocal $ trackSelection  configuration selecterSend


displayerProcess :: (Configuration , SendPort MasterMessage, Int) -> Process ()
displayerProcess (configuration, masterSend, displayIndex) =
  do
    initializeDebug . fromJust $ advanced configuration
    frameDebug DebugInfo  "Starting displayer."
    let
      Just AdvancedSettings{..} = advanced configuration
    message'@(AugmentDisplay gridsLinks) <- expect
    frameDebug DebugMessage $ "DI EX 1\t" ++ messageTag message'
    readyVar <- liftIO newEmptyMVar
    messageVar <- liftIO newEmptyMVar
    void . liftIO . forkOS $ displayer configuration displayIndex gridsLinks messageVar readyVar
    forever
      $ do
        message <- expect
        frameDebug DebugMessage $ "DI EX 2\t" ++ messageTag message
        liftIO . putMVar messageVar $!! message
        when (synchronizeDisplays && message /= RefreshDisplay)
          $ do 
            liftIO $ takeMVar readyVar
            frameDebug DebugMessage $ "DI SC 3\t" ++ messageTag Ready
            masterSend `sendChan` Ready


remotable ['displayerProcess]


masterMain :: Configuration -> [NodeId] -> Process ()
masterMain configuration peers =
  do
    initializeDebug . fromJust $ advanced configuration
    (masterSend, masterReceive) <- newChan
    displayerPids <-
      sequence
        [
          do
            frameDebug DebugInfo $ "Spawning display " ++ show i ++ " <" ++ show peer ++ ">."
            spawn peer ($(mkClosure 'displayerProcess) (configuration, masterSend, i))
        |
          (peer, i) <- zip peers [0 .. length (peersList configuration)]
        ]
    commonMain configuration masterReceive displayerPids


soloMain :: Configuration -> [NodeId] -> Process ()
soloMain configuration [] =
  do
    initializeDebug . fromJust $ advanced configuration
    (masterSend, masterReceive) <- newChan
    displayerPids <- spawnLocal (displayerProcess (configuration, masterSend, 0))
    commonMain configuration masterReceive [displayerPids]
soloMain _ (_ : _) = error "Some peers specified."


commonMain :: Configuration -> ReceivePort MasterMessage -> [ProcessId] -> Process ()
commonMain configuration masterReceive displayerPids =
  do
    frameDebug DebugInfo  "Starting master."
    (contentSend, contentReceive) <- newChan
    (controlSend, controlReceive) <- newChan
    void . spawnLocal $ multiplexerProcess configuration controlReceive contentReceive displayerPids
    (selecterSend, selecterReceive) <- newChan
    void . spawnLocal $ selecter         configuration selecterReceive contentSend
    void . spawnLocal $ trackerProcesses configuration selecterSend    contentSend
    void . spawnLocal $ provider         configuration selecterSend    contentSend
    let
      waitForAllReady counter =
        do
          Ready <- receiveChan masterReceive
          frameDebug DebugMessage $ "CM RC 1\t" ++ messageTag Ready
          if counter >= length displayerPids
            then do
                   frameDebug DebugMessage $ "CM SC 2\t" ++ messageTag Ready
                   controlSend `sendChan` Synchronize
                   waitForAllReady 1
            else waitForAllReady $ counter + 1
    waitForAllReady 1 :: Process ()

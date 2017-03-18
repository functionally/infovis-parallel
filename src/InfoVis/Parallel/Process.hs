{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}


module InfoVis.Parallel.Process (
  soloMain
, masterMain
, __remoteTable
, displayerProcess__tdict
) where


import Control.Concurrent (forkOS)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (newTMVarIO, putTMVar, takeTMVar, tryReadTMVar)
import Control.Concurrent.STM.TVar (newTVarIO, modifyTVar')
import Control.DeepSeq (force)
import Control.Distributed.Process (NodeId, Process, ProcessId, ReceivePort, SendPort, expect, liftIO, newChan, receiveChan, receiveChanTimeout, send, sendChan, spawn, spawnLocal)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Monad (forever, void, when)
import Data.Default (def)
import Data.List (union)
import Data.Maybe (fromJust)
import InfoVis.Parallel.Process.DataProvider (provider)
import InfoVis.Parallel.Rendering.Display (displayer)
import InfoVis.Parallel.Process.Select (selecter)
import InfoVis.Parallel.Process.Track (trackPov, trackRelocation, trackSelection)
import InfoVis.Parallel.Process.Util (Debug(..), collectChanMessages, initializeDebug, frameDebug)
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..), Configuration(..), peersList)
import InfoVis.Parallel.Types.Message (CommonMessage(..), DisplayerMessage(..), MasterMessage(..), SelecterMessage(..), messageTag, nextMessageIdentifier)

import qualified InfoVis.Parallel.Rendering.Display as C (Changes(..))


multiplexerProcess :: Configuration -> ReceivePort CommonMessage -> ReceivePort DisplayerMessage -> [ProcessId] -> Process ()
multiplexerProcess Configuration{..} control content displayerPids =
  do
    frameDebug DebugInfo  "Starting multiplexer."
    let
      Just AdvancedSettings{..} = advanced
      waitForGrid priors =
        do
          message <- receiveChan content
          frameDebug DebugMessage $ "MX RC 1\t" ++ messageTag message
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
              frameDebug DebugMessage $ "MX SE 2\t" ++ messageTag message
              mapM_ (`send` message) displayerPids
              when synchronizeDisplays
                $ do
                   maybeMessage <- receiveChanTimeout 0 control
                   case maybeMessage of
                     Just Synchronize -> do
                                           frameDebug DebugMessage "MX RC 3\tCommon\tSynchronize\tn/a"
                                           mid3 <- nextMessageIdentifier
                                           frameDebug DebugMessage $ "MX SE 4\t" ++ messageTag (RefreshDisplay mid3)
                                           mapM_ (`send` RefreshDisplay mid3) displayerPids
                     _                -> return ()
          |
            message <- messages
          ]
    prior : priors <- waitForGrid []
    frameDebug DebugMessage $ "MX SE 5\t" ++ messageTag prior
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
    AugmentDisplay mid1 gridsLinks <- expect
    frameDebug DebugMessage $ "DI EX 1\t" ++ messageTag (AugmentDisplay mid1 gridsLinks)
    readyVar <- liftIO . newTMVarIO $ not synchronizeDisplays
    changesVar <- liftIO $ newTVarIO def
    void . liftIO . forkOS $ displayer configuration displayIndex gridsLinks changesVar readyVar
    forever
      $ do
        when synchronizeDisplays
          $ do
            ready <- liftIO . atomically $ (== Just False) <$> tryReadTMVar readyVar
            when ready
              $ do
                mid3 <- nextMessageIdentifier
                frameDebug DebugMessage $ "DI SC 2\t" ++ messageTag (Ready mid3)
                masterSend `sendChan` Ready mid3
                liftIO . void . atomically $ takeTMVar readyVar
        message <- expect
        frameDebug DebugMessage $ "DI EX 3\t" ++ messageTag message
        liftIO
          . atomically
          $ case message of
            RefreshDisplay{} -> putTMVar readyVar True
            AugmentDisplay{} -> return ()
            Track{..}        -> modifyTVar' changesVar
                                  $ \c ->
                                    c
                                    {
                                      C.eyeLocation    = force eyePosition
                                    , C.eyeOrientation = force eyeOrientation
                                    }
            Relocate{..}     -> modifyTVar' changesVar
                                  $ \c ->
                                    c
                                    {
                                      C.centerOffset      = force centerDisplacement
                                    , C.centerOrientation = force centerRotation
                                    }
            Select{..}       -> modifyTVar' changesVar
                                  $ \c ->
                                    c
                                    {
                                      C.selectLocation = force   selectorLocation
                                    , C.selectChanges  = force $ selectionChanges `union` C.selectChanges c
                                    }


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
          Ready mid1 <- receiveChan masterReceive
          frameDebug DebugMessage $ "CM RC 1\t" ++ messageTag (Ready mid1)
          if counter >= length displayerPids
            then do
                   mid2 <- nextMessageIdentifier
                   frameDebug DebugMessage $ "CM SC 2\t" ++ messageTag (Ready mid2)
                   controlSend `sendChan` Synchronize
                   waitForAllReady 1
            else waitForAllReady $ counter + 1
    waitForAllReady 1 :: Process ()

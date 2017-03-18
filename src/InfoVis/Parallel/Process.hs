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
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, swapTMVar, readTMVar, takeTMVar)
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
                   maybeMessage <- receiveChanTimeout 0 control
                   case maybeMessage of
                     Just (Synchronize mid3)-> do
                                           frameDebug DebugMessage $ "MX RC 2\t" ++ messageTag (Synchronize mid3)
                                           mid4 <- nextMessageIdentifier
                                           frameDebug DebugMessage $ "MX SE 3\t" ++ messageTag (RefreshDisplay mid4)
                                           mapM_ (`send` RefreshDisplay mid4) displayerPids
                     _                -> return ()
          |
            message <- messages
          ]
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
    readyVar <- liftIO newEmptyTMVarIO
    changesVar <- liftIO $ newTVarIO def
    void . liftIO . forkOS $ displayer configuration displayIndex changesVar readyVar
    forever
      $ do
        when synchronizeDisplays
          $ do
            ready <- liftIO . atomically $ readTMVar readyVar
            when ready
              $ do
                mid3 <- nextMessageIdentifier
                frameDebug DebugMessage $ "DI SC 1\t" ++ messageTag (Ready mid3)
                masterSend `sendChan` Ready mid3
                liftIO . void . atomically $ swapTMVar readyVar False
        message <- expect
        frameDebug DebugMessage $ "DI EX 2\t" ++ messageTag message
        liftIO
          . atomically
          $ case message of
            RefreshDisplay{}   -> void $ takeTMVar readyVar
            SetText{..}        -> modifyTVar' changesVar
                                    $ \c ->
                                      c
                                      {
                                        C.dirty     = True
                                      , C.newText = text
                                      }
            AugmentDisplay{..} -> modifyTVar' changesVar
                                    $ \c ->
                                      c
                                      {
                                        C.dirty        = True
                                      , C.newDisplay = augmentations ++ C.newDisplay c
                                      }
            Track{..}          -> modifyTVar' changesVar
                                    $ \c ->
                                      c
                                      {
                                        C.dirty          = True
                                      , C.eyeLocation    = force eyePosition
                                      , C.eyeOrientation = force eyeOrientation
                                      }
            Relocate{..}       -> modifyTVar' changesVar
                                    $ \c ->
                                      c
                                      {
                                        C.dirty             = True
                                      , C.centerOffset      = force centerDisplacement
                                      , C.centerOrientation = force centerRotation
                                      }
            Select{..}         -> modifyTVar' changesVar
                                    $ \c ->
                                      c
                                      {
                                        C.dirty          = True
                                      , C.selectLocation = force   selectorLocation
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
                   frameDebug DebugMessage $ "CM SC 2\t" ++ messageTag (Synchronize mid2)
                   controlSend `sendChan` Synchronize mid2
                   waitForAllReady 1
            else waitForAllReady $ counter + 1
    waitForAllReady 1 :: Process ()

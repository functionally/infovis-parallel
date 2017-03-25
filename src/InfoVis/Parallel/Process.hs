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
import Control.Concurrent.STM.TVar (newTVarIO, writeTVar)
import Control.DeepSeq (force)
import Control.Distributed.Process (NodeId, Process, ProcessId, ReceivePort, SendPort, expect, liftIO, newChan, send, spawn, spawnLocal)
import Control.Distributed.Process.Debug (enableTrace, systemLoggerTracer)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Monad (forever, void, when)
import Data.Default (def)
import Data.Default.Util (nan)
import Data.Maybe (fromMaybe)
import InfoVis.Parallel.Process.DataProvider (provider)
import InfoVis.Parallel.Rendering.Display (displayer)
import InfoVis.Parallel.Process.Select (selecter)
import InfoVis.Parallel.Process.Track (trackPov, trackRelocation, trackSelection)
import InfoVis.Parallel.Process.Util (Debug(..), Debugger, makeDebugger, makeTimer, receiveChan', receiveChanTimeout', runProcess, sendChan', traceEnabled)
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..), Configuration(..), peersList)
import InfoVis.Parallel.Types.Message (CommonMessage(..), DisplayerMessage(..), MasterMessage(..), SelecterMessage(..))
import Linear.Affine (Point(..))
import Linear.V3 (V3(..))

import qualified Data.Map.Strict as M (fromList, toList, unionWith)
import qualified InfoVis.Parallel.Rendering.Display as C (Changes(..))


merge :: (Ord a, Ord b) => [(a, [(b, c)])] -> [(a, [(b, c)])] -> [(a, [(b, c)])]
merge old new =
  M.toList
    $ M.unionWith (\old' new' -> M.toList $ M.unionWith (const id) (M.fromList old') (M.fromList new'))
      (M.fromList old)
      (M.fromList new)


multiplexerProcess :: Debugger -> Configuration -> ReceivePort CommonMessage -> ReceivePort DisplayerMessage -> [ProcessId] -> Process ()
multiplexerProcess frameDebug Configuration{..} control content displayerPids =
  runProcess "multiplexer" 1 frameDebug $ \_ ->
    do
      currentHalfFrame <- makeTimer
      let
        Just AdvancedSettings{..} = advanced
        updateDelay' = fromMaybe 0 updateDelay
        reset c = c {C.sync = False, C.dirty = False, C.newText = [], C.newDisplay = [], C.selectChanges = []}
        average old new = fmap (trackAveraging *) old + fmap ((1 - trackAveraging) *) new
        loop changes remaining =
          do
            f0 <- currentHalfFrame
            maybeContent <- receiveChanTimeout' frameDebug content "MX RC 3" 90
            let
              first = isNaN x where P (V3 x _ _) = C.eyeLocation changes
              changes' =
                case maybeContent of
                  Just SetText{..}        -> changes
                                             {
                                               C.dirty   = True
                                             , C.newText = text
                                             }
                  Just AugmentDisplay{..} -> changes
                                             {
                                               C.dirty      = True
                                             , C.newDisplay = augmentations ++ C.newDisplay changes
                                             }
                  Just Track{..}          -> changes
                                             {
                                               C.dirty          = True
                                             , C.eyeLocation    = if first
                                                                    then eyePosition   
                                                                    else average (C.eyeLocation    changes) eyePosition
                                             , C.eyeOrientation = if first
                                                                    then eyeOrientation
                                                                    else average (C.eyeOrientation changes) eyeOrientation
                                             }
                  Just Relocate{..}       -> changes
                                             {
                                               C.dirty             = True
                                             , C.centerOffset      = centerDisplacement
                                             , C.centerOrientation = centerRotation
                                             }
                  Just Select{..}         -> changes
                                             {
                                               C.dirty          = True
                                             , C.currentTime    = currentTime
                                             , C.selectLocation = selectorLocation
                                             , C.selectChanges  = C.selectChanges changes `merge` selectionChanges
                                             }
                  _                       -> changes
            elapsed <- (+ negate f0) <$> currentHalfFrame
            if C.dirty changes' && elapsed > remaining
              then do
                     frameDebug DebugMessage ["MX SE 1", "DISPLAY"]
                     mapM_ (`send` force changes') displayerPids
                     when synchronizeDisplays
                       $ do
                          maybeControl <- receiveChanTimeout' frameDebug control "MX RC 2" 10
                          case maybeControl of
                            Just (Synchronize _) -> do
                                                         frameDebug DebugMessage ["MX SE 3", "SYNC"]
                                                         mapM_ (`send` changes {C.sync = True}) displayerPids
                            _                       -> return ()
                     loop (reset changes') updateDelay'
              else   loop changes' $ remaining - elapsed
      loop (def {C.eyeLocation = P $ V3 nan nan nan}) updateDelay'


displayerProcess :: (Configuration , SendPort MasterMessage, Int) -> Process ()
displayerProcess (configuration, masterSend, displayIndex) =
  do
    frameDebug <- makeDebugger $ advanced configuration
    runProcess "displayer" 2 frameDebug $ \nextMessageIdentifier ->
      do
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
                    sendChan' frameDebug nextMessageIdentifier masterSend "DI SC 1" Ready
                    liftIO . void . atomically $ swapTMVar readyVar False
            changes <- expect
            frameDebug DebugMessage ["DI EX 2", "CHANGE"]
            liftIO
              . atomically
              $ do
                writeTVar changesVar changes
                when (C.sync changes)
                  . void $ takeTMVar readyVar


trackerProcesses :: Debugger -> Configuration -> SendPort SelecterMessage -> SendPort DisplayerMessage -> Process ()
trackerProcesses frameDebug configuration@Configuration{..} selecterSend multiplexer =
  do
    frameDebug DebugInfo ["Starting trackers."]
    void . spawnLocal $ trackPov        frameDebug configuration multiplexer
    void . spawnLocal $ trackRelocation frameDebug configuration selecterSend
    void . spawnLocal $ trackSelection  frameDebug configuration selecterSend


remotable ['displayerProcess]


masterMain :: Configuration -> [NodeId] -> Process ()
masterMain configuration peers =
  do
    frameDebug <- makeDebugger $ advanced configuration
    (masterSend, masterReceive) <- newChan
    displayerPids <-
      sequence
        [
          do
            frameDebug DebugInfo ["Spawning display " ++ show i ++ " <" ++ show peer ++ ">."]
            spawn peer ($(mkClosure 'displayerProcess) (configuration, masterSend, i))
        |
          (peer, i) <- zip peers [0 .. length (peersList configuration)]
        ]
    commonMain frameDebug configuration masterReceive displayerPids


soloMain :: Configuration -> [NodeId] -> Process ()
soloMain configuration [] =
  do
    when (traceEnabled $ advanced configuration)
      $ enableTrace =<< spawnLocal systemLoggerTracer
    frameDebug <- makeDebugger $ advanced configuration
    (masterSend, masterReceive) <- newChan
    displayerPid <- spawnLocal (displayerProcess (configuration, masterSend, 0))
    commonMain frameDebug configuration masterReceive [displayerPid]
soloMain _ (_ : _) = error "Some peers specified."


commonMain :: Debugger -> Configuration -> ReceivePort MasterMessage -> [ProcessId] -> Process ()
commonMain frameDebug configuration masterReceive displayerPids =
  runProcess "master" 0 frameDebug $ \nextMessageIdentifier ->
    do
      (contentSend, contentReceive) <- newChan
      (controlSend, controlReceive) <- newChan
      void . spawnLocal $ multiplexerProcess frameDebug configuration controlReceive contentReceive displayerPids
      (selecterSend, selecterReceive) <- newChan
      void . spawnLocal $ selecter         frameDebug configuration selecterReceive contentSend
      void . spawnLocal $ trackerProcesses frameDebug configuration selecterSend    contentSend
      void . spawnLocal $ provider         frameDebug configuration selecterSend    contentSend
      let
        waitForAllReady counter =
          do
            Ready _ <- receiveChan' frameDebug masterReceive "CM RC 1"
            if counter >= length displayerPids
              then do
                     sendChan' frameDebug nextMessageIdentifier controlSend "CM SC 2" Synchronize
                     waitForAllReady 1
              else waitForAllReady $ counter + 1
      waitForAllReady 1 :: Process ()

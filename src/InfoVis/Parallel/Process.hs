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
import Control.Distributed.Process (NodeId, Process, ProcessId, ReceivePort, SendPort, expect, liftIO, newChan, receiveChan, receiveChanTimeout, send, sendChan, spawn, spawnLocal)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Monad (forever, void, when)
import Data.Default (def)
import Data.Default.Util (nan)
import Data.Maybe (fromMaybe)
import InfoVis.Parallel.Process.DataProvider (provider)
import InfoVis.Parallel.Rendering.Display (displayer)
import InfoVis.Parallel.Process.Select (selecter)
import InfoVis.Parallel.Process.Track (trackPov, trackRelocation, trackSelection)
import InfoVis.Parallel.Process.Util (Debug(..), Debugger, makeDebugger, makeTimer)
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..), Configuration(..), peersList)
import InfoVis.Parallel.Types.Message (CommonMessage(..), DisplayerMessage(..), MasterMessage(..), SelecterMessage(..), messageTag, makeNextMessageIdentifier)
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
  do
    currentHalfFrame <- makeTimer
    frameDebug DebugInfo  "Starting multiplexer."
    let
      Just AdvancedSettings{..} = advanced
      updateDelay' = fromMaybe 0 updateDelay
      reset c = c {C.sync = False, C.dirty = False, C.newText = [], C.newDisplay = [], C.selectChanges = []}
      average old new = fmap (trackAveraging *) old + fmap ((1 - trackAveraging) *) new
      loop changes remaining =
        do
          f0 <- currentHalfFrame
          maybeContent <- receiveChanTimeout 90 content
          maybe (return ()) (\m -> frameDebug DebugMessage $ "MX RC 3\t" ++ messageTag m) maybeContent
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
                   frameDebug DebugMessage "MX SE 1\tDISPLAY"
                   mapM_ (`send` force changes') displayerPids
                   when synchronizeDisplays
                     $ do
                        maybeControl <- receiveChanTimeout 10 control
                        case maybeControl of
                          Just (Synchronize mid2) -> do
                                                       frameDebug DebugMessage $ "MX RC 2\t" ++ messageTag (Synchronize mid2)
                                                       frameDebug DebugMessage "MX SE 3\tSYNC"
                                                       mapM_ (`send` changes {C.sync = True}) displayerPids
                          _                       -> return ()
                   loop (reset changes') updateDelay'
            else   loop changes' $ remaining - elapsed
    loop (def {C.eyeLocation = P $ V3 nan nan nan}) updateDelay'


displayerProcess :: (Configuration , SendPort MasterMessage, Int) -> Process ()
displayerProcess (configuration, masterSend, displayIndex) =
  do
    frameDebug <- makeDebugger $ advanced configuration
    frameDebug DebugInfo  "Starting displayer."
    nextMessageIdentifier <- makeNextMessageIdentifier 10 7
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
        changes <- expect
        frameDebug DebugMessage "DI EX 2\tCHANGE"
        liftIO
          . atomically
          $ do
            writeTVar changesVar changes
            when (C.sync changes)
              . void $ takeTMVar readyVar


trackerProcesses :: Debugger -> Configuration -> SendPort SelecterMessage -> SendPort DisplayerMessage -> Process ()
trackerProcesses frameDebug configuration@Configuration{..} selecterSend multiplexer =
  do
    frameDebug DebugInfo  "Starting trackers."
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
            frameDebug DebugInfo $ "Spawning display " ++ show i ++ " <" ++ show peer ++ ">."
            spawn peer ($(mkClosure 'displayerProcess) (configuration, masterSend, i))
        |
          (peer, i) <- zip peers [0 .. length (peersList configuration)]
        ]
    commonMain frameDebug configuration masterReceive displayerPids


soloMain :: Configuration -> [NodeId] -> Process ()
soloMain configuration [] =
  do
    frameDebug <- makeDebugger $ advanced configuration
    (masterSend, masterReceive) <- newChan
    displayerPids <- spawnLocal (displayerProcess (configuration, masterSend, 0))
    commonMain frameDebug configuration masterReceive [displayerPids]
soloMain _ (_ : _) = error "Some peers specified."


commonMain :: Debugger -> Configuration -> ReceivePort MasterMessage -> [ProcessId] -> Process ()
commonMain frameDebug configuration masterReceive displayerPids =
  do
    frameDebug DebugInfo  "Starting master."
    nextMessageIdentifier <- makeNextMessageIdentifier 10 8
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

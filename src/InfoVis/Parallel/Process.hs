{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Process (
  soloMain
, masterMain
, __remoteTable
, displayerProcess__tdict
) where


import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Distributed.Process (NodeId, Process, ProcessId, expect, getSelfPid, liftIO, say, send, spawn, spawnLocal)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Monad (void)
import Data.IORef (newIORef, readIORef, writeIORef)
import InfoVis.Parallel.Process.DataProvider (provider)
import InfoVis.Parallel.Process.Display (SelectionAction(Highlight), displayer)
import InfoVis.Parallel.Process.Track -- FIXME
import InfoVis.Parallel.Types.Configuration (Configuration(..), Input(..), peersList)
import Linear.Affine (Point)
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Linear.Vector (zero)


providerProcess :: Configuration Float -> [ProcessId] -> Process ()
providerProcess configuration listeners =
  do
    pid <- getSelfPid
    say $ "Starting data provider <" ++ show pid ++ ">."
    gridsLinks <- liftIO $ provider configuration
    mapM_ (`send` gridsLinks) listeners


trackerProcess :: Input -> [ProcessId] -> Process ()
trackerProcess input listeners =
  do
    pid <- getSelfPid
    say $ "Starting tracker <" ++ show pid ++ ">."
    flag <- liftIO newEmptyMVar
    pov <- liftIO $ newIORef (zero :: Point V3 Float , Quaternion 1 zero :: Quaternion Float)
    relocation <- liftIO $ newIORef (zero :: V3 Float, Quaternion 1 zero :: Quaternion Float)
    selection <- liftIO $ newIORef (zero :: V3 Float, Highlight)
    void . liftIO . forkIO $ trackPov input pov flag
    void . liftIO . forkIO $ trackRelocation input relocation flag
    void . liftIO . forkIO $ trackSelection input selection flag
    let
      loop =
        do
          pov' <- liftIO $ readIORef pov
          relocation' <- liftIO $ readIORef relocation
          selection' <- liftIO $ readIORef selection
          mapM_ (`send` (pov', relocation', selection')) listeners
          liftIO $ takeMVar flag
          loop
    loop


displayerProcess :: (Configuration Float, Int) -> Process ()
displayerProcess (configuration, displayIndex) =
  do
    pid <- getSelfPid
    say $ "Starting displayer <" ++ show pid ++ ">."
    gridLinks <- expect
    pov <- liftIO $ newIORef (zero :: Point V3 Float , Quaternion 1 zero :: Quaternion Float)
    relocation <- liftIO $ newIORef (zero :: V3 Float, Quaternion 1 zero :: Quaternion Float)
    selection <- liftIO $ newIORef (zero :: V3 Float, Highlight)
    void . liftIO . forkOS $ displayer configuration displayIndex gridLinks (pov, relocation, selection)
    let
      loop =
        do
          (pov', relocation', selection') <- expect
          liftIO $ writeIORef pov  pov'
          liftIO $ writeIORef relocation relocation'
          liftIO $ writeIORef selection selection'
          loop
    loop


remotable ['displayerProcess]


masterMain :: Configuration Float -> [NodeId] -> Process ()
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
    say $ "Spawning data provider <" ++ show pid ++ ">."
    void $ spawnLocal (providerProcess configuration peerPids)
    say $ "Spawning tracker <" ++ show pid ++ ">."
    void $ spawnLocal (trackerProcess (input configuration) peerPids)
    expect :: Process ()


soloMain :: Configuration Float -> [NodeId] -> Process ()
soloMain configuration [] =
  do
    peerPids <- (: []) <$> spawnLocal (displayerProcess (configuration, 0))
    void $ spawnLocal (providerProcess configuration peerPids)
    void $ spawnLocal (trackerProcess (input configuration) peerPids)
    expect :: Process ()
soloMain _ (_ : _) = error "Some peers specified."

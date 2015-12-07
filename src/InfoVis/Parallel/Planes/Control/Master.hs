{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}


module InfoVis.Parallel.Planes.Control.Master (
  master
, __remoteTable
, displayerProcess__tdict
) where


import Control.Concurrent (forkIO, takeMVar)
import Control.Monad (forM_, void, zipWithM)
import Control.Distributed.Process (NodeId, Process, ProcessId, expect, getSelfPid, liftIO, say, send, spawn, spawnLocal)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Data.Binary.Instances ()
import Data.Default (def)
import Data.IORef (newIORef)
import Data.Relational.Lists (Tabulation)
import Graphics.Rendering.DLP.Callbacks (dlpDisplayCallback)
import Graphics.Rendering.Handa.Viewer (dlpViewerDisplay)
import Graphics.Rendering.OpenGL (ClearBuffer(ColorBuffer), GLfloat, Vector3(..), ($=!), clear, get)
import Graphics.UI.GLUT (createWindow, displayCallback, initialize, mainLoop, swapBuffers)
import Graphics.UI.Handa.Setup (Setup(..), Stereo, setup)
import Graphics.UI.SpaceNavigator (Track(..))
import InfoVis.Parallel.Planes.Control (display, setupContent, setupLocationTracking)


trackerProcess :: [ProcessId] -> Process ()
trackerProcess listeners =
  do
    (location, tracking, updated) <- liftIO $ do
      void $ initialize "trackerProcess" ["-geometry", "250x50"]
      void $ createWindow "Tracker Process"
      displayCallback $=! do
        clear [ColorBuffer]
        swapBuffers
      setupLocationTracking
    _forkPid <- liftIO $ forkIO mainLoop
    let
      loop =
        do
          say "Tracker waiting."
          liftIO $ takeMVar updated
          location' <- liftIO $ get location
          tracking' <- liftIO $ get tracking
          forM_ listeners $ flip send (location', tracking')
          loop
    loop


displayerProcess :: (String, Setup, Tabulation Double) -> Process ()
displayerProcess (screen, setUp, content) =
  do
    pid <- getSelfPid
    location <- liftIO $ newIORef (Vector3 0 0 (-1.5) :: Vector3 GLfloat)
    tracking <- liftIO $ newIORef $ def {trackPosition = Vector3 0 0 1.1}
    void $ liftIO $ forkIO $ do
      (dlp, viewerParameters, _) <- setup "displayerProcess" ("Display " ++ show pid) ["-display", screen] setUp
      (configuration, grids) <- setupContent viewerParameters content
      dlpDisplayCallback $=!
        dlpViewerDisplay
          dlp
          viewerParameters
          (display configuration grids location tracking)
      mainLoop
    let
      loop =
        do
          (location', tracking') <- expect :: Process (Vector3 GLfloat, Track)
          liftIO $ do
            location $=! location'
            tracking $=! tracking'
          loop
    loop


remotable ['displayerProcess]


master :: Stereo -> Tabulation Double -> [NodeId] -> Process ()
master stereoMode content peers =
  do
    let
      setUp =
        Setup
        {
          stereo     = stereoMode
        , switchEyes = False
        , viewer     = def
        , fullscreen = True
        }
    peerPids <- zipWithM (\peer screen -> spawn peer ($(mkClosure 'displayerProcess) (":0." ++ show screen, setUp, content))) peers [(0::Int)..]
    _tracker <- spawnLocal (trackerProcess peerPids)
    expect :: Process ()

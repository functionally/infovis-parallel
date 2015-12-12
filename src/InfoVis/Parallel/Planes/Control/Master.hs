{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}


module InfoVis.Parallel.Planes.Control.Master (
  MultiDisplayConfiguration(..)
, DisplayConfiguration(..)
, peersList
, master
, __remoteTable
, displayerProcess__tdict
) where


import Control.Concurrent (forkIO, takeMVar)
import Control.Monad (forM_, void, zipWithM, guard)
import Control.Distributed.Process (NodeId, Process, ProcessId, expect, getSelfPid, liftIO, send, spawn, spawnLocal)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Data.Aeson (FromJSON)
import Data.Default (def)
import Data.IORef (newIORef)
import Data.Relational.Lists (Tabulation)
import Foreign.C.Types.Instances ()
import GHC.Generics (Generic)
import Graphics.Rendering.DLP.Callbacks (dlpDisplayCallback)
import Graphics.Rendering.Handa.Projection (Screen)
import Graphics.Rendering.Handa.Viewer (ViewerParameters(ViewerParameters), dlpViewerDisplay)
import Graphics.Rendering.OpenGL (ClearBuffer(ColorBuffer), Vector3(..), Vertex3, ($=!), clear, get)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import Graphics.UI.GLUT (createWindow, displayCallback, initialize, mainLoop, swapBuffers)
import Graphics.UI.Handa.Setup (Setup(Setup), Stereo, setup)
import Graphics.UI.SpaceNavigator (Track(..))
import InfoVis.Parallel.Planes.Control (display, setupContent, setupLocationTracking)
import InfoVis.Parallel.Planes.Grid (Resolution)

import qualified Graphics.Rendering.Handa.Viewer as V (ViewerParameters(..))
import qualified Graphics.UI.Handa.Setup as S (Setup(..))


data MultiDisplayConfiguration a =
  MultiDisplayConfiguration
  {
    stereo        :: Stereo
  , nearPlane     :: a
  , farPlane      :: a
  , eyePosition   :: Vertex3 a
  , eyeSeparation :: Vector3 a
  , eyeUpward     :: Vector3 a
  , sceneCenter   :: Vertex3 a
  , sceneScale    :: Vector3 a
  , displays      :: [DisplayConfiguration a]
  }
  deriving (Eq, Generic, Read, Show)

instance Functor MultiDisplayConfiguration where
  fmap f MultiDisplayConfiguration{..} =
    MultiDisplayConfiguration
    {
      stereo        =              stereo
    , nearPlane     =           f  nearPlane
    , farPlane      =           f  farPlane
    , eyePosition   =      fmap f  eyePosition
    , eyeSeparation =      fmap f  eyeSeparation
    , eyeUpward     =      fmap f  eyeUpward
    , sceneCenter   =      fmap f  sceneCenter
    , sceneScale    =      fmap f  sceneScale
    , displays      = map (fmap f) displays
    }

instance (FromJSON a, Generic a) => FromJSON (MultiDisplayConfiguration a)


data DisplayConfiguration a =
  DisplayConfiguration
  {
    host              :: String
  , port              :: String
  , displayIdentifier :: String
  , geometry          :: Maybe String
  , screen            :: Screen a
  }
  deriving (Eq, Generic, Read, Show)

instance Functor DisplayConfiguration where
  fmap f DisplayConfiguration{..} =
    DisplayConfiguration
    {
      host              =        host
    , port              =        port
    , displayIdentifier =        displayIdentifier
    , geometry          =        geometry
    , screen            = fmap f screen
    }

instance (FromJSON a, Generic a) => FromJSON (DisplayConfiguration a)


peersList :: MultiDisplayConfiguration a -> [(String, String)]
peersList MultiDisplayConfiguration{..} =
  map (\DisplayConfiguration{..} -> (host, port)) displays


trackerProcess :: [ProcessId] -> Process ()
trackerProcess listeners =
  do
    pid <- getSelfPid
    (location, tracking, updated) <- liftIO $ do
      void $ initialize "trackerProcess" ["-geometry", "250x50"]
      void $ createWindow $ "<" ++ show pid ++ ">"
      displayCallback $=! do
        clear [ColorBuffer]
        swapBuffers
      setupLocationTracking
    _forkPid <- liftIO $ forkIO mainLoop
    let
      loop =
        do
          liftIO $ takeMVar updated
          location' <- liftIO $ get location :: Process (Vector3 Resolution)
          tracking' <- liftIO $ get tracking :: Process (Track Resolution)
          forM_ listeners $ flip send (location', tracking')
          loop
    loop


displayerProcess :: (String, Maybe String, Setup Resolution, Tabulation Double) -> Process ()
displayerProcess (screen, geometry, setUp, content) =
  do
    pid <- getSelfPid
    location <- liftIO $ newIORef (Vector3 0 0 (-1.5) :: Vector3 Resolution)
    tracking <- liftIO $ newIORef $ def {trackPosition = Vector3 0 0 1.1}
    let
      setUp' = realToFrac <$> setUp :: Setup Resolution
    void $ liftIO $ forkIO $ do
      (dlp, viewerParameters, _) <-
        setup
          ("<" ++ show pid ++ ">")
          "displayerProcess"
          (
            maybe id (\g -> (++ ["-geometry", g])) geometry
              ["-display", screen]
          )
          setUp'
      (configuration, grids) <- setupContent content
      dlpDisplayCallback $=!
        dlpViewerDisplay
          dlp
          viewerParameters
          (display configuration grids location tracking)
      mainLoop
    let
      loop =
        do
          (location', tracking') <- expect :: Process (Vector3 Resolution, Track Resolution)
          liftIO $ do
            location $=! location'
            tracking $=! tracking'
          loop
    loop


remotable ['displayerProcess]


master :: MultiDisplayConfiguration Resolution -> Tabulation Double -> [NodeId] -> Process ()
master MultiDisplayConfiguration{..} content peers =
  do
    let
      setUp =
        Setup
        {
          stereo     = stereo
        , switchEyes = False
        , viewer     = undefined
        , fullscreen = False
        }
      viewer' =
        ViewerParameters
        {
          screen        = undefined
        , nearPlane     = nearPlane
        , farPlane      = farPlane
        , eyePosition   = eyePosition
        , eyeSeparation = eyeSeparation
        , eyeUpward     = eyeUpward
        , sceneCenter   = sceneCenter
        , sceneScale    = sceneScale
        }
    peerPids <-
      zipWithM 
        (\peer DisplayConfiguration{..} ->
          let
            setUp' :: Setup Resolution
            setUp' =
              setUp
                {
                  S.fullscreen = geometry == Just "fullscreen"
                , S.viewer       = Left $ viewer' {V.screen = screen}
                }
            geometry' =
              do
                guard (geometry `notElem` [Just "", Just "fullscreen"])
                geometry
          in
            spawn peer ($(mkClosure 'displayerProcess) (displayIdentifier, geometry', setUp', content))
        )
        peers
        displays
    _tracker <- spawnLocal (trackerProcess peerPids)
    expect :: Process ()

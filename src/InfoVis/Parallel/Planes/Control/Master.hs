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


import Control.Concurrent (forkOS, newMVar, takeMVar)
import Control.Monad (forM_, guard, void, zipWithM)
import Control.Distributed.Process (NodeId, Process, ProcessId, expect, getSelfPid, liftIO, say, send, spawn, spawnLocal)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Data.Aeson (FromJSON)
import Data.Default (def)
import Data.IORef (newIORef)
import Data.Relational.Lists (Tabulation)
import Foreign.C.Types.Instances ()
import GHC.Generics (Generic)
import Graphics.Rendering.DLP.Callbacks (dlpDisplayCallback)
import Graphics.Rendering.Handa.Projection (Screen)
import Graphics.Rendering.Handa.Viewer (ViewerParameters(ViewerParameters), dlpViewerDisplay')
import Graphics.Rendering.OpenGL (ClearBuffer(ColorBuffer), Vector3(..), Vertex3(..), ($=!), ($~!), clear, get)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import Graphics.UI.GLUT (createWindow, displayCallback, initialize, keyboardMouseCallback, mainLoop, swapBuffers)
import Graphics.UI.Handa.Setup (Setup(Setup), Stereo, setup)
import Graphics.UI.SpaceNavigator (Track(..), spaceNavigatorCallback)
import InfoVis.Parallel.Planes.Control (display, keyboard, setupContent, spaceNavigator)
import InfoVis.Parallel.Planes.Control.VRPN (trackHeadAndJoystick)
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
  , headTracker   :: Maybe String
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
    , headTracker   =              headTracker
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


trackerProcess :: Vertex3 Resolution -> Vector3 Resolution -> Either String (Vertex3 Resolution)-> [ProcessId] -> Process ()
trackerProcess center scale headTracker listeners =
  do
    pid <- getSelfPid
    say $ "Starting tracker <" ++ show pid ++ ">."
    updated <- liftIO $ newMVar ()
    location <- liftIO $ newIORef $ (\(Vertex3 x y z) -> Vector3 x y z) center
    tracking <- liftIO $ newIORef $ def {trackPosition = Vector3 0 0 1.1}
    eyes <- liftIO $ newIORef $ either (const $ Vertex3 0 0 0) id headTracker
    look <- liftIO $ newIORef $ Vector3 0 0 0
    void $ liftIO $ forkOS $ do
      void $ initialize "trackerProcess" ["-geometry", "250x50"]
      void $ createWindow $ "<" ++ show pid ++ ">"
      displayCallback $=! do
        clear [ColorBuffer]
        swapBuffers
      spaceNavigatorCallback $=! Just (spaceNavigator updated tracking)
      keyboardMouseCallback $=! Just (keyboard updated location)
      either
        (trackHeadAndJoystick eyes look tracking location scale updated)
        (const $ return ())
        headTracker
      mainLoop
    let
      loop =
        do
          liftIO $ takeMVar updated
          location' <- liftIO $ get location :: Process (Vector3 Resolution)
          tracking' <- liftIO $ get tracking :: Process (Track Resolution)
          eyes'     <- liftIO $ get eyes     :: Process (Vertex3 Resolution)
          look'     <- liftIO $ get look     :: Process (Vector3 Resolution)
          forM_ listeners $ flip send (location', tracking', eyes', look')
          loop
    loop


displayerProcess :: (String, Maybe String, Setup Resolution, Tabulation Double) -> Process ()
displayerProcess (screen, geometry, setUp, content) =
  do
    pid <- getSelfPid
    say $ "Starting display <" ++ show pid ++ ">."
    let
      viewerParameters' = either id undefined $ S.viewer setUp
    viewerParameters <- liftIO $ newIORef viewerParameters'
    location <- liftIO $ newIORef $ (\(Vertex3 x y z) -> Vector3 x y z) $ V.sceneCenter viewerParameters'
    tracking <- liftIO $ newIORef $ def {trackPosition = Vector3 0 0 1.1}
    void $ liftIO $ forkOS $ do
      (dlp, _, _) <-
        setup
          ("<" ++ show pid ++ ">")
          "displayerProcess"
          (
            maybe id (\g -> (++ ["-geometry", g])) geometry
              ["-display", screen]
          )
          (realToFrac <$> setUp :: Setup Resolution)
      (configuration, grids) <- setupContent content
      dlpDisplayCallback $=!
        dlpViewerDisplay'
          False
          dlp
          viewerParameters
          (display configuration grids location tracking)
      mainLoop
    let
      loop =
        do
          (location', tracking', eyes', look') <- expect :: Process (Vector3 Resolution, Track Resolution, Vertex3 Resolution, Vector3 Resolution)
{-
          let
            Vector3 lx ly lz = realToFrac <$> location'               :: Vector3 Double
            Vector3 tx ty tz = realToFrac <$> trackPosition tracking' :: Vector3 Double
            Vertex3 ex ey ez = realToFrac <$> eyes'                   :: Vertex3 Double
          say $ "LOCATION " ++ show (lx, ly, lz) ++ "; SELECT " ++ show (tx, ty, tz) ++ "; EYES " ++ show (ex, ey, ez)
-}
          liftIO $ do
            location         $=! location'
            tracking         $=! tracking'
            viewerParameters $~! \vp -> vp {V.eyePosition = eyes', V.eyeSeparation = look'}
          loop
    loop


remotable ['displayerProcess]


master :: MultiDisplayConfiguration Resolution -> Tabulation Double -> [NodeId] -> Process ()
master MultiDisplayConfiguration{..} content peers =
  do
    pid <- getSelfPid
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
          do
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
            say $ "Spawning display <" ++ show peer ++ ">."
            spawn peer ($(mkClosure 'displayerProcess) (displayIdentifier, geometry', setUp', content))
        )
        peers
        displays
    say $ "Spawning tracker <" ++ show pid ++ ">."
    void $ spawnLocal (trackerProcess sceneCenter sceneScale (maybe (Right eyePosition) Left headTracker) peerPids)
    say "Waiting forever."
    expect :: Process ()

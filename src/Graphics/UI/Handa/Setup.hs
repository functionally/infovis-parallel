module Graphics.UI.Handa.Setup (
  setup
, handleArguments
, idle
) where


import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Default (def)
import Data.List ((\\))
import Data.IORef (IORef)
import Graphics.Rendering.DLP (DlpEncoding(FrameSequential), DlpState, initDlp)
import Graphics.Rendering.Handa.Viewer (ViewerParameters(eyeSeparation), desktopViewer, laptopViewer, projectorViewer, reshape)
import Graphics.Rendering.OpenGL (BlendingFactor(..), Capability(Enabled), ComparisonFunction(Less), Vector3(..), ($=), blend, blendFunc)
import Graphics.UI.GLUT (DisplayMode(..), IdleCallback, createWindow, depthFunc, getArgsAndInitialize, fullScreen, idleCallback, initialDisplayMode, postRedisplay, reshapeCallback)


setup :: String -> IO (Maybe (IORef DlpState), ViewerParameters, [String])
setup title =
  do
    (_, arguments) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow title
    depthFunc $= Just Less 
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    r@(_, viewerParameters, _) <- handleArguments arguments
    reshapeCallback $= Just (reshape viewerParameters)
    idleCallback $= Just idle
    return r


handleArguments :: [String] -> IO (Maybe (IORef DlpState), ViewerParameters, [String])
handleArguments arguments =
  do
    when ("--fullscreen" `elem` arguments) fullScreen
    dlp <-
      if "--stereo" `elem` arguments
        then Just <$> initDlp FrameSequential
        else return Nothing
    let
      viewerParameters
        | "--laptop"    `elem` arguments = laptopViewer
        | "--desktop"   `elem` arguments = desktopViewer
        | "--projector" `elem` arguments = projectorViewer
        | otherwise                      = def
      viewerParameters' =
        if "--switchEyes" `elem` arguments
        then viewerParameters {eyeSeparation = (\(Vector3 x y z) -> Vector3 (-x) (-y) (-z)) $ eyeSeparation viewerParameters}
        else viewerParameters
      keywords = ["--fullscreen", "--stereo", "--laptop", "--desktop", "--projector", "--switchEyes"]
    return (dlp, viewerParameters', arguments \\ keywords)


idle :: IdleCallback
idle = postRedisplay Nothing

module Main (
  main
) where


import Control.Applicative ((<$>))
import Data.Default (Default, def)
import Data.IORef (IORef, newIORef)
import Graphics.Rendering.Handa.Viewer (ViewerParameters(displayAspectRatio))
import Graphics.Rendering.OpenGL (ClearBuffer(..), GLdouble, GLfloat, Vector3(..), ($=!), clear, get, preservingMatrix, scale, translate)
import Graphics.UI.GLUT (DisplayCallback, displayCallback, keyboardMouseCallback, mainLoop)
import Graphics.UI.Handa.Keyboard (keyboardPosition)
import Graphics.UI.Handa.Setup (setup)
import Graphics.UI.Handa.Util (dlpViewerDisplay)
import Graphics.UI.SpaceNavigator (SpaceNavigatorCallback, Track(..), defaultQuantization, defaultTracking, doTracking', quantize, spaceNavigatorCallback, track)
import InfoVis.Parallel.Planes.Grid (drawPlanes, drawSelector)


main :: IO ()
main =
  do
    (dlp, viewerParameters, _) <- setup "Parallel Planes"
    location <- newIORef (Vector3 0 0 (-1) :: Vector3 GLfloat)
    tracking <- newIORef $ def {trackPosition = Vector3 0 0 1.1}
    spaceNavigatorCallback $=! Just (spaceNavigator tracking)
    keyboardMouseCallback $=! Just (keyboardPosition (Vector3 (-0.05) (-0.05) (-0.05)) location)
    displayCallback $=!
      dlpViewerDisplay (display (displayAspectRatio viewerParameters) location tracking)
      dlp
      viewerParameters
    mainLoop


spaceNavigator :: IORef Track -> SpaceNavigatorCallback
spaceNavigator tracking =
  quantize defaultQuantization
    $ \input -> track defaultTracking tracking input >> get tracking >>= print


display :: GLdouble -> IORef (Vector3 GLfloat) -> IORef Track -> DisplayCallback
display aspect location tracking =
  do
    Vector3 x y z <- trackPosition <$> get tracking
    let
      selection@(ix, _, _) = (round $ (x + 1) / 2 * 4 + 1, round $ (y * realToFrac aspect + 1) / 2 * 10 + 0.5, round $ (z + 1) / 2 * 10 + 0.5)
      selections = [selection | abs ((x + 1) / 2 * 4 + 1 - fromIntegral ix) < 0.15]
    location' <- get location
    translate location'
    clear [ColorBuffer, DepthBuffer]
    preservingMatrix $ do
      doTracking' tracking
      drawSelector
    preservingMatrix $ do
      scale 1 (1 / aspect) 1
      drawPlanes selections 5 10

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}


module Main (
  main
) where


import Data.Default (Default, def)
import Data.IORef (IORef, newIORef)
import Graphics.Rendering.Handa.Viewer (ViewerParameters(displayAspectRatio))
import Graphics.Rendering.OpenGL (ClearBuffer(..), GLfloat, Vector3(..), ($=!), clear, get, preservingMatrix, translate)
import Graphics.UI.GLUT (DisplayCallback, displayCallback, keyboardMouseCallback, mainLoop)
import Graphics.UI.Handa.Keyboard (keyboardPosition)
import Graphics.UI.Handa.Setup (setup)
import Graphics.UI.Handa.Util (dlpViewerDisplay)
import Graphics.UI.SpaceNavigator (SpaceNavigatorCallback, Track(..), defaultQuantization, defaultTracking, doTracking', quantize, spaceNavigatorCallback, track)
import InfoVis.Parallel.Planes.Configuration (Configuration(..))
import InfoVis.Parallel.Planes.Grid (Grids, GridsAction(..), addPoints, drawGrids, drawSelector, makeGrids, updateGrids)


main :: IO ()
main =
  do
    (dlp, viewerParameters, _) <- setup "Parallel Planes"
    location <- newIORef (Vector3 0 0 (-1.5) :: Vector3 GLfloat)
    tracking <- newIORef $ def {trackPosition = Vector3 0 0 1.1}
    spaceNavigatorCallback $=! Just (spaceNavigator tracking)
    keyboardMouseCallback $=! Just (keyboardPosition (Vector3 (-0.05) (-0.05) (-0.05)) location)
    let
      configuration = def {aspect = displayAspectRatio viewerParameters}
    grids' <- makeGrids configuration
    grids'' <- 
      addPoints grids'
        [
          [
            (1 + sin (n * theta)) / 2
          |
            theta <- [1..10]
          ]
        |
          n <- [1..100]
        ]
    grids <- newIORef grids''
    displayCallback $=!
      dlpViewerDisplay (display configuration grids location tracking)
      dlp
      viewerParameters
    mainLoop


spaceNavigator :: IORef Track -> SpaceNavigatorCallback
spaceNavigator tracking =
  quantize defaultQuantization
    $ track defaultTracking tracking


updateIORef :: (a -> IO a) -> IORef a -> IO ()
updateIORef f x =
  do
    x' <- get x
    x'' <- f x'
    x $=! x''


display :: Configuration -> IORef Grids -> IORef (Vector3 GLfloat) -> IORef Track -> DisplayCallback
display Configuration{..} grids location tracking =
  do
    location' <- get location
    Track{..} <- get tracking
    let
      gridsAction
        | trackLeftPress && trackRightPress = ClearGrids
        | trackLeftPress                    = SelectGrids
        | trackRightPress                   = DeselectGrids
        | otherwise                         = HighlightGrids
    updateIORef (updateGrids gridsAction trackPosition) grids
    translate location'
    clear [ColorBuffer, DepthBuffer]
    grids' <- get grids
    preservingMatrix $ do
      doTracking' tracking
      drawSelector grids'
    preservingMatrix $
      drawGrids grids'

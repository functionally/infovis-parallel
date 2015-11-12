{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}


module Main (
  main
) where


import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Default (Default, def)
import Data.List (transpose)
import Data.List.Split (splitOn)
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
import System.Random (randomIO)

main :: IO ()
main =
  do
    (columns : measurements) <- map (splitOn "\t") . lines <$> getContents   
    (dlp, viewerParameters, _) <- setup "Parallel Planes"
    location <- newIORef (Vector3 0 0 (-1.5) :: Vector3 GLfloat)
    tracking <- newIORef $ def {trackPosition = Vector3 0 0 1.1}
    spaceNavigatorCallback $=! Just (spaceNavigator tracking)
    keyboardMouseCallback $=! Just (keyboardPosition (Vector3 (-0.05) (-0.05) (-0.05)) location)
    let
      n = 2 * (length columns `div` 2)
      configuration =
        def
        {
          planes = n `div` 2
        , aspect = displayAspectRatio viewerParameters
        , axisLabels = take n columns
        }
    normalized <-
      sequence
      [
        do
          jitter <- replicateM (length column) randomIO
          return $ zipWith (\x r -> ((x - x0) / dx + 0.002 * (r - 0.5)) * 0.990 + 0.005) column jitter
      |
        column <- transpose $ map (map read) measurements
      , let (x0, x1) = (minimum &&& maximum) column
            dx = x1 - x0
      ]
    grids' <- makeGrids configuration
    grids'' <- addPoints grids' $ transpose normalized
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

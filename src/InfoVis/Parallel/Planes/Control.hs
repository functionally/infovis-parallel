{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}


module InfoVis.Parallel.Planes.Control (
  main
) where


import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Default (def)
import Data.List (transpose)
import Data.IORef (IORef, newIORef)
import Data.Relational (Relation(names, toLists))
import Data.Relational.Lists (Tabulation)
import Graphics.Rendering.DLP.Callbacks (dlpDisplayCallback)
import Graphics.Rendering.Handa.Viewer (ViewerParameters(displayAspectRatio), dlpViewerDisplay)
import Graphics.Rendering.OpenGL (GLfloat, Vector3(..), ($=!), get, preservingMatrix, translate)
import Graphics.UI.GLUT (DisplayCallback, keyboardMouseCallback, mainLoop)
import Graphics.UI.Handa.Keyboard (keyboardPosition)
import Graphics.UI.Handa.Setup (Setup, setup)
import Graphics.UI.SpaceNavigator (SpaceNavigatorCallback, Track(..), defaultQuantization, defaultTracking, doTracking', quantize, spaceNavigatorCallback, track)
import InfoVis.Parallel.Planes.Configuration (Configuration(..))
import InfoVis.Parallel.Planes.Grid (Grids, GridsAction(..), addPoints, drawGrids, drawSelector, makeGrids, updateGrids)
import System.Random (randomIO)


main :: String -> String -> [String] -> Setup -> Tabulation Double -> IO ()
main program title arguments setUp content =
  do
    (dlp, viewerParameters, _) <- setup program title arguments setUp
    (configuration, grids) <- setupContent viewerParameters content
    (location, tracking) <- setupLocationTracking
    dlpDisplayCallback $=!
      dlpViewerDisplay
        dlp
        viewerParameters
        (display configuration grids location tracking)
    mainLoop


setupContent :: ViewerParameters -> Tabulation Double -> IO (Configuration, IORef Grids)
setupContent viewerParameters content =
  do
    let
      columns = names content
      measurements = toLists content
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
        column <- transpose measurements
      , let (x0, x1) = (minimum &&& maximum) column
            dx = x1 - x0
      ]
    grids' <- makeGrids configuration
    grids'' <- addPoints grids' $ transpose normalized
    grids <- newIORef grids''
    return (configuration, grids)


setupLocationTracking :: IO (IORef (Vector3 GLfloat), IORef Track)
setupLocationTracking =
  do
    location <- newIORef (Vector3 0 0 (-1.5) :: Vector3 GLfloat)
    tracking <- newIORef $ def {trackPosition = Vector3 0 0 1.1}
    spaceNavigatorCallback $=! Just (spaceNavigator tracking)
    keyboardMouseCallback $=! Just (keyboardPosition (Vector3 (-0.05) (-0.05) (-0.05)) location)
    return (location, tracking)


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
    grids' <- get grids
    preservingMatrix $ do
      doTracking' tracking
      drawSelector grids'
    preservingMatrix $
      drawGrids grids'

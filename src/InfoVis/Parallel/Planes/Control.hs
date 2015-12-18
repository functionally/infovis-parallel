{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module InfoVis.Parallel.Planes.Control (
  main
, setupContent
, setupLocationTracking
, display
) where


import Control.Arrow ((&&&))
import Control.Concurrent (MVar, newMVar, tryPutMVar)
import Control.Monad (replicateM, void)
import Data.Default (def)
import Data.List (transpose)
import Data.IORef (IORef, newIORef)
import Data.Relational (Relation(names, toLists))
import Data.Relational.Lists (Tabulation)
import Foreign.Storable (Storable)
import Graphics.Rendering.DLP.Callbacks (dlpDisplayCallback)
import Graphics.Rendering.Handa.Viewer (ViewerParameters(sceneCenter), dlpViewerDisplay)
import Graphics.Rendering.OpenGL (MatrixComponent, Vector3(..), Vertex3(..), ($=!), get, preservingMatrix, translate)
import Graphics.UI.GLUT (DisplayCallback, KeyboardMouseCallback, keyboardMouseCallback, mainLoop)
import Graphics.UI.Handa.Keyboard (keyboardPosition)
import Graphics.UI.Handa.Setup (Setup, setup)
import Graphics.UI.SpaceNavigator (SpaceNavigatorCallback, Track(..), defaultQuantization, defaultTracking, doTracking', quantize, spaceNavigatorCallback, track)
import InfoVis.Parallel.Planes.Configuration (Configuration(..))
import InfoVis.Parallel.Planes.Grid (Grids, GridsAction(..), Resolution, addPoints, drawGrids, drawSelector, makeGrids, updateGrids)
import System.Random (randomIO)


main :: String -> String -> [String] -> Setup Resolution -> Tabulation Double -> IO ()
main program title arguments setUp content =
  do
    (dlp, viewerParameters, _) <- setup program title arguments setUp
    (configuration, grids) <- setupContent content
    (location, tracking, _) <- setupLocationTracking $ sceneCenter viewerParameters
    dlpDisplayCallback $=!
      dlpViewerDisplay
        True
        dlp
        viewerParameters
        (display configuration grids location tracking)
    mainLoop


setupContent :: Tabulation Double -> IO (Configuration, IORef Grids)
setupContent content =
  do
    let
      columns = names content
      measurements = toLists content
      n = 2 * (length columns `div` 2)
      configuration =
        def
        {
          planes = n `div` 2
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


setupLocationTracking :: RealFloat a => Vertex3 a -> IO (IORef (Vector3 a), IORef (Track a), MVar ())
setupLocationTracking center =
  do
    updated <- newMVar ()
    location <- newIORef $ (\(Vertex3 x y z) -> Vector3 x y z) center
    tracking <- newIORef $ def {trackPosition = Vector3 0 0 1.1}
    spaceNavigatorCallback $=! Just (spaceNavigator updated tracking)
    keyboardMouseCallback $=! Just (keyboard updated location)
    return (location, tracking, updated)


spaceNavigator :: RealFloat a => MVar () -> IORef (Track a) -> SpaceNavigatorCallback a
spaceNavigator updated tracking input =
  do
    quantize defaultQuantization (track defaultTracking tracking) input
    void $ tryPutMVar updated ()


keyboard :: RealFloat a => MVar () -> IORef (Vector3 a) -> KeyboardMouseCallback
keyboard updated location key keyState modifiers position =
  do
    keyboardPosition (Vector3 (-0.05) (-0.05) (-0.05)) location key keyState modifiers position
    void $ tryPutMVar updated ()


updateIORef :: (a -> IO a) -> IORef a -> IO ()
updateIORef f x =
  do
    x' <- get x
    x'' <- f x'
    x $=! x''


display :: (MatrixComponent a, RealFloat a, Storable a) => Configuration -> IORef Grids -> IORef (Vector3 a) -> IORef (Track a) -> DisplayCallback
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

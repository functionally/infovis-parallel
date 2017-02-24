{-# LANGUAGE RecordWildCards #-}


module Main (
  main
) where


import Data.Default (def)
import Data.IORef (IORef, newIORef)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import InfoVis.Parallel.IO (readDataset)
import InfoVis.Parallel.Primitive (prepareGrids, prepareLinks)
import InfoVis.Parallel.Rendering (DisplayBuffer(bufferIdentifier), drawBuffer, makeBuffer, updateBuffer)
import InfoVis.Parallel.Types (Coloring(..))
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Display (DisplayType(..))
import Graphics.Rendering.DLP.Callbacks (dlpDisplayCallback)
import Graphics.Rendering.Handa.Viewer (dlpViewerDisplay)
import Graphics.Rendering.OpenGL (GLfloat, Vector3(..), ($=!), preservingMatrix)
import Graphics.UI.GLUT (mainLoop)
import Graphics.UI.Handa.Setup (Setup(fullscreen), setup)
import Graphics.UI.SpaceNavigator (Track(..), defaultQuantization, defaultTracking, doTracking', quantize, spaceNavigatorCallback, track)


main :: IO ()
main =
  do
    Configuration{..} <- loadYamlSettingsArgs [] useEnv
    rs <- readDataset dataset
    let
      grids = prepareGrids world presentation
      links = prepareLinks world presentation dataset rs
    (dlp, viewerParameters, _) <- setup "InfoVis Parallel" "InfoVis Parallel" [] (def {fullscreen = True} :: Setup Double)
    buffers <- mapM makeBuffer $ links ++ grids
    mapM_ (\g -> updateBuffer g [(i, HighlightColoring) | i <- [100..200]])
      $ filter ((== LinkType) . fst . bufferIdentifier) buffers
    tracking <- newIORef $ def {trackPosition = Vector3 0 0 0} :: IO (IORef (Track GLfloat))
    spaceNavigatorCallback $=! Just
      (
        quantize defaultQuantization
          $ track defaultTracking tracking
      )
    dlpDisplayCallback $=!
      dlpViewerDisplay
        True
        dlp
        viewerParameters
        (
          preservingMatrix $ do
            doTracking' tracking
            mapM_ drawBuffer buffers
        )
    mainLoop

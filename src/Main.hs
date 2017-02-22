{-# LANGUAGE RecordWildCards #-}


module Main (
  main
) where


import Data.Default (def)
import Data.IORef (IORef, newIORef)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import InfoVis.Parallel.IO (readDataset)
import InfoVis.Parallel.Primitive (prepareGrids, prepareLinks)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Display (DisplayList(..))
import InfoVis.Parallel.Types.Scaffold (Characteristic(..))
import Graphics.Rendering.DLP.Callbacks (dlpDisplayCallback)
import Graphics.Rendering.Handa.Shape (drawShape, makeShape)
import Graphics.Rendering.Handa.Viewer (dlpViewerDisplay)
import Graphics.Rendering.OpenGL (DataType(Float), GLfloat, Vector3(..), ($=!), color, preservingMatrix)
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
    shapes <-
      sequence
        [
          makeShape 3 Float listPrimitive listVertices
            . color 
            . normalColor
            $ head listCharacteristics
        |
          DisplayList{..} <- links ++ grids
        ]
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
            mapM_ drawShape shapes
        )
    mainLoop

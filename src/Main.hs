{-# LANGUAGE RecordWildCards #-}


module Main (
  main
) where


import Data.Default (def)
import Data.IORef (IORef, newIORef)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import InfoVis.Parallel.IO (readDataset)
import InfoVis.Parallel.Presenting (linkPresentation, presentWorld)
import InfoVis.Parallel.Primitive (fromLocations, partitionPrimitives)
import InfoVis.Parallel.Scaling (scaleToWorld)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import Graphics.Rendering.DLP.Callbacks (dlpDisplayCallback)
import Graphics.Rendering.Handa.Shape (drawShape, makeShape)
import Graphics.Rendering.Handa.Viewer (dlpViewerDisplay)
import Graphics.Rendering.OpenGL (DataType(Float), GLfloat, PrimitiveMode(..), Vector3(..), ($=!), get, preservingMatrix)
import Graphics.UI.GLUT (mainLoop)
import Graphics.UI.Handa.Setup (Setup, setup)
import Graphics.UI.SpaceNavigator (Track(..), defaultQuantization, defaultTracking, doTracking', quantize, spaceNavigatorCallback, track)


main :: IO ()
main =
  do
    Configuration{..} <- loadYamlSettingsArgs [] useEnv
    rs <- {- take 5 <$> -} readDataset dataset
    let
      ws = scaleToWorld world presentation dataset <$> rs
      ls = linkPresentation presentation <$> ws
      ps = presentWorld world presentation
      (pointLinks, lineLinks, _        ) = partitionPrimitives $ fromLocations $ concat ls
      (_         , lineGrids, quadGrids) = partitionPrimitives $ fromLocations          ps
    (dlp, viewerParameters, _) <- setup "Window" "Program" ["-fullscreen"] (def :: Setup Double)
    point <- makeShape 3 Float Points pointLinks $ return ()
    line <- makeShape 3 Float Lines (lineLinks ++ lineGrids) $ return ()
    quad <- makeShape 3 Float Quads quadGrids $ return ()
    tracking <- newIORef $ def {trackPosition = Vector3 0 0 0} :: IO (IORef (Track GLfloat))
    spaceNavigatorCallback $=! Just
      (
        quantize defaultQuantization
          $ \input ->
            do
              track defaultTracking tracking input
              tracking' <- get tracking
              print tracking'
      )
    dlpDisplayCallback $=!
      dlpViewerDisplay
        True
        dlp
        viewerParameters
        (
          preservingMatrix $ do
            doTracking' tracking
            drawShape quad
            drawShape line
            drawShape point
        )
    mainLoop

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Rendering.Display (
  Changes(..)
, displayer
) where


import Control.Arrow ((&&&))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, isEmptyTMVar, putTMVar)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVar)
import Control.Exception (IOException, catch)
import Control.Monad (unless, void, when)
import Data.Default (Default(..))
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import Graphics.OpenGL.Util.Setup (dlpViewerDisplay, idle, setup)
import Graphics.Rendering.OpenGL (GLfloat, ($=))
import Graphics.Rendering.OpenGL.GL.CoordTrans (preservingMatrix, scale, translate)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector3(..))
import Graphics.Rendering.OpenGL.GL.VertexSpec (color)
import Graphics.UI.GLUT.Begin (mainLoop)
import Graphics.UI.GLUT.Callbacks.Global (idleCallback)
import Graphics.UI.GLUT.Fonts (StrokeFont(Roman), fontHeight, renderString, stringWidth)
import Graphics.UI.GLUT.Objects (Flavour(Solid), Object(Sphere'), renderObject)
import InfoVis.Parallel.Process.DataProvider (GridsLinks)
import InfoVis.Parallel.Process.Util (Debug(..), currentHalfFrameIO, frameDebugIO)
import InfoVis.Parallel.Rendering.Shapes (drawBuffer, makeBuffer, updateBuffer)
import InfoVis.Parallel.Rendering.Types (DisplayText(..))
import InfoVis.Parallel.Types (Coloring)
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..), Configuration(..))
import InfoVis.Parallel.Types.Presentation (Presentation(..))
import InfoVis.Parallel.Types.World (World(..), WorldExtent(..))
import Linear.Affine (Point(..), (.-.))
import Linear.Quaternion (Quaternion(..))
import Linear.Util (rotationFromPlane)
import Linear.Util.Graphics (toRotation, toVector3)
import Linear.V3 (V3(..))
import Linear.Vector (zero)
import Text.Printf (printf)

#ifdef INFOVIS_SWAP_GROUP
import Graphics.OpenGL.Functions (joinSwapGroup)
#endif


data Changes =
  Changes
  {
    eyeLocation       :: Point V3 Double
  , eyeOrientation    :: Quaternion Double
  , centerOffset      :: V3 Double
  , centerOrientation :: Quaternion Double
  , selectLocation    :: Point V3 Double
  , selectChanges     :: [(Int, Coloring)]
  }
    deriving (Eq, Show)

instance Default Changes where
  def =
    Changes
    {
      eyeLocation       = zero
    , eyeOrientation    = Quaternion 1 zero
    , centerOffset      = zero
    , centerOrientation = Quaternion 1 zero
    , selectLocation    = zero
    , selectChanges     = []
    }


displayer :: Configuration
          -> Int
          -> GridsLinks
          -> TVar Changes
          -> TMVar Bool
          -> IO ()
displayer Configuration{..} displayIndex (texts, grids, links) changesVar readyVar =
  do
    let
      Just AdvancedSettings{..} = advanced
    dlp <- setup debugOpenGL "InfoVis Parallel" "InfoVis Parallel" viewers displayIndex
    fontHeight' <-
     catch (fontHeight Roman)
       ((\_ -> fromIntegral <$> stringWidth Roman "wn") :: IOException -> IO GLfloat)
    gridBuffers <- mapM makeBuffer grids
    linkBuffers <- mapM makeBuffer links
    currentVar <- newIORef def
    let
      selector = color c >> renderObject Solid (Sphere' (s / 2) 12 8)
        where
          s = selectorSize presentation * baseSize world
          c = selectorColor presentation
      eraseSelection c = c {selectChanges = []}
      changeLoop =
        do
          ready <- atomically $ isEmptyTMVar readyVar
          when ready
            $ do
              f0 <- currentHalfFrameIO
              current <-
                atomically
                  $ do
                    when synchronizeDisplays
                      . void $ putTMVar readyVar True
                    current' <- readTVar changesVar
                    modifyTVar' changesVar eraseSelection
                    return current'
              writeIORef currentVar current
              when useIdleLoop
                idle
              f1 <- currentHalfFrameIO
              frameDebugIO DebugDisplay $ show displayIndex ++ "\tCHANGE\tLOCS\t" ++ printf "%.3f" (f1 - f0)
          Changes{..} <- readIORef currentVar
          unless (null selectChanges)
            $ do
                f0 <- currentHalfFrameIO
                mapM_ (`updateBuffer` selectChanges) linkBuffers -- FIXME: This could be split among frames.
                modifyIORef' currentVar eraseSelection
                when useIdleLoop
                  idle
                f1 <- currentHalfFrameIO
                frameDebugIO DebugDisplay $ show displayIndex ++ "\tSELECT\tSELECT\t" ++ printf "%.3f" (f1 - f0)
      drawText =
        sequence_
          [
            preservingMatrix $ do
              color textColor
              translate $ toVector3 (realToFrac <$> textOrigin .-. zero :: V3 GLfloat)
              toRotation qrot
              scale s s s
              translate $ Vector3 0 (- fontHeight') 0
              renderString Roman textContent
          |
            DisplayText{..} <- texts
          , let WorldExtent{..} = worldExtent world
          , let s = realToFrac textSize * realToFrac (baseSize world) / fontHeight' :: GLfloat
          , let qrot = rotationFromPlane (V3 1 0 0) (V3 0 (-1) 0) textOrigin textWidth textHeight
          ]
    idleCallback $= Just (if useIdleLoop then changeLoop else idle)
    dlpViewerDisplay dlp viewers displayIndex ((eyeLocation &&& eyeOrientation) <$> readIORef currentVar)
      $ do
        unless useIdleLoop changeLoop
        f0 <- currentHalfFrameIO
        preservingMatrix
          $ do
            P location <- selectLocation <$> readIORef currentVar
            translate (toVector3 $ realToFrac <$> location :: Vector3 GLfloat)
            selector
        f1 <- currentHalfFrameIO
        frameDebugIO DebugDisplay $ show displayIndex ++ "\tDRAW\tSELECT\t" ++ printf "%.3f" (f1 - f0)
        preservingMatrix
          $ do
            (location, orientation) <- (centerOffset &&& centerOrientation) <$> readIORef currentVar
            toRotation orientation
            translate (toVector3 $ realToFrac <$> location :: Vector3 GLfloat)
            mapM_ drawBuffer linkBuffers
            f2 <- currentHalfFrameIO
            frameDebugIO DebugDisplay $ show displayIndex ++ "\tDRAW\tLINKS\t" ++ printf "%.3f" (f2 - f1)
            mapM_ drawBuffer gridBuffers
            f3 <- currentHalfFrameIO
            frameDebugIO DebugDisplay $ show displayIndex ++ "\tDRAW\tGRIDS\t" ++ printf "%.3f" (f3 - f2)
            drawText
            f4 <- currentHalfFrameIO
            frameDebugIO DebugDisplay $ show displayIndex ++ "\tDRAW\tTOTAL\t" ++ printf "%.3f" (f4 - f3)
        f5 <- currentHalfFrameIO
        frameDebugIO DebugDisplay $ show displayIndex ++ "\tDRAW\tTEXT\t" ++ printf "%.3f" (f5 - f0)
#ifdef INFOVIS_SWAP_GROUP
    _ <- maybe (return False) joinSwapGroup useSwapGroup
#endif
    mainLoop

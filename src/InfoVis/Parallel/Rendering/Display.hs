{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Rendering.Display (
  displayer
) where


import Control.Concurrent.MVar (MVar, putMVar, tryTakeMVar)
import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import Data.IORef (newIORef, readIORef, writeIORef)
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
import InfoVis.Parallel.Rendering.Shapes (drawBuffer, makeBuffer, updateBuffer)
import InfoVis.Parallel.Rendering.Types (DisplayText(..))
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..), Configuration(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), nextMessageIdentifier)
import InfoVis.Parallel.Types.Presentation (Presentation(..))
import InfoVis.Parallel.Types.World (World(..), WorldExtent(..))
import Linear.Affine (Point(..), (.-.))
import Linear.Quaternion (Quaternion(..))
import Linear.Util (rotationFromPlane)
import Linear.Util.Graphics (toRotation, toVector3)
import Linear.V3 (V3(..))
import Linear.Vector (zero)

#ifdef INFOVIS_SWAP_GROUP
import Graphics.OpenGL.Functions (joinSwapGroup)
#endif


displayer :: Configuration
          -> Int
          -> GridsLinks
          -> MVar DisplayerMessage
          -> MVar ()
          -> IO ()
displayer Configuration{..} displayIndex (texts, grids, links) messageVar readyVar =
  do
    let
      Just AdvancedSettings{..} = advanced
    dlp <- setup debugOpenGL "InfoVis Parallel" "InfoVis Parallel" viewers displayIndex
    gridBuffers <- mapM makeBuffer grids
    linkBuffers <- mapM makeBuffer links
    povVarNext <- newIORef (zero, Quaternion 1 zero)
    povVar     <- newIORef (zero, Quaternion 1 zero)
    relocationVarNext <- newIORef (zero, Quaternion 1 zero)
    relocationVar     <- newIORef (zero, Quaternion 1 zero)
    selectionVarNext <- newIORef zero
    selectionVar     <- newIORef zero
    let
      selector = color c >> renderObject Solid (Sphere' s 12 8)
        where
          s = selectorSize presentation * baseSize world
          c = selectorColor presentation
      useNext =
        do
          readIORef povVarNext        >>= writeIORef povVar
          readIORef relocationVarNext >>= writeIORef relocationVar
          readIORef selectionVarNext  >>= writeIORef selectionVar
      messageLoop =
        do
          message <- tryTakeMVar messageVar
          case message of
            Just Track{..}        -> do
                                       writeIORef povVarNext (eyePosition, eyeOrientation)
                                       if synchronizeDisplays
                                         then putMVar readyVar ()
                                         else do
                                                useNext
                                                when useIdleLoop
                                                  idle
            Just Relocate{..}     -> do
                                       writeIORef relocationVarNext (centerDisplacement, centerRotation)
                                       if synchronizeDisplays
                                         then putMVar readyVar ()
                                         else do
                                                useNext
                                                when useIdleLoop
                                                  idle
            Just Select{..}       -> do
                                       writeIORef selectionVarNext selectorLocation
                                       mapM_ (`updateBuffer` selectionChanges) linkBuffers
                                       if synchronizeDisplays
                                         then putMVar readyVar ()
                                         else do
                                                useNext
                                                when useIdleLoop
                                                  idle
            Just RefreshDisplay{} -> do
                                       useNext
                                       when (synchronizeDisplays && useIdleLoop)
                                         idle
            _                     -> return ()
    idleCallback $= Just (if useIdleLoop then messageLoop else idle)
    h <-
     catch (fontHeight Roman)
       ((\_ -> fromIntegral <$> stringWidth Roman "wn") :: IOException -> IO GLfloat)
    let
      drawText =
        sequence_
          [
            preservingMatrix $ do
              color textColor
              translate $ toVector3 (realToFrac <$> textOrigin .-. zero :: V3 GLfloat)
              toRotation qrot
              scale s s s
              translate $ Vector3 0 (- h) 0
              renderString Roman textContent
          |
            DisplayText{..} <- texts
          , let WorldExtent{..} = worldExtent world
          , let s = realToFrac textSize * realToFrac (baseSize world) / h :: GLfloat
          , let qrot = rotationFromPlane (V3 1 0 0) (V3 0 (-1) 0) textOrigin textWidth textHeight
          ]
    dlpViewerDisplay dlp viewers displayIndex (readIORef povVar)
      $ do
        unless useIdleLoop messageLoop
        preservingMatrix
          $ do
            P location <- readIORef selectionVar  -- FIXME
            translate (toVector3 $ realToFrac <$> location :: Vector3 GLfloat)
            selector
        preservingMatrix
          $ do
            (location, orientation) <- readIORef relocationVar
            toRotation orientation
            translate (toVector3 $ realToFrac <$> location :: Vector3 GLfloat)
            mapM_ drawBuffer linkBuffers
            mapM_ drawBuffer gridBuffers
            drawText
#ifdef INFOVIS_SWAP_GROUP
    _ <- maybe (return False) joinSwapGroup useSwapGroup
#endif
    mainLoop

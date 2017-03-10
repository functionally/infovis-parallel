{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.Display (
  displayer
) where


import Control.Concurrent.MVar (MVar, newMVar, putMVar, readMVar, swapMVar, tryTakeMVar)
import Control.Exception (IOException, catch)
import Control.Monad (void, when)
import Data.Default (Default(def))
import Graphics.Rendering.DLP (DlpEncoding, DlpEye(..))
import Graphics.Rendering.DLP.Callbacks (DlpDisplay(..), dlpDisplayCallback)
import Graphics.Rendering.Handa.Face (brickFaces, drawFaces)
import Graphics.Rendering.Handa.Projection (OffAxisProjection(VTKOffAxis), projection)
import Graphics.Rendering.Handa.Util (degree)
import Graphics.Rendering.OpenGL (BlendingFactor(..), Capability(Enabled), ComparisonFunction(Greater, Less), GLfloat, MatrixComponent, MatrixMode(..), Position(..), Vector3(..), Vertex3(..), ($=!), ($=), alphaFunc, blend, blendFunc, color, loadIdentity, matrixMode, preservingMatrix, viewport)
import Graphics.Rendering.OpenGL.GL.CoordTrans (rotate, scale, translate)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import Graphics.UI.GLUT (DisplayCallback, DisplayMode(..), StrokeFont(Roman), createWindow, depthFunc, fontHeight, fullScreen, idleCallback, initialDisplayMode, initialize, mainLoop, renderString, reshapeCallback, stringWidth)
import Graphics.UI.Handa.Setup (Stereo(..), idle)
import InfoVis.Parallel.Process.DataProvider (GridsLinks)
import InfoVis.Parallel.Rendering (drawBuffer, makeBuffer, updateBuffer)
import InfoVis.Parallel.Types.Configuration (Configuration(..), Display(..), Viewers(..))
import InfoVis.Parallel.Types.Display (DisplayText(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..))
import InfoVis.Parallel.Types.Presentation (Presentation(..))
import InfoVis.Parallel.Types.World (World(..))
import Linear.Affine (Point(..), (.+^))
import Linear.Metric (dot, normalize)
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..), cross)
import Linear.Vector ((*^), zero)

import qualified Graphics.Rendering.DLP as D (DlpEncoding(..))
import qualified Linear.Quaternion as Q (rotate)

#ifdef SYNC_DISPLAYS
import Graphics.GL.Util (joinSwapGroup)
#endif


type PointOfView a = (Point V3 a, Quaternion a)


setup :: String
      -> String
      -> Viewers Double
      -> Int
      -> IO DlpEncoding
setup title program Viewers{..} displayIndex =
  do
    let
      Display{..} = displays !! displayIndex
      dlp = case stereo of
        DLP        -> D.FrameSequential
        QuadBuffer -> D.QuadBuffer
        Cardboard  -> D.SideBySide
        Mono       -> D.LeftOnly
    void
      . initialize program
      . maybe id ((("-display"  :) .) .(:)) identifier
      $ case geometry of
          Nothing           -> []
          Just "fullscreen" -> []
          Just geometry'    -> ["-geometry", geometry']
    initialDisplayMode $=
      (if stereo == QuadBuffer then (Stereoscopic :) else id)
        [WithDepthBuffer, DoubleBuffered]
    void $ createWindow title
    when (geometry == Just "fullscreen")
      fullScreen
    depthFunc $= Just Less 
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    alphaFunc $= Just (Greater, 0)
    return dlp


dlpViewerDisplay ::DlpEncoding
                 -> Viewers Double
                 -> Int
                 -> MVar (PointOfView Double)
                 -> DisplayCallback
                 -> IO ()
dlpViewerDisplay dlp Viewers{..} displayIndex pov displayAction =
  do
    let
      Display{..} = displays !! displayIndex
    reshapeCallback $= Just
      (\wh -> 
         do
           (P eyePosition, _) <- readMVar pov
           viewport $=! (Position 0 0, wh)
           matrixMode $=! Projection
           loadIdentity
           projection VTKOffAxis screen (toVertex3 $ realToFrac <$> P eyePosition) nearPlane farPlane
           matrixMode $=! Modelview 0
      )
    dlpDisplayCallback $=!
      def
      {
        dlpEncoding = dlp
      , doDisplay = \eye -> do
                              (eyePosition, eyeOrientation) <- readMVar pov
                              let
                                offset =
                                  case eye of
                                    LeftDlp  -> -0.5
                                    RightDlp ->  0.5
                                eyePosition' = eyePosition .+^ eyeOrientation `Q.rotate` (offset *^ eyeSeparation)
                              matrixMode $=! Projection
                              loadIdentity
                              projection VTKOffAxis screen (toVertex3 $ realToFrac <$> eyePosition') nearPlane farPlane
                              matrixMode $=! Modelview 0
                              loadIdentity
                              displayAction
      }


toVertex3 :: Point V3 a -> Vertex3 a
toVertex3 (P (V3 x y z)) = Vertex3 x y z


toVector3 :: V3 a -> Vector3 a
toVector3 (V3 x y z) = Vector3 x y z


toRotation :: (Floating a, MatrixComponent a) => Quaternion a -> IO ()
toRotation (Quaternion w (V3 x y z)) = rotate (2 * acos w * degree) $ Vector3 x y z


displayer :: Configuration Double
          -> Int
          -> GridsLinks
          -> MVar DisplayerMessage -- (Point V3 Double, Quaternion Double), MVar (V3 Double, Quaternion Double), MVar (Point V3 Double, SelectionAction))
          -> MVar ()
          -> IO ()
displayer Configuration{..} displayIndex (texts, grids, links) messageVar readyVar =
  do
    dlp <- setup "InfoVis Parallel" "InfoVis Parallel" viewers displayIndex
    gridBuffers <- mapM makeBuffer grids
    linkBuffers <- mapM makeBuffer links
    povVar <- newMVar (zero, Quaternion 1 zero)
    relocationVar <- newMVar (zero, Quaternion 1 zero)
    selectionVar <- newMVar zero
    let
      selector = color c >> drawFaces faces
        where
          s = selectorSize presentation * baseSize world
          c = selectorColor presentation
          faces = brickFaces s s s
    idleCallback $= Just idle
    h <-
     catch (fontHeight Roman)
       ((\_ -> fromIntegral <$> stringWidth Roman "wn") :: IOException -> IO GLfloat)
    print h
    dlpViewerDisplay dlp viewers displayIndex povVar
      $ do
        message <- tryTakeMVar messageVar
        case message of
          Just Track{..}        -> do
                                     void $ swapMVar povVar (eyePosition, eyeOrientation)
--                                   putMVar readyVar ()
          Just Relocate{..}     -> do
                                     void $ swapMVar relocationVar (centerDisplacement, centerRotation)
--                                   putMVar readyVar ()
          Just Select{..}       -> do
--                                   putStrLn $ "IDLE\t" ++ show selectorLocation
                                     void $ swapMVar selectionVar selectorLocation
{- slows things down -}              mapM_ (`updateBuffer` selectionChanges) linkBuffers
--                                   putMVar readyVar ()
--        Just DisplayDisplayer -> idle
          _                     -> return ()
        preservingMatrix
          $ do
            P location <- readMVar selectionVar  -- FIXME
--          putStrLn $ "DISP\t" ++ show location
            translate (toVector3 $ realToFrac <$> location :: Vector3 GLfloat)
            selector
        preservingMatrix
          $ do
            (location, orientation) <- readMVar relocationVar
            toRotation orientation
--          translate (toVector3 $ realToFrac <$> location :: Vector3 GLfloat)
            mapM_ drawBuffer linkBuffers
            mapM_ drawBuffer gridBuffers
            sequence_
              [
                preservingMatrix $ do
                  color textColor
                  translate $ Vector3 xo yo zo
                  toRotation qrot
                  scale s s s
                  translate $ Vector3 0 (- h) 0
                  renderString Roman textContent
              |
                DisplayText{..} <- texts
              , let P (V3 xo yo zo) = realToFrac <$> textOrigin
              , let P (V3 xw yw zw) = realToFrac <$> textWidth
              , let P (V3 xh yh zh) = realToFrac <$> textHeight
              , let s = 0.00002 + 0 * sqrt ( (xh - xo) * (xh - xo) + (yh - yo) * (yh - yo) + (zh - zo) * (zh - zo)) / h
              , let v1 = normalize $ V3 1         0         0
                    v2 = normalize $ V3 (xw - xo) (yw - yo) (zw - zo)
                    v = normalize $ v1 + v2
                    qrot = Quaternion (v `dot` v2) $ v `cross` v2
{-
Before rotation: u0, v0. After rotation: u2, v2.
Quaternion q2 = Quaternion::fromTwoVectors(u0, u2);
Vector v1 = v2.rotate(q2.conjugate());
Vector v0_proj = v0.projectPlane(u0);
Vector v1_proj = v1.projectPlane(u0);
Quaternion q1 = Quaternion::fromTwoVectors(v0_proj, v1_proj);
return (q2 * q1).normalized();
-}
              ]
#ifdef SYNC_DISPLAYS
    void $ joinSwapGroup 1
#endif
    mainLoop

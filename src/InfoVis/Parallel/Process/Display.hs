{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Process.Display (
  displayer
) where


import Control.Arrow (second)
import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import Control.Monad (void, when)
import Data.Default (Default(def))
import Data.Function.MapReduce (mapReduce)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Graphics.Rendering.DLP (DlpEncoding, DlpEye(..))
import Graphics.Rendering.DLP.Callbacks (DlpDisplay(..), dlpDisplayCallback)
import Graphics.Rendering.Handa.Face (brickFaces, drawFaces)
import Graphics.Rendering.Handa.Projection (OffAxisProjection(VTKOffAxis), projection)
import Graphics.Rendering.Handa.Util (degree)
import Graphics.Rendering.OpenGL (BlendingFactor(..), Capability(Enabled), ComparisonFunction(Less), GLfloat, MatrixComponent, MatrixMode(..), Position(..), Vector3(..), Vertex3(..), ($=!), ($=), blend, blendFunc, color, loadIdentity, matrixMode, preservingMatrix, viewport)
import Graphics.Rendering.OpenGL.GL.CoordTrans (rotate, translate)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import Graphics.UI.GLUT (DisplayCallback, DisplayMode(..), createWindow, depthFunc, fullScreen, idleCallback, initialDisplayMode, initialize, mainLoop, reshapeCallback)
import Graphics.UI.Handa.Setup (Stereo(..), idle)
import InfoVis.Parallel.Process.DataProvider (GridsLinks)
import InfoVis.Parallel.Process.Select (selecter)
import InfoVis.Parallel.Rendering (drawBuffer, makeBuffer, updateBuffer)
import InfoVis.Parallel.Types (Coloring(..))
import InfoVis.Parallel.Types.Configuration (Configuration(..), Display(..), Viewers(..))
import InfoVis.Parallel.Types.Display (DisplayList(..))
import InfoVis.Parallel.Types.Message (SelectionAction(..))
import InfoVis.Parallel.Types.Scaffold (Presentation(..), World(..))
import Linear.Affine (Point(..), (.+^))
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Linear.Vector ((*^))

import qualified Graphics.Rendering.DLP as D (DlpEncoding(..))
import qualified Linear.Quaternion as Q (rotate)



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
    idleCallback $= Just idle
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
          -> (MVar (Point V3 Double, Quaternion Double), MVar (V3 Double, Quaternion Double), MVar (Point V3 Double, SelectionAction))
          -> IO ()
displayer configuration@Configuration{..} displayIndex gridsLinks@(grids, links) (pov, relocation, selection) =
  do
    dlp <- setup "InfoVis Parallel" "InfoVis Parallel" viewers displayIndex
    gridBuffers <- mapM makeBuffer grids
    linkBuffers <- mapM makeBuffer links
    let
      selector = color c >> drawFaces faces
        where
          s = selectorSize presentation * baseSize world
          c = selectorColor presentation
          faces = brickFaces s s s
    persistentColoringsRef <- newIORef . mapReduce id (curry $ second maximum) $ (, NormalColoring) <$> concatMap listVertexIdentifiers links
    transientColoringsRef  <- newIORef . mapReduce id (curry $ second maximum) $ (, NormalColoring) <$> concatMap listVertexIdentifiers links
    dlpViewerDisplay dlp viewers displayIndex pov
      $ do
        preservingMatrix
          $ do
            (P location, press) <- readMVar selection  -- FIXME
            (location', orientation') <- readMVar relocation
            when (press /= Highlight)
              $ modifyMVar_ selection $ return . second (const Highlight)
            translate (toVector3 $ realToFrac <$> location :: Vector3 GLfloat)
            selector
            persistentColorings <- readIORef persistentColoringsRef :: IO [(Int, Coloring)]
            transientColorings <- readIORef transientColoringsRef :: IO [(Int, Coloring)]
            let
              ((persistentColorings', transientColorings'), changes) = selecter configuration gridsLinks (persistentColorings, transientColorings) (P location, press) (P location', orientation')
            modifyIORef persistentColoringsRef $ const persistentColorings'
            modifyIORef transientColoringsRef $ const transientColorings'
            mapM_ (`updateBuffer` changes) linkBuffers
        preservingMatrix
          $ do
            (location, orientation) <- readMVar relocation
            toRotation orientation
            translate (toVector3 $ realToFrac <$> location :: Vector3 GLfloat)
            mapM_ drawBuffer linkBuffers
            mapM_ drawBuffer gridBuffers
    mainLoop

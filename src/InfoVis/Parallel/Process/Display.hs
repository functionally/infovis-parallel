{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Process.Display (
  SelectionAction(..)
, displayer
) where


import Control.Arrow (second)
import Control.Monad (void, when)
import Data.Binary (Binary)
import Data.Default (Default(def))
import Data.Function.MapReduce (mapReduce)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import GHC.Generics (Generic)
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
import InfoVis.Parallel.Rendering (drawBuffer, makeBuffer, updateBuffer)
import InfoVis.Parallel.Types (Coloring(..))
import InfoVis.Parallel.Types.Configuration (Configuration(..), Display(..), Viewers(..))
import InfoVis.Parallel.Types.Display (DisplayList(..))
import InfoVis.Parallel.Types.Scaffold (Presentation(..), World(..))
import Linear.Affine (Point(..), (.+^), (.-.))
import Linear.Conjugate (conjugate)
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Linear.Vector ((*^))

import qualified Graphics.Rendering.DLP as D (DlpEncoding(..))
import qualified Linear.Quaternion as Q (rotate)



type PointOfView a = (Point V3 a, Quaternion a)


setup :: String
      -> String
      -> Viewers GLfloat
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
                 -> Viewers GLfloat
                 -> Int
                 -> IORef (PointOfView GLfloat)
                 -> DisplayCallback
                 -> IO ()
dlpViewerDisplay dlp Viewers{..} displayIndex pov displayAction =
  do
    let
      Display{..} = displays !! displayIndex
    reshapeCallback $= Just
      (\wh -> 
         do
           (P eyePosition, eyeOrientation) <- readIORef pov
           let
             eyePosition' = eyeOrientation `Q.rotate` eyePosition -- FIXME: Check this.
           viewport $=! (Position 0 0, wh)
           matrixMode $=! Projection
           loadIdentity
           projection VTKOffAxis screen (toVertex3 $ P eyePosition') nearPlane farPlane
           matrixMode $=! Modelview 0
      )
    dlpDisplayCallback $=!
      def
      {
        dlpEncoding = dlp
      , doDisplay = \eye -> do
                              (P eyePosition, eyeOrientation) <- readIORef pov
                              let
                                offset =
                                  case eye of
                                    LeftDlp  -> -0.5
                                    RightDlp ->  0.5
                                eyePosition' = eyeOrientation `Q.rotate` (eyePosition .+^ offset *^ eyeSeparation) -- FIXME: Check this.
                              matrixMode $=! Projection
                              loadIdentity
                              projection VTKOffAxis screen (toVertex3 $ P eyePosition') nearPlane farPlane
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


displayer :: Configuration Float
          -> Int
          -> GridsLinks
          -> (IORef (Point V3 Float, Quaternion Float), IORef (V3 Float, Quaternion Float), IORef (V3 Float, SelectionAction))
          -> IO ()
displayer Configuration{..} displayIndex (grids, links) (pov, relocation, selection) =
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
            (location, press) <- readIORef selection
            (location', orientation') <- readIORef relocation
            when (press /= Highlight)
              $ modifyIORef selection $ second $ const Highlight
            translate (toVector3 location :: Vector3 GLfloat)
            selector
            persistentColorings <- readIORef persistentColoringsRef :: IO [(Int, Coloring)]
            transientColorings <- readIORef transientColoringsRef :: IO [(Int, Coloring)]
            let
              V3 x y z = (P $ conjugate orientation' `Q.rotate` location) .-. P location'
              d = realToFrac $ selectorSize presentation * baseSize world / 2
              h w w' = abs (w - w' + d) <= d
              f (Vertex3 x' y' z') = h x x' && h y y' && h z z'
              g DisplayList{..} = map (second f) $ zip listVertexIdentifiers listVertices
              selections = mapReduce id (curry $ second or) $ concatMap g links :: [(Int, Bool)]
              persistentColorings' =
                case press of
                  Highlight       -> persistentColorings
                  Deselect        -> zipWith (\(i, o) (_, n) -> (i, if n then NormalColoring    else o)) persistentColorings selections
                  Select          -> zipWith (\(i, o) (_, n) -> (i, if n then SelectColoring    else o)) persistentColorings selections
                  Clear           -> map (\(i, _) -> (i, NormalColoring))                                persistentColorings
              transientColorings'  = zipWith (\(i, o) (_, n) -> (i, if n then HighlightColoring else o)) persistentColorings selections
              changes = fmap snd $ filter (uncurry (/=)) $ zip transientColorings transientColorings'
            modifyIORef persistentColoringsRef $ const persistentColorings'
            modifyIORef transientColoringsRef $ const transientColorings'
            mapM_ (`updateBuffer` changes) linkBuffers
        preservingMatrix
          $ do
            (location, orientation) <- readIORef relocation
            toRotation orientation
            translate (toVector3 location :: Vector3 GLfloat)
            mapM_ drawBuffer linkBuffers
            mapM_ drawBuffer gridBuffers
    mainLoop


data SelectionAction = Highlight | Select | Deselect | Clear
 deriving (Binary, Eq, Generic)

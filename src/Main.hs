{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module Main (
  main
) where


import Control.Arrow (first, second)
import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Data.Default (Default(def))
import Data.Function.MapReduce (mapReduce)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
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
import InfoVis.Parallel.Types.Display (DisplayList(..))
import InfoVis.Parallel.IO (readDataset)
import InfoVis.Parallel.Primitive (prepareGrids, prepareLinks)
import InfoVis.Parallel.Rendering (drawBuffer, makeBuffer, updateBuffer)
import InfoVis.Parallel.Types (Coloring(..))
import InfoVis.Parallel.Types.Configuration (Configuration(..), Display(..), Input(..), Viewers(..))
import InfoVis.Parallel.Types.Scaffold (Presentation(..))
import Linear.Affine (Point(..), (.+^), (.-.))
import Linear.Conjugate (conjugate)
import Linear.Epsilon (Epsilon)
import Linear.Quaternion (Quaternion(..), axisAngle)
import Linear.V3 (V3(..))
import Linear.Vector ((*^), basis, zero)
import Network.UI.Kafka (consumerLoop)
import Network.UI.Kafka.Types (Button(IndexButton), Event(..), Toggle(..))

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
                 -> DisplayCallback
                 -> IO (IORef (PointOfView GLfloat))
dlpViewerDisplay dlp Viewers{..} displayIndex displayAction =
  do
    let
      Display{..} = displays !! displayIndex
    pov <- newIORef (zero, Quaternion 1 zero)
    reshapeCallback $= Just
      (\wh -> 
         do
           (eyePosition, _) <- readIORef pov -- FIXME: Use eye orientation.
           viewport $=! (Position 0 0, wh)
           matrixMode $=! Projection
           loadIdentity
           projection VTKOffAxis screen (toVertex3 eyePosition) nearPlane farPlane
           matrixMode $=! Modelview 0
      )
    dlpDisplayCallback $=!
      def
      {
        dlpEncoding = dlp
      , doDisplay = \eye -> do
                              (eyePosition, _) <- readIORef pov -- FIXME: Use eye orientation.
                              let
                                offset =
                                  case eye of
                                    LeftDlp  -> -0.5
                                    RightDlp ->  0.5
                                eyePosition' = eyePosition .+^ offset *^ eyeSeparation
                              matrixMode $=! Projection
                              loadIdentity
                              projection VTKOffAxis screen (toVertex3 eyePosition') nearPlane farPlane
                              matrixMode $=! Modelview 0
                              loadIdentity
                              displayAction
      }
    return pov


toVertex3 :: Point V3 a -> Vertex3 a
toVertex3 (P (V3 x y z)) = Vertex3 x y z


toVector3 :: V3 a -> Vector3 a
toVector3 (V3 x y z) = Vector3 x y z


toRotation :: (Floating a, MatrixComponent a) => Quaternion a -> IO ()
toRotation (Quaternion w (V3 x y z)) = rotate (2 * acos w * degree) $ Vector3 x y z


fromDegrees :: (Floating a, Num a) => a -> a
fromDegrees = (/ degree)


fromEuler :: (Epsilon a, Num a, RealFloat a) => V3 a -> Quaternion a
fromEuler (V3 phi theta psi) =
  let
    [ex, ey, ez] = basis
  in
    ez `axisAngle` psi * ey `axisAngle` theta * ex `axisAngle` phi


main :: IO ()
main =
  do
    Configuration{..} <- loadYamlSettingsArgs [] useEnv
    rs <- readDataset dataset
    let
      grids = prepareGrids world presentation
      links = prepareLinks world presentation dataset rs
    dlp <- setup "InfoVis Parallel" "InfoVis Parallel" viewers 0
    gridBuffers <- mapM makeBuffer grids
    linkBuffers <- mapM makeBuffer links
    let
      selector = color c >> drawFaces faces
        where
          s = selectorSize presentation
          c = selectorColor presentation
          faces = brickFaces s s s
    relocation <- newIORef (zero, Quaternion 1 zero)
    selection <- newIORef (zero, Highlight)
    persistentColoringsRef <- newIORef . mapReduce id (curry $ second maximum) $ (, NormalColoring) <$> concatMap listVertexIdentifiers links
    transientColoringsRef  <- newIORef . mapReduce id (curry $ second maximum) $ (, NormalColoring) <$> concatMap listVertexIdentifiers links
    pov <-
      dlpViewerDisplay dlp viewers 0
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
              d = realToFrac $ selectorSize presentation / 2
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
    void . forkIO $ trackPov input pov
    void . forkIO $ trackRelocation input relocation
    void . forkIO $ trackSelection input selection
    mainLoop


trackPov :: (Epsilon a, Fractional a, RealFloat a) => Input -> IORef (PointOfView a) -> IO ()
trackPov Input{..} pov =
  case povInput of
    Left (location, orientation) -> modifyIORef pov $ const (P $ realToFrac <$> location, fromEuler $ realToFrac . fromDegrees <$> orientation)
    Right povInput'              -> do
                                      let
                                        processInput sensor (LocationEvent (x, y, z)) =
                                          when (sensor == povInput')
                                            $ modifyIORef pov
                                            $ first
                                            $ const
                                            $ realToFrac
                                            <$> P (V3 x y z)
                                        processInput sensor (OrientationEvent (w, x, y, z)) =
                                          when (sensor == povInput')
                                            $ modifyIORef pov
                                            $ second
                                            $ const
                                            $ realToFrac
                                            <$> Quaternion w (V3 x y z)
                                        processInput _ _ =
                                          return ()
                                      (_, loop) <- consumerLoop kafka processInput
                                      result <- loop
                                      either print return result


trackRelocation :: (Epsilon a, Fractional a, RealFloat a) => Input -> IORef (V3 a, Quaternion a) -> IO ()
trackRelocation Input{..} relocation =
  do
    let
      processInput sensor (LocationEvent (x, y, z)) =
        when (sensor == relocationInput)
          $ modifyIORef relocation
          $ first
          $ const
          $ realToFrac
          <$> V3 x y z
      processInput sensor (OrientationEvent (w, x, y, z)) =
        when (sensor == relocationInput)
          $ modifyIORef relocation
          $ second
          $ const
          $ realToFrac
          <$> Quaternion w (V3 x y z)
      processInput _ _ =
        return ()
    (_, loop) <- consumerLoop kafka processInput
    result <- loop
    either print return result


data SelectionAction = Highlight | Select | Deselect | Clear
 deriving Eq


trackSelection :: (Epsilon a, Fractional a, RealFloat a) => Input -> IORef (V3 a, SelectionAction) -> IO ()
trackSelection Input{..} selection =
  do
    let
      processInput sensor (LocationEvent (x, y, z)) =
        when (sensor == selectorInput)
          $ modifyIORef selection
          $ first 
          $ const
          $ realToFrac
          <$> V3 x y z
      processInput sensor (PointerEvent (x, y, z)) =
        when (sensor == selectorInput)
          $ modifyIORef selection
          $ first
          $ const
          $ realToFrac
          <$> V3 x y z
      processInput sensor (ButtonEvent (IndexButton i, Down)) =
        when ((sensor, i) `elem` [selectButton, deselectButton, clearButton])
         $ modifyIORef selection
         $ second
         $ const
         $ if (sensor, i) == selectButton then Select else (if (sensor, i) == deselectButton then Deselect else Clear)
      processInput _ _ =
        return ()
    (_, loop) <- consumerLoop kafka processInput
    result <- loop
    either print return result

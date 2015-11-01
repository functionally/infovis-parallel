{-# LANGUAGE RecordWildCards #-}


module Graphics.Rendering.Handa.Viewer (
  ViewerParameters(..)
, viewerGeometry
, laptopViewer
, desktopViewer
, projectorViewer
, fieldOfView
, reshape
, loadViewer
) where


import Data.Default (Default, def)
import Data.IORef (IORef)
import Graphics.Rendering.DLP (DlpEye(..), DlpState, showEye')
import Graphics.Rendering.Handa.Util (degree)
import Graphics.Rendering.OpenGL (GLdouble, MatrixMode(..), Position(..), Size(..), Vector3(..), Vertex3(..), ($=!), loadIdentity, lookAt, matrixMode, perspective, viewport)
import Graphics.UI.GLUT (ReshapeCallback)


data ViewerParameters =
  ViewerParameters
  {
    displayAspectRatio :: GLdouble -- width over height
  , displayThrowRatio  :: GLdouble -- width over distance
  , distanceNearPlane  :: GLdouble
  , distanceFarPlane   :: GLdouble
  , eyePosition        :: Vertex3 GLdouble
  , eyeSeparation      :: Vector3 GLdouble
  , eyeUpward          :: Vector3 GLdouble
  , sceneCenter        :: Vertex3 GLdouble
  }
    deriving (Eq, Read, Show)

instance Default ViewerParameters where
  def =
    ViewerParameters
    {
      displayAspectRatio = 1
    , displayThrowRatio  = 1
    , distanceNearPlane  = 0.5
    , distanceFarPlane   = 4.5
    , eyePosition        = Vertex3 0   0 2
    , eyeSeparation      = Vector3 0.2 0 0
    , eyeUpward          = Vector3 0   1 0
    , sceneCenter        = Vertex3 0   0 0
    }


viewerGeometry :: GLdouble -> GLdouble -> GLdouble -> ViewerParameters
viewerGeometry width height throw =
  def
  {
    displayAspectRatio = width / height
  , displayThrowRatio  = throw / width
  }


laptopViewer :: ViewerParameters
laptopViewer = viewerGeometry 13.625 7.875 24


desktopViewer :: ViewerParameters
desktopViewer = viewerGeometry 20.75 11.625 32


projectorViewer :: ViewerParameters
projectorViewer =
  def
  {
    displayAspectRatio = 1.6 / 1.0
  , displayThrowRatio  = 1.5 / 1.0
  }


fieldOfView :: ViewerParameters -> GLdouble
fieldOfView ViewerParameters{..} = 2 * atan2 0.5 displayThrowRatio * degree


reshape :: ViewerParameters -> ReshapeCallback
reshape vp@ViewerParameters{..} wh@(Size w h) = 
  do
    viewport $=! (Position 0 0, wh)
    matrixMode $=! Projection
    loadIdentity
    perspective (fieldOfView vp) (fromIntegral w / fromIntegral h) distanceNearPlane distanceFarPlane
    matrixMode $=! Modelview 0


loadViewer :: Maybe (IORef DlpState) -> ViewerParameters -> IO ()
loadViewer dlp ViewerParameters{..} =
  do
    loadIdentity
    offset <-
      case dlp of
        Nothing   -> return 0
        Just dlp' -> do
                       isLeft <- showEye' LeftDlp dlp'
                       return $ if isLeft then -1/2 else 1/2
    let
      Vertex3  xEye  yEye  zEye = eyePosition
      Vector3 dxEye dyEye dzEye = eyeSeparation
    lookAt
      (Vertex3 (xEye + offset * dxEye) (yEye + offset * dyEye) (zEye + offset * dzEye))
      sceneCenter
      eyeUpward

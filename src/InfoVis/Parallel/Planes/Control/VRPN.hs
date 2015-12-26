module InfoVis.Parallel.Planes.Control.VRPN (
  trackHead
, trackHeadAndJoystick
) where


import Control.Concurrent (MVar, tryPutMVar)
import Control.Monad (void)
import Data.IORef (IORef)
import Graphics.Rendering.OpenGL (Vector3(..), Vertex3(..), ($=!), get)
import Graphics.UI.GLUT (idleCallback)
import Graphics.UI.SpaceNavigator (Track(..))
import InfoVis.Parallel.Planes.Grid (Resolution)
import Network.VRPN (ButtonCallback, PositionCallback, Device(Button, Tracker), mainLoop, openDevice)


adjustVector :: Num a => (a, a, a) -> (a, a, a)
adjustVector (x, y, z) = (-x, z, y)


adjustQuaternion :: Num a => (a, a, a, a) -> (a, a, a, a)
adjustQuaternion (x, y, z, w) = (-x, z, y, w)


orientEyes :: Num a => (a, a, a, a) -> (a, a, a)
orientEyes (q1, q2, q3, q0) =
  (
    q0 * q0 + q1 * q1 - q2 * q2 - q3 * q3
  , 2 * (  q1 * q2 + q0 * q3)
  , 2 * (- q0 * q2 + q1 * q3)
  )


headCallback :: IORef (Vertex3 Resolution) -> IORef (Vector3 Resolution) -> MVar () -> PositionCallback Int Resolution
headCallback eyes look updated _ _ p o =
  do
    let
      (px, py, pz) = adjustVector p
      (ex, ey, ez) = orientEyes $ adjustQuaternion o
    eyes $=! realToFrac <$> Vertex3 px py pz
    look $=! realToFrac . (* 0.06) <$> Vector3 ex ey ez
    void $ tryPutMVar updated ()


joystickTrackingCallback :: IORef (Track Resolution) -> IORef (Vector3 Resolution) -> Vector3 Resolution -> MVar () -> PositionCallback Int Resolution
joystickTrackingCallback track sceneCenter (Vector3 sx sy sz) updated _ _ p o =
  do
    Vector3 cx cy cz <- get sceneCenter
    let
      (px, py, pz) = adjustVector p
      px' =  px         / sx - cx
      py' =  py         / sy - cy
      pz' = (pz - 0.25) / sz - cz
      (ox, oy, oz, ow) = adjustQuaternion o
      ox' = ox / sx
      oy' = oy / sy
      oz' = oz / sz
      ow' = ow
    track' <- get track
    track $=!
      track'
      {
        trackPosition    = Vector3 px' py' pz'
      , trackOrientation = Vector3
                             (atan2 (2 * (ow' * oz' + ox' * oy')) (1 - 2 * (oy' * oy' + oz' * oz'))) -- yaw
                             (asin  (2 * (ow' * oy' - oz' * ox'))                                  ) -- pitch
                             (atan2 (2 * (ow' * ox' + oy' * oz')) (1 - 2 * (ox' * ox' + oy' * oy'))) -- roll
      }
    void $ tryPutMVar updated ()


joystickButtonCallback :: IORef (Track Resolution) -> MVar () -> ButtonCallback Int
joystickButtonCallback track updated _ button pressed =
  do
    track' <- get track
    case button of
      0 -> track $=! track' {trackLeftPress  = pressed}
      2 -> track $=! track' {trackRightPress = pressed}
      _ -> return ()
    void $ tryPutMVar updated ()


trackHead :: IORef (Vertex3 Resolution) -> IORef (Vector3 Resolution) -> MVar () -> String -> IO ()
trackHead eyes look updated name =
  do
    let
      device :: Device Int Int Int Resolution
      device = Tracker name (Just $ headCallback eyes look updated) Nothing Nothing
    remote <- openDevice device
    idleCallback $=! Just (mainLoop remote)


trackHeadAndJoystick :: IORef (Vertex3 Resolution) -> IORef (Vector3 Resolution) -> IORef (Track Resolution) -> IORef (Vector3 Resolution) -> Vector3 Resolution -> MVar () -> String -> IO ()
trackHeadAndJoystick eyes look tracking sceneCenter sceneScale updated name =
  do
    let
      headDevice :: Device Int Int Int Resolution
      headDevice = Tracker name (Just $ headCallback eyes look updated) Nothing Nothing
      joystickTrackingDevice :: Device Int Int Int Resolution
      joystickTrackingDevice = Tracker "Joystick0@10.60.6.100" (Just $ joystickTrackingCallback tracking sceneCenter sceneScale updated) Nothing Nothing
      joystickButtonDevice :: Device Int Int Int Resolution
      joystickButtonDevice = Button "JoystickA@10.60.6.100:3884" (Just $ joystickButtonCallback tracking updated)
    remotes <- mapM openDevice [headDevice, joystickTrackingDevice, joystickButtonDevice]
    idleCallback $=! Just (mapM_ mainLoop remotes)

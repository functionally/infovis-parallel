module InfoVis.Parallel.Planes.Control.VRPN (
  trackHead
) where


import Control.Concurrent (MVar, tryPutMVar)
import Control.Monad (void)
import Data.IORef (IORef)
import Graphics.Rendering.OpenGL (Vertex3(..), ($=!))
import Graphics.UI.GLUT (idleCallback)
import InfoVis.Parallel.Planes.Grid (Resolution)
import Network.VRPN (PositionCallback, Device(Tracker), mainLoop, openDevice)


headCallback :: IORef (Vertex3 Resolution) -> MVar () -> PositionCallback Int Resolution
headCallback eyes updated _ _ (x, y, z) _ =
  do
    eyes $=! realToFrac <$> Vertex3 (-x) z y
    void $ tryPutMVar updated ()


trackHead :: IORef (Vertex3 Resolution) -> MVar () -> String -> IO ()
trackHead eyes updated name =
  do
    let
      device :: Device Int Int Int Resolution
      device = Tracker name (Just $ headCallback eyes updated) Nothing Nothing
    remote <- openDevice device
    idleCallback $=! Just (mainLoop remote)

module Graphics.UI.Handa.Keyboard (
  keyboardPosition
) where


import Data.IORef (IORef)
import Graphics.Rendering.OpenGL (Vector3(..), ($~!))
import Graphics.UI.GLUT (Key(..), KeyState(..), KeyboardMouseCallback, SpecialKey(..))


keyboardPosition :: Num a => Vector3 a -> IORef (Vector3 a) -> KeyboardMouseCallback
keyboardPosition (Vector3 ix iy iz) location key Down _ _ =
  do
    let
      Vector3 dx dy dz = case key of
        (Char       '+'     ) -> Vector3   0     iy    0
        (Char       '-'     ) -> Vector3   0   (-iy)   0
        (SpecialKey KeyLeft ) -> Vector3 (-ix)   0     0
        (SpecialKey KeyRight) -> Vector3   ix    0     0
        (SpecialKey KeyUp   ) -> Vector3   0     0   (-iz)
        (SpecialKey KeyDown ) -> Vector3   0     0     iz
        _                     -> Vector3   0     0     0
    location $~! \(Vector3 x y z) -> Vector3 (x + dx) (y + dy) (z + dz)
keyboardPosition _ _ _ _ _ _ = return ()

module Linear.Util.Graphics (
  toVector3
, fromVector3
, toVertex3
, fromVertex3
, toRotation
) where


import Data.Math.Util (acosd)
import Graphics.Rendering.OpenGL.GL.CoordTrans (MatrixComponent, rotate)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector3(..), Vertex3(..))
import Linear.Affine (Point(..))
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))


{-# INLINE toVector3 #-}
toVector3 :: V3 a -> Vector3 a
toVector3 (V3 x y z) = Vector3 x y z


{-# INLINE fromVector3 #-}
fromVector3 :: Vector3 a -> V3 a
fromVector3 (Vector3 x y z) = V3 x y z


{-# INLINE toVertex3 #-}
toVertex3 :: Point V3 a -> Vertex3 a
toVertex3 (P (V3 x y z)) = Vertex3 x y z


{-# INLINE fromVertex3 #-}
fromVertex3 :: Vertex3 a -> Point V3 a
fromVertex3 (Vertex3 x y z) = P $ V3 x y z


toRotation :: (Floating a, MatrixComponent a) => Quaternion a -> IO ()
toRotation (Quaternion w (V3 x y z)) = rotate (2 * acosd w) $ Vector3 x y z

module Graphics.OpenGL.Util (
  vertex3
, vector3
, color4
) where


import Graphics.Rendering.OpenGL.GL.Tensor (Vector3(..), Vertex3(..))
import Graphics.Rendering.OpenGL.GL.VertexSpec (Color4(..), ColorComponent, VertexComponent, color, vertex)


-- | Action for a four-component color from a tuple.
color4 :: ColorComponent a => (a, a, a, a) -> IO ()
color4 (r, g, b, a) = color $ Color4 r g b a


-- | Action for a three-component vertex from a tuple.
vertex3 :: VertexComponent a => (a, a, a) -> IO ()
vertex3 (x, y, z) = vertex $ Vertex3 x y z


-- | Construct a three-component vector from a tuple.
vector3 :: (a, a, a) -> Vector3 a
vector3 (x, y, z) = Vector3 x y z

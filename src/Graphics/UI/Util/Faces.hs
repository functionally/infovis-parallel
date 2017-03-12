module Graphics.UI.Util.Faces (
  -- * Types
  Face
, Edges
, faceToEdges
  -- * Rendering
, drawFaces
, drawFrame
  -- * Particular Objects
, coneFaces
, brickFaces
) where


import Data.Math.Util (cosd, sind)
import Graphics.UI.Util (vertex3)
import Graphics.Rendering.OpenGL.GL (PrimitiveMode(..), VertexComponent, renderPrimitive)


-- | Edges are paired series of triple of vertex components.
type Edges a = [(a, a, a)]


-- | A face is a collections of the vertices on its boundary.
type Face a = [(a, a, a)]


-- | Convert a face to a
faceToEdges :: Face a -> Edges a
faceToEdges face = concat $ zipWith ((. return) . (:)) (init face) (tail face)


-- | Draw faces.
drawFaces :: (Num a, VertexComponent a)
          => [Face a] -- ^ The faces.
          -> IO ()    -- ^ An action to draw the faces.
drawFaces = renderPrimitive Quads . mapM_ vertex3 . concat


-- | Draw the frame around faces.
drawFrame :: (Num a, VertexComponent a)
          => [Face a] -- ^ The faces.
          -> IO ()    -- ^ An action to draw the frame of the faces' edges.
drawFrame = renderPrimitive Lines . mapM_ vertex3 . concatMap faceToEdges


-- | The faces of a truncated cone that has its tip at the origin and points towards the negative z axis.
coneFaces :: (Enum a, Floating a, VertexComponent a)
          => Int      -- ^ The number of faces around the cone.
          -> a        -- ^ The height of the cone.
          -> a        -- ^ The radius of the cone.
          -> [Face a] -- ^ Faces representing the cone.
coneFaces n height radius =
  let
    point = (0, 0, 0)
    center = (0, 0, height)
    rim = [(radius * cosd x, radius * sind x, height) | x <- [0,(360 / fromIntegral n)..360]]
  in 
    [
      [p0, p1, p2]
    |
      (p1, p2) <- zip (tail rim) (init rim)
    , p0 <- [point, center]
    ]


-- | The faces of a brick that has one corner at the origin.
brickFaces :: Num a
           => a        -- ^ The x (width) dimension of the brick.
           -> a        -- ^ The y (height) dimension of the brick.
           -> a        -- ^ The z (depth) dimension of the brick.
           -> [Face a] -- ^ Faces representing the brick.
brickFaces w h d =
  [[(0, 0, z), (w, 0, z), (w, h, z), (0, h, z)] | z <- [0, d]]
  ++
  [[(0, y, 0), (w, y, 0), (w, y, d), (0, y, d)] | y <- [0, h]]
  ++
  [[(x, 0, 0), (x, h, 0), (x, h, d), (x, 0, d)] | x <- [0, w]]

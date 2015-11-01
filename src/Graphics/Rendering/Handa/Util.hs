module Graphics.Rendering.Handa.Util (
  degree
, cosd
, sind
, color4
, vertex3
, vector3
, Face
, faceToEdges
, drawFaces
, drawFrame
, coneFaces
, brickFaces
) where


import Graphics.Rendering.OpenGL.GL (Color4(..), ColorComponent, PrimitiveMode(..), Vector3(..), Vertex3(..), VertexComponent, color, renderPrimitive, vertex)
--port Graphics.Rendering.OpenGL.GL


degree :: Floating a => a
degree = 180 / pi


cosd :: Floating a => a -> a
cosd = cos . (/ degree)


sind :: Floating a => a -> a
sind = sin . (/ degree)


color4 :: ColorComponent a => (a, a, a, a) -> IO ()
color4 (r, g, b, a) = color $ Color4 r g b a


vertex3 :: VertexComponent a => (a, a, a) -> IO ()
vertex3 (x, y, z) = vertex $ Vertex3 x y z


vector3 :: (a, a, a) -> Vector3 a
vector3 (x, y, z) = Vector3 x y z


type Face a = [(a, a, a)]


faceToEdges :: Face a -> [(a, a, a)]
faceToEdges face = concat $ zipWith ((. return) . (:)) (init face) (tail face)


drawFaces :: (Num a, VertexComponent a) => [Face a] -> IO ()
drawFaces = renderPrimitive Quads . mapM_ vertex3 . concat


drawFrame :: (Num a, VertexComponent a) => [Face a] -> IO ()
drawFrame = renderPrimitive Lines . mapM_ vertex3 . concatMap faceToEdges


coneFaces :: (Enum a, Floating a, VertexComponent a) => a -> a -> [Face a]
coneFaces height radius =
  let
    point = (0, 0, 0)
    center = (0, 0, height)
    rim = [(radius * cosd x, radius * sind x, height) | x <- [0,15..360]]
  in 
    [
      [p0, p1, p2]
    |
      (p1, p2) <- zip (tail rim) (init rim)
    , p0 <- [point, center]
    ]


brickFaces :: Num a => a -> a -> a -> [Face a]
brickFaces w h d =
  [[(0, 0, z), (w, 0, z), (w, h, z), (0, h, z)] | z <- [0, d]]
  ++
  [[(0, y, 0), (w, y, 0), (w, y, d), (0, y, d)] | y <- [0, h]]
  ++
  [[(x, 0, 0), (x, h, 0), (x, h, d), (x, 0, d)] | x <- [0, w]]

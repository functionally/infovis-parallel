module InfoVis.Parallel.Planes.Grid (
  drawSelector
, drawPlanes
) where


import Graphics.Rendering.Handa.Util (brickFaces, color4, coneFaces, drawFaces, drawFrame, vector3)
import Graphics.Rendering.OpenGL (GLfloat, Vector3(..), preservingMatrix, translate)


drawSelector :: IO ()
drawSelector =
  do
    let
      radius = 0.03 :: GLfloat
      height = 4 * radius
      coneOpacity = 1 :: GLfloat
    color4 (0.5, 0.3, 0.2, coneOpacity)
    translate $ Vector3 0 0 (- height)
    drawFaces $ coneFaces height radius


drawPlanes :: Int -> Int -> IO ()
drawPlanes planes divisions =
  do
    let
      spacing = 2 / fromIntegral (planes - 1) :: GLfloat
      size    = 2 / fromIntegral divisions    :: GLfloat
      frameGray = 0.4 :: GLfloat
      faceGray  = 0.2 :: GLfloat
      frameOpacity = 0.4 :: GLfloat
      faceOpacity  = 0.2 :: GLfloat
      thickness = size / 20
      faces = brickFaces thickness size size
      drawBrick =
        do
          color4 (faceGray, faceGray, faceGray, faceOpacity)
          drawFaces faces
          color4 (frameGray, frameGray, frameGray, frameOpacity)
          drawFrame faces
    sequence_
      [
        preservingMatrix $ do
          translate $ vector3 ((fromIntegral i - 1) * spacing - 1, (fromIntegral j - 1) * size - 1, (fromIntegral k - 1) * size)
          drawBrick
      |
        i <- [1..planes]
      , j <- [1..divisions]
      , k <- [1..divisions]
      ]

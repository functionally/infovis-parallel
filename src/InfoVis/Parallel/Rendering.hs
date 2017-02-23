{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Rendering (
  makeBuffer
, drawBuffer
) where


import Data.Array.Storable (newListArray, withStorableArray)
import Data.Word (Word8)
import Foreign.C.Types (CPtrdiff)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.Storable (Storable, sizeOf)
import Graphics.Rendering.OpenGL (DataType, BufferObject, BufferTarget(ArrayBuffer), BufferUsage(StaticDraw), Capability(..), ClientArrayType(VertexArray), Color4, GLfloat, NumArrayIndices, NumComponents, PrimitiveMode, Stride, Vertex3, VertexArrayDescriptor(..), ($=), arrayPointer, bindBuffer, bufferData, drawArrays, clientState, genObjectNames)

--import Graphics.Rendering.Handa.Shape (Shape, drawShape, makeShape)
import Graphics.Rendering.OpenGL (DataType(Float), color)
import InfoVis.Parallel.Types (Color)
import InfoVis.Parallel.Types.Display (DisplayList(..))
import InfoVis.Parallel.Types.Scaffold (Characteristic(..))
import Graphics.Rendering.OpenGL.GL.BufferObjects
import Graphics.Rendering.OpenGL.GL.VertexArrays


type OffsetStride = (Int, Stride)


data Shape =
  Shape
  {
    bo       :: BufferObject    -- ^ The handle to the vertex buffer object.
  , osv      :: OffsetStride
  , osc      :: OffsetStride
  , size     :: NumArrayIndices
  , dataType :: DataType        -- ^ The data type of the shape's vertices.
  , mode     :: PrimitiveMode   -- ^ The type of primitive.
  }


makeShape :: (Storable a, Storable b)
          => DataType      -- ^ The data type for the vertices' components.
          -> PrimitiveMode -- ^ The type of primitive.
          -> [(Vertex3 a, Color4 b)]       -- ^ The colors.
          -> IO Shape      -- ^  An action for the shape.
makeShape dataType mode verticesColors =
  do
    [bo] <- genObjectNames 1
    let
      (vs, cs) = unzip verticesColors
      n = length verticesColors
      mv =
        if null vs
          then 0
          else sizeOf $ head vs
      mc =
        if null cs
          then 0
          else sizeOf $ head cs
      nequiv = n * mv `div` mc + 1
      size = toEnum n
      osv = (0, toEnum mv)
      osc = (toEnum $ nequiv * mc, toEnum mc)
    print (n, mv, mc, nequiv)
    vs' <- newListArray (0, n - 1) vs
    cs' <- newListArray (0, n - 1) cs
    bindBuffer ArrayBuffer $= Just bo
    zeros <- newListArray (0, (nequiv + n) * mc `div` 4) $ (repeat 0.8 :: [GLfloat])
    withStorableArray zeros $ \ptr -> bufferData ArrayBuffer $= (toEnum $ (nequiv + n) * mc, ptr, StaticDraw)
    withStorableArray vs' $ bufferSubData ArrayBuffer WriteToBuffer 0 (toEnum $ n * mv)
    withStorableArray cs' $ bufferSubData ArrayBuffer WriteToBuffer (toEnum $ nequiv * mc)  (toEnum $ n * mc)
    bindBuffer ArrayBuffer $= Nothing
    return Shape {..}


drawShape :: Shape -- ^ The shape.
          -> IO () -- ^ An action to render the shape, also executing its prior actions before rendering it.
drawShape Shape{..} =
  do
    clientState VertexArray $= Enabled
    clientState ColorArray $= Enabled
    bindBuffer ArrayBuffer $= Just bo
    arrayPointer VertexArray $= VertexArrayDescriptor 3 dataType (snd osv) nullPtr
    arrayPointer ColorArray  $= VertexArrayDescriptor 4 dataType (snd osc) (nullPtr `plusPtr` fst osc)
    drawArrays mode 0 size
    bindBuffer ArrayBuffer $= Nothing
    clientState ColorArray $= Disabled
    clientState VertexArray $= Disabled


data DisplayBuffer a b =
  DisplayBuffer
  {
    bufferIdentifier        :: a
  , bufferVertexIdentifiers :: [b]
  , bufferShape             :: Shape
  }


makeBuffer :: DisplayList a b -> IO (DisplayBuffer a b)
makeBuffer DisplayList{..} =
  do
    let
      bufferIdentifier = listIdentifier
      bufferVertexIdentifiers = listVertexIdentifiers
    bufferShape <-
      makeShape Float listPrimitive
        $ zip listVertices
        $ repeat
        $ normalColor
        $ head listCharacteristics
    return DisplayBuffer{..}


drawBuffer :: DisplayBuffer a b -> IO ()
drawBuffer DisplayBuffer{..} =
  drawShape bufferShape

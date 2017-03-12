{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Rendering.Shapes (
  DisplayBuffer(bufferIdentifier)
, makeBuffer
, freeBuffer
, updateBuffer
, drawBuffer
) where



import Control.Arrow ((&&&))
import Data.Array.Storable (newListArray, withStorableArray)
import Data.Map.Strict as M (Map, (!), fromList, lookup)
import Data.Maybe (catMaybes)
import Data.Word (Word8)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.Storable (Storable, poke, sizeOf)
import Graphics.Rendering.OpenGL.GL (($=), deleteObjectName, genObjectName)
import Graphics.Rendering.OpenGL.GL.BufferObjects (BufferAccess(..), BufferObject, BufferTarget(..), BufferUsage(StaticDraw), TransferDirection(WriteToBuffer), bindBuffer, bufferData, bufferSubData, withMappedBuffer)
import Graphics.Rendering.OpenGL.GL.PrimitiveMode (PrimitiveMode)
import Graphics.Rendering.OpenGL.GL.Tensor (Vertex3)
import Graphics.Rendering.OpenGL.GL.VertexArrays (Capability(..), ClientArrayType(..), DataType(Float), NumArrayIndices, VertexArrayDescriptor(..), arrayPointer, clientState, drawArrays)
import Graphics.Rendering.OpenGL.GL.VertexSpec (Color4)
import InfoVis.Parallel.Rendering.Types (DisplayList(..))
import InfoVis.Parallel.Types (Color, Coloring(..))
import InfoVis.Parallel.Types.Presentation (Characteristic(..))


data Shape =
  Shape
  {
    bufferObject :: BufferObject    -- ^ The handle to the vertex buffer object.
  , vertexOffset :: Int
  , vertexStride :: Int
  , colorOffset  :: Int
  , colorStride  :: Int
  , size         :: NumArrayIndices
  , dataType     :: DataType        -- ^ The data type of the shape's vertices.
  , mode         :: PrimitiveMode   -- ^ The type of primitive.
  }


makeShape :: (Storable a, Storable b)
          => DataType      -- ^ The data type for the vertices' components.
          -> PrimitiveMode -- ^ The type of primitive.
          -> [(Vertex3 a, Color4 b)]       -- ^ The colors.
          -> IO Shape      -- ^  An action for the shape.
makeShape dataType mode verticesColors =
  do
    bufferObject <- genObjectName
    let
      analyze xs = id &&& toEnum $ if null xs then 0 else sizeOf (head xs)
      (vertices, colors) = unzip verticesColors
      (mVertices, vertexStride) = analyze vertices
      (mColors, colorStride) = analyze colors
      n = length verticesColors
      size = toEnum n
      nEquiv = n * mVertices `div` mColors + 1
      vertexOffset = 0
      colorOffset = toEnum $ nEquiv * mColors
    bindBuffer ArrayBuffer $= Just bufferObject
    zeros <- newListArray (0, (nEquiv + n) * mColors) (repeat 0 :: [Word8])
    withStorableArray zeros $ \ptr -> bufferData ArrayBuffer $= (toEnum $ (nEquiv + n) * mColors, ptr, StaticDraw)
    writeSubbuffer vertexOffset vertexStride n vertices
    writeSubbuffer colorOffset colorStride n colors
    bindBuffer ArrayBuffer $= Nothing
    return Shape {..}


freeShape :: Shape -> IO ()
freeShape Shape{..} = deleteObjectName bufferObject


updateColors :: Storable a
             => Shape
             -> [(Int, Color4 a)]       -- ^ The color changes.
             -> IO ()
updateColors Shape{..} changes =
  do
    bindBuffer ArrayBuffer $= Just bufferObject
    withMappedBuffer ArrayBuffer WriteOnly
      (
        \ptr ->
          sequence_
            [
              (ptr `plusPtr` i') `poke` c
            |
              (i, c) <- changes
            , let i' = colorOffset + i * colorStride
            ]
      )
      print
    bindBuffer ArrayBuffer $= Nothing


writeSubbuffer :: Storable a => Int -> Int -> Int -> [a] -> IO ()
writeSubbuffer o s n xs =
  do
    xs' <- newListArray (0, n - 1) xs
    withStorableArray xs'
      $ bufferSubData ArrayBuffer WriteToBuffer (fromIntegral o) (fromIntegral $ n * s)


drawShape :: Shape -- ^ The shape.
          -> IO () -- ^ An action to render the shape, also executing its prior actions before rendering it.
drawShape Shape{..} =
  do
    clientState VertexArray  $= Enabled
    clientState ColorArray   $= Enabled
    bindBuffer ArrayBuffer   $= Just bufferObject
    arrayPointer VertexArray $= VertexArrayDescriptor 3 dataType (fromIntegral vertexStride) (nullPtr `plusPtr` vertexOffset)
    arrayPointer ColorArray  $= VertexArrayDescriptor 4 dataType (fromIntegral colorStride ) (nullPtr `plusPtr` colorOffset )
    drawArrays mode 0 size
    bindBuffer ArrayBuffer   $= Nothing
    clientState ColorArray   $= Disabled
    clientState VertexArray  $= Disabled


data DisplayBuffer a b =
  DisplayBuffer
  {
    bufferIdentifier        :: a
  , bufferVertexIdentifiers :: [b]
  , bufferShape             :: Shape
  , bufferColorings         :: Map Coloring Color
  }


makeBuffer :: DisplayList a b -> IO (DisplayBuffer a b)
makeBuffer DisplayList{..} =
  do
    let
      bufferIdentifier = listIdentifier
      bufferVertexIdentifiers = listVertexIdentifiers
      ColorSet{..} = head listCharacteristics
      bufferColorings =
        M.fromList
          [
            (NormalColoring   , normalColor)
          , (SelectColoring   , selectColor)
          , (HighlightColoring, highlightColor)
          ]
    bufferShape <-
      makeShape Float listPrimitive
        $ zip listVertices
        $ repeat normalColor
    return DisplayBuffer{..}


freeBuffer :: DisplayBuffer a b -> IO ()
freeBuffer DisplayBuffer{..} = freeShape bufferShape


updateBuffer :: Ord b => DisplayBuffer a b -> [(b, Coloring)] -> IO ()
updateBuffer DisplayBuffer{..} updates =
  let
    updates' = (bufferColorings !) <$> M.fromList updates
  in
    updateColors bufferShape
      . catMaybes
      $ zipWith (\i v -> (i, ) <$> v `M.lookup` updates') [0..] bufferVertexIdentifiers


drawBuffer :: DisplayBuffer a b -> IO ()
drawBuffer DisplayBuffer{..} =
  drawShape bufferShape

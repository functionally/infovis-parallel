{-# LANGUAGE RecordWildCards #-}


module Graphics.Rendering.Handa.Shape (
  Shape
, makeShape
, remakeShape
, drawShape
) where


import Control.Applicative ((<$>))
import Data.Array.Storable (newListArray, withStorableArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable, sizeOf)
import Graphics.Rendering.OpenGL (DataType, BufferObject, BufferTarget(ArrayBuffer), BufferUsage(StaticDraw), Capability(..), ClientArrayType(VertexArray), NumArrayIndices, NumComponents, PrimitiveMode, Stride, VertexArrayDescriptor(..), ($=), arrayPointer, bindBuffer, bufferData, drawArrays, clientState, genObjectNames)



data Shape =
  Shape
  {
    vbo         :: BufferObject
  , components  :: NumComponents
  , dataType    :: DataType
  , stride      :: Stride
  , size        :: NumArrayIndices
  , mode        :: PrimitiveMode
  , priorAction :: IO ()
  }


makeShape :: Storable a => NumComponents -> DataType -> PrimitiveMode -> [a] -> IO () -> IO Shape
makeShape components dataType mode vertices priorAction =
  let
    stride = undefined
    size = undefined
  in do
    vbo <- head <$> genObjectNames 1
    remakeShape Shape{..} vertices


remakeShape :: Storable a => Shape -> [a] -> IO Shape
remakeShape shape@Shape{..} vertices =
  let
    n = length vertices
    m =
      if null vertices
        then 0
        else sizeOf $ head vertices
    ptrSize = toEnum $ n * m
  in do
    bindBuffer ArrayBuffer $= Just vbo
    vertices' <- newListArray (0, n - 1) vertices
    withStorableArray vertices' $ \ptr -> bufferData ArrayBuffer $= (ptrSize, ptr, StaticDraw)
    bindBuffer ArrayBuffer $= Nothing
    return $ shape {size = toEnum n, stride = toEnum m}

    
drawShape :: Shape -> IO ()
drawShape Shape{..} =
  do
    clientState VertexArray $= Enabled
    bindBuffer ArrayBuffer $= Just vbo
    arrayPointer VertexArray $= VertexArrayDescriptor components dataType stride nullPtr
    priorAction
    drawArrays mode 0 size
    bindBuffer ArrayBuffer $= Nothing
    clientState VertexArray $= Disabled

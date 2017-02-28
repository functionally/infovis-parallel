{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module InfoVis.Parallel.Types.Display (
  Primitive3D
, DisplayItem(..)
, DisplayList(..)
, DisplayType(..)
) where


import Data.Binary (Binary(..), getWord8, putWord8)
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL (GLfloat, PrimitiveMode(..), Vertex3)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import InfoVis.Parallel.Types.Scaffold (Characteristic)


type Primitive3D = Vertex3 GLfloat


data DisplayItem a b =
  DisplayItem
  {
    itemIdentifier :: a
  , itemPrimitive  :: PrimitiveMode
  , itemVertices   :: [b]
  }
    deriving (Eq, Generic, Show)

instance Functor (DisplayItem a) where
  fmap f x@DisplayItem{..} = x {itemVertices = f <$> itemVertices}


data DisplayList a b =
  DisplayList
  {
    listIdentifier        :: a
  , listCharacteristics   :: [Characteristic]
  , listVertexIdentifiers :: [b]
  , listPrimitive         :: PrimitiveMode
  , listVertices          :: [Primitive3D]
  }
    deriving (Binary, Eq, Generic, Show)


instance Binary PrimitiveMode where
  get = do
          i <- getWord8
          return
            $ case i of
              0  -> Points
              1  -> Lines        
              2  -> LineLoop     
              3  -> LineStrip    
              4  -> Triangles    
              5  -> TriangleStrip
              6  -> TriangleFan  
              7  -> Quads        
              8  -> QuadStrip    
              9  -> Polygon      
              10 -> Patches
              _  -> error "Unknown serialization of PrimitiveMode."
  put Points        = putWord8 0
  put Lines         = putWord8 1
  put LineLoop      = putWord8 2
  put LineStrip     = putWord8 3
  put Triangles     = putWord8 4
  put TriangleStrip = putWord8 5
  put TriangleFan   = putWord8 6
  put Quads         = putWord8 7
  put QuadStrip     = putWord8 8
  put Polygon       = putWord8 9
  put Patches       = putWord8 10


data DisplayType =
    GridType
  | LinkType
    deriving (Binary, Eq, Generic, Show)

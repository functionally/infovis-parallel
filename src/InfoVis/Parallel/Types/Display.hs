{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Types.Display (
  Primitive3D
, DisplayItem(..)
, DisplayList(..)
, DisplayType(..)
) where


import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL (GLfloat, PrimitiveMode, Vertex3)
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
    deriving (Eq, Generic, Show)


data DisplayType =
    GridType
  | LinkType
    deriving (Eq, Generic, Show)

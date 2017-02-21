{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Types.Display (
  Primitive3D
, DisplayItem(..)
, DisplayList(..)
) where


import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL (GLfloat, PrimitiveMode, Vertex3)


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
    displayIdentifier :: a
  , vertexIdentifiers :: [b]
  , vertices          :: [Primitive3D]
  }
    deriving (Eq, Generic, Show)

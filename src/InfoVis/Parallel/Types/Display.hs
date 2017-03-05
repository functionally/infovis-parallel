{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Types.Display (
  Primitive3D
, DisplayItem(..)
, DisplayList(..)
, DisplayType(..)
) where


import Data.Binary (Binary)
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
    deriving (Eq, Generic, Ord, Show)

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
    deriving (Binary, Eq, Generic, Ord, Show)


data DisplayType =
    GridType
  | LinkType
    deriving (Binary, Eq, Generic, Ord, Show)

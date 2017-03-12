{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Rendering.Types (
  Primitive3D
, DisplayItem(..)
, DisplayList(..)
, DisplayType(..)
, DisplayText(..)
) where


import Data.Binary (Binary)
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL (GLfloat, PrimitiveMode(..), Vertex3)
import InfoVis.Parallel.Types (Color)
import InfoVis.Parallel.Types.Presentation (Characteristic)


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


data DisplayText a b =
  DisplayText
  {
    textContent :: a
  , textOrigin  :: b
  , textWidth   :: b
  , textHeight  :: b
  , textSize    :: Double
  , textColor   :: Color
  }
    deriving (Binary, Eq, Generic, Ord, Show)

instance Functor (DisplayText a) where
  fmap f x@DisplayText{..} = x {textOrigin = f textOrigin, textWidth = f textWidth, textHeight = f textHeight}

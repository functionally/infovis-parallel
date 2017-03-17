{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Rendering.Types (
  Primitive3D
, fromLocation
, fromLocations
, PrimitiveMode(..)
, DisplayItem(..)
, DisplayList(..)
, DisplayType(..)
, DisplayText(..)
, inBox
) where


import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL (GLfloat)
import Graphics.Rendering.OpenGL.GL.PrimitiveMode (PrimitiveMode(..))
import Graphics.Rendering.OpenGL.GL.Tensor (Vertex3(..))
import InfoVis.Parallel.Types (Color, Location)
import InfoVis.Parallel.Types.Presentation (Characteristic)
import Linear.Affine (Point(..))
import Linear.V3 (V3(..))


type Primitive3D = Vertex3 GLfloat


fromLocation :: Location -> Primitive3D
fromLocation (P (V3 x y z)) = realToFrac <$> Vertex3 x y z


fromLocations :: [DisplayItem a Location] -> [DisplayItem a Primitive3D]
fromLocations = fmap (fmap fromLocation)


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
    deriving (Binary, Eq, Generic, NFData, Ord, Show)


data DisplayType =
    GridType
  | LinkType
    deriving (Binary, Eq, Generic, NFData, Ord, Show)


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
    deriving (Binary, Eq, Generic, NFData, Ord, Show)

instance Functor (DisplayText a) where
  fmap f x@DisplayText{..} = x {textOrigin = f textOrigin, textWidth = f textWidth, textHeight = f textHeight}


{-# INLINE inBox #-}
inBox :: (Num a, Ord a) => a -> Vertex3 a -> Vertex3 a -> Bool
inBox d (Vertex3 x y z) (Vertex3 x' y' z') =
  h x x' + h y y' + h z z' <= d^(2::Int)
    where
      h w w' = (w - w')^(2::Int)

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module InfoVis.Parallel.Types.Presentation (
  Axis(..)
, Axes1D
, Axes2D
, Axes3D
, GridAlias
, Grid(..)
, Extent(..)
, Extended(..)
, Container(..)
, LinkAlias
, Link(..)
, ColorSet(..)
, Label(..)
, Selector(..)
, Status(..)
, Presentation(..)
) where


import Control.DeepSeq (NFData)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Binary (Binary)
import GHC.Generics (Generic)
import InfoVis.Parallel.Types (Alias, Color, Location, Offset, Orientation, VariableAlias)
import Linear.Util.Instances()
import Linear.V1 (V1)
import Linear.V2 (V2)
import Linear.V3 (V3)


{-# ANN module "HLint: ignore Use newtype instead of data" #-}


type GridAlias = Alias


data Axis =
  Axis
  {
    axisVariable :: VariableAlias
  }
  deriving (Binary, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)


type Axes1D = V1 Axis


type Axes2D = V2 Axis


type Axes3D = V3 Axis


data Grid =
    LineGrid
    {
      gridAlias  :: GridAlias
    , axes1D     :: Axes1D
    , divisions  :: Int
    , lineColors :: ColorSet
    , label      :: Label
    }
  | RectangleGrid
    {
      gridAlias  :: GridAlias
    , axes2D     :: Axes2D
    , divisions  :: Int
    , lineColors :: ColorSet 
    , faceColors :: ColorSet
    , label      :: Label
    }
  | BoxGrid
    {
      gridAlias  :: GridAlias
    , axes3D     :: Axes3D
    , divisions  :: Int
    , lineColors :: ColorSet
    , faceColors :: ColorSet
    , label      :: Label
    }
    deriving (Binary, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)


data Extent =
    Extent1D
    {
      origin  :: Location
    , cornerX :: Location
    }
  | Extent2D
    {
      origin  :: Location
    , cornerX :: Location
    , cornerY :: Location
    }
  | Extent3D
    {
      origin  :: Location
    , cornerX :: Location
    , cornerY :: Location
    , cornerZ :: Location
    }
    deriving (Binary, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)


data Extended a =
  Extended
  {
    extent :: Extent
  , grid   :: a
  }
    deriving (Binary, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)


data Container = -- FIXME: The dimensionality between extents and grids is not enforced to be consistent.  Can this be easily done at the type level?
    Singleton
    {
      singletonContains :: Extended Grid
    }
  | Array
    {
      arrayContains :: [Extended Grid]
    }
  | Collection
    {
      collectionContains :: [Extended Container]
    }
    deriving (Binary, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)
    

type LinkAlias = Alias


data Link =
    Point
    {
      linkAlias  :: LinkAlias
    , linkGrid   :: GridAlias
    , linkColors :: ColorSet
    }
  | Polyline
    {
      linkAlias  :: LinkAlias
    , linkGrids  :: [GridAlias]
    , linkColors :: ColorSet
    }
    deriving (Binary, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)


data ColorSet =
    ColorSet
    {
      normalColor    :: Color
    , selectColor    :: Color
    , highlightColor :: Color
    , hiddenColor    :: Color
    }
    deriving (Binary, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)


data Selector =
  Selector
  {
    selectorLocation    :: Location
  , selectorSize        :: Double
  , selectorOrientation :: Orientation
  , selectorColor       :: Color
  }
    deriving (Binary, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)


data Label =
  Label
  {
    labelColor :: Color
  , labelSize  :: Double
  }
    deriving (Binary, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)


data Status =
  Status
  {
    statusOrigin :: Location
  , statusWidth  :: Offset
  , statusHeight :: Offset
  , statusSize   :: Double
  , statusColor  :: Color
  }
    deriving (Binary, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)


data Presentation =
  Presentation
  {
    containers   :: [Container]
  , links        :: [Link]
  , animationKey :: Maybe VariableAlias
  , uniqueKey    :: Maybe VariableAlias
  , selector     :: Selector
  , status       :: Status
  }
    deriving (Binary, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)


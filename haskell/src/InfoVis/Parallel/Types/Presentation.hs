{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module InfoVis.Parallel.Types.Presentation (
  Axis(..)
, Axes1D
, Axes2D
, Axes3D
, GridAlias
, Grid(..)
, GriddedLocation
, Extent(..)
, Container(..)
, LinkAlias
, Link(..)
, TimeAlias
, Characteristic(..)
, Presentation(..)
) where


import Control.DeepSeq (NFData)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Graphics.OpenGL.Util.Instances ()
import InfoVis.Parallel.Types (Color, Location)
import InfoVis.Parallel.Types.Dataset (VariableAlias)
import Linear.Util.Instances ()
import Linear.V1 (V1)
import Linear.V2 (V2)
import Linear.V3 (V3)


{-# ANN module "HLint: ignore Use newtype instead of data" #-}


type GridAlias = String


data Axis =
  Axis
  {
    axisVariable :: VariableAlias
  }
  deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)


type Axes1D = V1 Axis


type Axes2D = V2 Axis


type Axes3D = V3 Axis


data Grid =
    LineGrid
    {
      gridAlias           :: GridAlias
    , axes1D              :: Axes1D
    , divisions           :: Int
    , lineCharacteristics :: [Characteristic]
    , labelColor          :: Color
    , labelSize           :: Double
    }
  | RectangleGrid
    {
      gridAlias           :: GridAlias
    , axes2D              :: Axes2D
    , divisions           :: Int
    , lineCharacteristics :: [Characteristic]
    , faceCharacteristics :: [Characteristic]
    , labelColor          :: Color
    , labelSize           :: Double
    }
  | BoxGrid
    {
      gridAlias           :: GridAlias
    , axes3D              :: Axes3D
    , divisions           :: Int
    , lineCharacteristics :: [Characteristic]
    , faceCharacteristics :: [Characteristic]
    , labelColor          :: Color
    , labelSize           :: Double
    }
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)


type GriddedLocation = (GridAlias, Location)


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
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)


data Container = -- FIXME: The dimensionality between extents and grids is not enforced to be consistent.  Can this be easily done at the type level?
    Singleton
    {
      extent  :: Extent
    , grid    :: Grid
    }
  | Array
    {
      extents :: [Extent]
    , grids   :: [Grid]
    }
  | Collection
    {
      extents    :: [Extent]
    , containeds :: [Container]
    }
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)
    

type LinkAlias = String


data Link =
    Point
    {
      linkAlias       :: LinkAlias
    , linkedGrid      :: GridAlias
    , characteristics :: [Characteristic]
    }
  | Polyline
    {
      linkAlias       :: LinkAlias
    , linkedGrids     :: [GridAlias]
    , characteristics :: [Characteristic]
    }
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)


type TimeAlias = String


data Characteristic =
    ColorSet
    {
      normalColor     :: Color
    , selectColor     :: Color
    , highlightColor  :: Color
    }
    deriving (Binary, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)


data Presentation =
  Presentation
  {
    containers    :: [Container]
  , links         :: [Link]
  , animationKey  :: Maybe VariableAlias
  , uniqueKey     :: Maybe VariableAlias
  , selectorColor :: Color
  , selectorSize  :: Double
  , statusColor   :: Color
  , statusSize    :: Double
  , statusOrigin  :: Location
  , statusWidth   :: Location
  , statusHeight  :: Location
  }
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)
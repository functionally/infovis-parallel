{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}


module InfoVis.Parallel.Types (
  Location
, Offset
, Orientation
, Color
, Alias
, VariableAlias
, Coloring(..)
) where


import Control.DeepSeq (NFData)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Linear.Affine (Point(..))
import Linear.V3 (V3)
import Linear.V4 (V4)


type Alias = String


type VariableAlias = Alias


type Location = Point V3 Double


type Offset = V3 Double


type Orientation = ()


type Color = V4 Double


data Coloring =
    NormalColoring
  | SelectColoring
  | HighlightColoring
  deriving (Binary, Bounded, Enum, Eq, FromJSON, Generic, NFData, Ord, Read, Show, ToJSON)


{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}


module InfoVis.Parallel.Types (
  SelectionAction(..)
, Location
, Color
, Coloring(..)
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Linear.Affine (Point(..))
import Linear.V3 (V3)
import Graphics.Rendering.OpenGL (Color4(..), GLfloat)


data SelectionAction = Highlight | Select | Deselect | Clear
 deriving (Binary, Eq, Generic)


type Location = Point V3 Double


type Color = Color4 GLfloat

data Coloring =
    NormalColoring
  | SelectColoring
  | HighlightColoring
  deriving (Binary, Bounded, Enum, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

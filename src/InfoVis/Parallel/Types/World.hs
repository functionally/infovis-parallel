{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module InfoVis.Parallel.Types.World (
  WorldExtent(..)
, World(..)
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import InfoVis.Parallel.Types (Location)
import Linear.Util.Instances ()
import Linear.V3 (V3)


data WorldExtent =
  WorldExtent
  {
    worldOrigin  :: Location
  , worldCornerX :: Location
  , worldCornerY :: Location
  , worldCornerZ :: Location
  }
    deriving (Binary, Eq, FromJSON, Generic, Hashable, Ord, Read, Show, ToJSON)


data World =
  World
  {
    displayExtent  :: WorldExtent
  , worldExtent    :: WorldExtent
  , selectorOffset :: V3 Double
  , baseSize       :: Double      -- FIXME: Needs implementation.
  }
    deriving (Binary, Eq, FromJSON, Generic, Hashable, Ord, Read, Show, ToJSON)

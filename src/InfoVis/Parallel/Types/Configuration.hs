{-# LANGUAGE DeriveGeneric #-}


module InfoVis.Parallel.Types.Configuration (
  Configuration(..)
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)
import InfoVis.Parallel.Types.Dataset (Dataset)
import InfoVis.Parallel.Types.Scaffold (Presentation, World)


data Configuration =
  Configuration
  {
    dataset      :: Dataset
  , presentation :: Presentation
  , world        :: World
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Configuration

instance ToJSON Configuration

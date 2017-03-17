{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module InfoVis.Parallel.Types.Dataset (
  Dataset(..)
, VariableAlias
, Variable(..)
, RecordIdentifier
, Record
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Binary (Binary)
import GHC.Generics (Generic)


data Dataset =
  Dataset
  {
    datasetIdentifier :: String
  , variables         :: [Variable]
  }
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)


data Variable =
  ContinuousVariable
  {
    variableAlias :: VariableAlias
  , variableName  :: String
  , units         :: Maybe String
  , lowerBound    :: Maybe Double
  , upperBound    :: Maybe Double
  , jitter        :: Maybe Double -- FIXME: Needs implementation.
  }
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)


type VariableAlias = String


type RecordIdentifier = Int


type Record = [Double]

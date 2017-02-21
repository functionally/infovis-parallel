{-# LANGUAGE DeriveGeneric #-}


module InfoVis.Parallel.Types.Dataset (
  Dataset(..)
, VariableAlias
, Variable(..)
, RecordIdentifier
, Record
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Word (Word64)
import GHC.Generics (Generic)


data Dataset =
  Dataset
  {
    datasetIdentifier :: String
  , variables         :: [Variable]
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Dataset

instance ToJSON Dataset


data Variable =
  ContinuousVariable
  {
    variableAlias :: VariableAlias
  , variableName  :: String
  , units         :: Maybe String
  , lowerBound    :: Maybe Double
  , upperBound    :: Maybe Double
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Variable

instance ToJSON Variable


type VariableAlias = String


type RecordIdentifier = Word64


type Record = [Double]

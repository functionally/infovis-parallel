{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}


module InfoVis.Parallel.Types.Input.VRPN (
  InputVRPN(..)
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Binary (Binary)
import GHC.Generics (Generic)


data InputVRPN =
  Input
  {
    headDevice     :: String
  , headSensor     :: Int
  , selectDevice   :: String
  , selectSensor   :: Int
  , buttonDevice   :: String
  , selectButton   :: Int
  , deselectButton :: Int
  , clearButton    :: Int
  , forwardButton  :: Int
  , backwardButton :: Int
  , resetButton    :: Int
  }
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)


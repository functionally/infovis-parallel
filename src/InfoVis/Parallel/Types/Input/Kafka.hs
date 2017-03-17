{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module InfoVis.Parallel.Types.Input.Kafka (
  InputKafka(..)
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Network.UI.Kafka (Sensor, TopicConnection(..))
import Linear.Util.Instances ()
import Linear.V3 (V3)


deriving instance Ord TopicConnection

deriving instance Binary TopicConnection

deriving instance Hashable TopicConnection


data InputKafka =
  Input
  {
    kafka           :: TopicConnection
  , povInput        :: Either (V3 Double, V3 Double) Sensor
  , relocationInput :: Sensor
  , selectorInput   :: Sensor
  , selectButton    :: (Sensor, Int)
  , deselectButton  :: (Sensor, Int)
  , clearButton     :: (Sensor, Int)
  }
    deriving (Binary, Eq, FromJSON, Generic, Hashable, Ord, Read, Show, ToJSON)

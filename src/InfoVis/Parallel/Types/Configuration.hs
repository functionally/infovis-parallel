{-# LANGUAGE DeriveGeneric #-}


module InfoVis.Parallel.Types.Configuration (
  Configuration(..)
, Viewers(..)
, Display(..)
, Input(..)
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)
import Graphics.Rendering.Handa.Projection (Screen)
import Graphics.UI.Handa.Setup (Stereo)
import InfoVis.Parallel.Types.Dataset (Dataset)
import InfoVis.Parallel.Types.Scaffold (Presentation, World)
import Linear.V3 (V3)
import Network.UI.Kafka (Sensor, TopicConnection)


data Configuration a =
  Configuration
  {
    dataset      :: Dataset
  , presentation :: Presentation
  , world        :: World
  , viewers      :: Viewers a
  , input        :: Input
  }
    deriving (Eq, Generic, Read, Show)

instance (FromJSON a, Generic a) => FromJSON (Configuration a)

instance (ToJSON a, Generic a) => ToJSON (Configuration a)


data Viewers a =
  Viewers
  {
    stereo        :: Stereo
  , nearPlane     :: a
  , farPlane      :: a
  , eyeSeparation :: V3 a
  , displays      :: [Display a]
  }
    deriving (Eq, Generic, Read, Show)

instance (FromJSON a, Generic a) => FromJSON (Viewers a)

instance (ToJSON a, Generic a) => ToJSON (Viewers a)


data Display a =
  Display
  {
    identifier :: Maybe String
  , geometry   :: Maybe String
  , screen     :: Screen a
  }
    deriving (Eq, Generic, Read, Show)

instance (FromJSON a, Generic a) => FromJSON (Display a)

instance (ToJSON a, Generic a) => ToJSON (Display a)


data Input =
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
    deriving (Eq, Generic, Read, Show)

instance FromJSON Input

instance ToJSON Input

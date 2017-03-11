{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE RecordWildCards      #-}


module InfoVis.Parallel.Types.Configuration (
  Configuration(..)
, Viewers(..)
, Display(..)
, peersList
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Binary (Binary)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Graphics.Rendering.Handa.Projection (Screen)
import Graphics.UI.Handa.Setup (Stereo)
import InfoVis.Parallel.Types.Dataset (Dataset)
import InfoVis.Parallel.Types.Input (Input)
import InfoVis.Parallel.Types.Presentation (Presentation)
import InfoVis.Parallel.Types.World (World)
import Linear.V3 (V3)


data Configuration a =
  Configuration
  {
    dataset      :: Dataset
  , presentation :: Presentation
  , world        :: World
  , viewers      :: Viewers a
  , input        :: Input
  }
    deriving (Binary, Eq, Generic, Ord, Read, Show)

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
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)


data Display a =
  Display
  {
    host       :: Maybe String
  , port       :: Maybe Int
  , identifier :: Maybe String
  , geometry   :: Maybe String
  , screen     :: Screen a
  }
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)


peersList :: Configuration a -> [(String, String)]
peersList Configuration{..} =
  [
    (fromMaybe "localhost" host, show $ fromMaybe 44444 port)
  |
    let Viewers{..} = viewers
  , Display{..} <- displays
  ]

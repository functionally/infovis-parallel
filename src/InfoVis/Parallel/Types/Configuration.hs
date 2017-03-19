{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE RecordWildCards      #-}


module InfoVis.Parallel.Types.Configuration (
  Configuration(..)
, AdvancedSettings(..)
, Viewers(..)
, Display(..)
, peersList
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Binary (Binary)
import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Graphics.OpenGL.Util.Types (Display(..), Viewers(..))
import InfoVis.Parallel.Types.Dataset (Dataset)
import InfoVis.Parallel.Types.Input (Input)
import InfoVis.Parallel.Types.Presentation (Presentation)
import InfoVis.Parallel.Types.World (World)


data Configuration =
  Configuration
  {
    dataset      :: Dataset
  , presentation :: Presentation
  , world        :: World
  , viewers      :: Viewers Double
  , input        :: Input
  , advanced     :: Maybe AdvancedSettings
  }
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)


data AdvancedSettings =
  AdvancedSettings
  {
    debugTiming         :: Bool
  , debugMessages       :: Bool
  , debugDisplay        :: Bool
  , debugOpenGL         :: Bool
  , useSwapGroup        :: Maybe Int
  , synchronizeDisplays :: Bool
  , useIdleLoop         :: Bool
  , updateDelay         :: Maybe Double
  , trackAveraging      :: Double
  }
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance Default AdvancedSettings where
  def =
    AdvancedSettings
    {
      debugTiming         = False
    , debugMessages       = False
    , debugDisplay        = False
    , debugOpenGL         = False
    , useSwapGroup        = Nothing
    , synchronizeDisplays = False
    , useIdleLoop         = True
    , updateDelay         = Just 2
    , trackAveraging      = 0.10
    }   


peersList :: Configuration -> [(String, String)]
peersList Configuration{..} =
  [
    (fromMaybe "localhost" host, show $ fromMaybe 44444 port)
  |
    let Viewers{..} = viewers
  , Display{..} <- displays
  ]

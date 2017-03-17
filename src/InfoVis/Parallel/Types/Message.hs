{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}


module InfoVis.Parallel.Types.Message (
  -- * Classes
  SumTag(..)
, MessageTag(..)
-- * Types
, Augmentation
, SelectionAction(..)
  -- * Messages
, CommonMessage(..)
, MasterMessage(..)
, SelecterMessage(..)
, SelecterMessage'
, DisplayerMessage(..)
, DisplayerMessage'
) where


import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import InfoVis.Parallel.Rendering.Types (DisplayList, DisplayText, DisplayType)
import InfoVis.Parallel.Types (Coloring, Location)
import InfoVis.Parallel.Types.Configuration (Configuration)
import Linear.Affine (Point)
import Linear.Quaternion (Quaternion)
import Linear.V3 (V3)


class SumTag a where
  sumTag :: a -> Char


class MessageTag a where
  messageTag :: a -> String


type Augmentation = DisplayList (DisplayType, String) Int


data CommonMessage =
    Reconfigure
    {
      configuration :: Configuration
    }
  | Synchronize
  | Terminate
    deriving (Binary, Eq, Generic, Ord, Show)


data MasterMessage =
    Ready
  | Fault
    {
      fault :: String
    }
  | Exit
    deriving (Binary, Eq, Generic, Ord, Show)

instance SumTag MasterMessage where
  sumTag Ready   = '0'
  sumTag Fault{} = '1'
  sumTag Exit    = '2'

instance MessageTag MasterMessage where
  messageTag Ready   = "Master\tReady"
  messageTag Fault{} = "Master\tFault"
  messageTag Exit    = "Master\tExit"


data SelecterMessage =
    AugmentSelection
    {
      selecterAugmentations :: ([DisplayText String Location], [Augmentation], [Augmentation])
    }
  | UpdateSelection
    {
      selecterPosition :: Point V3 Double
    , selecterState    :: SelectionAction
    }
  | RelocateSelection
    {
      relocationDisplacement :: V3 Double
    , relocationRotation     :: Quaternion Double
    }
    deriving (Binary, Eq, Generic, NFData, Ord, Show)

instance SumTag SelecterMessage where
  sumTag AugmentSelection{}   = '0'
  sumTag UpdateSelection{}    = '1'
  sumTag RelocateSelection{}  = '2'

instance MessageTag SelecterMessage where
  messageTag AugmentSelection{}   = "Select\tAugment"
  messageTag UpdateSelection{}    = "Select\tUpdate"
  messageTag RelocateSelection{}  = "Select\tRelocate"


type SelecterMessage' = Either CommonMessage SelecterMessage

instance SumTag SelecterMessage' where
  sumTag (Right x            ) = sumTag x
  sumTag (Left  Reconfigure{}) = '7'
  sumTag (Left  Terminate    ) = '8'
  sumTag (Left  Synchronize  ) = '9'


data DisplayerMessage =
    RefreshDisplay
  | AugmentDisplay
    {
      augmentations :: ([DisplayText String Location], [Augmentation], [Augmentation])
    }
  | Track
    {
      eyePosition    :: Point V3 Double
    , eyeOrientation :: Quaternion Double
    }
  | Relocate
    {
      centerDisplacement :: V3 Double
    , centerRotation     :: Quaternion Double
    }
  | Select
    {
      selectorLocation :: Point V3 Double
    , selectionChanges :: [(Int, Coloring)]
    }
    deriving (Binary, Eq, Generic, NFData, Ord, Show)

instance SumTag DisplayerMessage where
  sumTag RefreshDisplay   = '0'
  sumTag AugmentDisplay{} = '1'
  sumTag Track{}          = '2'
  sumTag Relocate{}       = '3'
  sumTag Select{}         = '4'

instance MessageTag DisplayerMessage where
  messageTag RefreshDisplay   = "Display\tRefresh"
  messageTag AugmentDisplay{} = "Display\tAugment"
  messageTag Track{}          = "Display\tTrack"
  messageTag Relocate{}       = "Display\tRelocate"
  messageTag Select{..}       = "Display\tSelect\t" ++ show (length selectionChanges)


type DisplayerMessage' = Either CommonMessage DisplayerMessage

instance SumTag DisplayerMessage' where
  sumTag (Right x            ) = sumTag x
  sumTag (Left  Reconfigure{}) = '7'
  sumTag (Left  Terminate    ) = '8'
  sumTag (Left  Synchronize  ) = '9'


data SelectionAction = Highlight | Selection | Deselection | Clear
 deriving (Binary, Eq, Generic, NFData, Ord, Show)

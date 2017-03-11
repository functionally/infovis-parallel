{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}


module InfoVis.Parallel.Types.Message (
  -- * Classes
  SumTag(..)
-- * Types
, Processes(..)
, Augmentation
, SelectionAction(..)
  -- * Messages
, MasterMessage(..)
, TrackerMessage(..)
, SelecterMessage(..)
, DisplayerMessage(..)
) where


import Control.Distributed.Process (ProcessId)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import InfoVis.Parallel.Types (Coloring, Location)
import InfoVis.Parallel.Types.Configuration (Configuration)
import InfoVis.Parallel.Types.Input (Input)
import InfoVis.Parallel.Types.Display (DisplayList, DisplayText, DisplayType)
import Linear.Affine (Point)
import Linear.Quaternion (Quaternion)
import Linear.V3 (V3)


class SumTag a where
  sumTag :: a -> Char


data Processes =
  Processes
  {
    masterPid     :: ProcessId
  , trackerPid    :: ProcessId
  , selecterPid   :: ProcessId
  , displayerPids :: [ProcessId]
  }
    deriving (Binary, Eq, Generic, Show)


type Augmentation = DisplayList (DisplayType, String) Int


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


data TrackerMessage =
    ResetTracker
    {
      trackerInput :: Input
    }
  | TerminateTracker
    deriving (Binary, Eq, Generic, Ord, Show)

instance SumTag TrackerMessage where
  sumTag ResetTracker{}   = '0'
  sumTag TerminateTracker = '1'


data SelecterMessage =
    ResetSelecter
    {
      selecterConfiguration :: Configuration Double
    }
  | TerminateSelecter
  | AugmentSelecter
    {
      selecterAugmentations :: ([DisplayText String Location], [Augmentation], [Augmentation])
    }
  | UpdateSelecter
    {
      selecterPosition :: Point V3 Double
    , selecterState    :: SelectionAction
    }
  | RelocateSelecter
    {
      relocationDisplacement :: V3 Double
    , relocationRotation     :: Quaternion Double
    }
    deriving (Binary, Eq, Generic, Ord, Show)

instance SumTag SelecterMessage where
  sumTag ResetSelecter{}     = '0'
  sumTag TerminateSelecter{} = '1'
  sumTag AugmentSelecter{}   = '2'
  sumTag UpdateSelecter{}    = '3'
  sumTag RelocateSelecter{}  = '4'


data DisplayerMessage =
    ResetDisplayer
  | TerminateDisplayer
  | DisplayDisplayer
  | AugmentDisplayer
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
  | Debug
    {
      debugging :: Bool
    }
    deriving (Binary, Eq, Generic, Ord, Show)

instance SumTag DisplayerMessage where
  sumTag ResetDisplayer     = '0'
  sumTag TerminateDisplayer = '1'
  sumTag DisplayDisplayer   = '2'
  sumTag AugmentDisplayer{} = '3'
  sumTag Track{}            = '4'
  sumTag Relocate{}         = '5'
  sumTag Select{}           = '6'
  sumTag Debug{}            = '7'


data SelectionAction = Highlight | Selection | Deselection | Clear
 deriving (Binary, Eq, Generic, Ord, Show)

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module InfoVis.Parallel.Types.Message (
  -- * Types
  Processes(..)
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
import InfoVis.Parallel.Types.Configuration (Input)
import InfoVis.Parallel.Types.Display (DisplayList, DisplayType)
import Linear.Affine (Point)
import Linear.Quaternion (Quaternion)
import Linear.V3 (V3)


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


data TrackerMessage =
    ResetTracker
    {
      trackerInput :: Input
    }
  | TerminateTracker
  | AugmentTracker
    {
      trackerAugmentations :: ([Augmentation], [Augmentation])
    }
    deriving (Binary, Eq, Generic, Ord, Show)


data SelecterMessage =
    ResetSelecter
  | TerminateSelecter
  | AugmentSelecter
    {
      selecterAugmentations :: ([Augmentation], [Augmentation])
    }
    deriving (Binary, Eq, Generic, Ord, Show)


data DisplayerMessage =
    ResetDisplayer
  | TerminateDisplayer
  | DisplayDisplayer
  | AugmentDisplayer
    {
      augmentations :: ([Augmentation], [Augmentation])
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
      selectPosition :: Point V3 Double
    , selectState    :: SelectionAction
    }
  | Debug
    {
      debuggin :: Bool
    }
    deriving (Binary, Eq, Generic, Ord, Show)


data SelectionAction = Highlight | Selection | Deselection | Clear
 deriving (Binary, Eq, Generic, Ord, Show)

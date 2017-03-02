{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module InfoVis.Parallel.Types.Message (
  -- * Types
  Processes(..)
  -- * Messages
, MasterMessage(..)
, TrackerMessage(..)
, SelecterMessage(..)
, DisplayerMessage(..)
) where


import Control.Distributed.Process (ProcessId)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import InfoVis.Parallel.Types.Display (DisplayList)
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


data MasterMessage =
    Ready
  | Fault
    {
      fault :: String
    }
  | Exit
    deriving (Binary, Eq, Generic, Ord, Show)


data TrackerMessage a b =
    ResetTracker
  | TerminateTracker
  | AugmentTracker
    {
      trackerAugmentation :: DisplayList a b
    }
    deriving (Binary, Eq, Generic, Ord, Show)


data SelecterMessage a b =
    ResetSelecter
  | TerminateSelecter
  | AugmentSelecter
    {
      selecterAugmentation :: DisplayList a b
    }
    deriving (Binary, Eq, Generic, Ord, Show)


data DisplayerMessage a b =
    ResetDisplayer
  | TerminateDisplayer
  | DisplayDisplayer
  | AugmentDisplayer
    {
      augmentation :: DisplayList a b
    }
  | Track
    {
      eyePosition    :: Point V3 b
    , eyeOrientation :: Quaternion b
    }
  | Relocate
    {
      centerDisplacement :: V3 b
    , centerRotation     :: Quaternion b
    }
  | Select
  | Debug
    {
      debuggin :: Bool
    }
    deriving (Binary, Eq, Generic, Ord, Show)

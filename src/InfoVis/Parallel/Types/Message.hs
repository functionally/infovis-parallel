{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}


module InfoVis.Parallel.Types.Message (
  -- * Classes
  SumTag(..)
, MessageTag(..)
-- * Types
, AugmentationType
, Augmentation
, SelectionAction(..)
  -- * Messages
, CommonMessage(..)
, MasterMessage(..)
, SelecterMessage(..)
, SelecterMessage'
, DisplayerMessage(..)
, DisplayerMessage'
, makeNextMessageIdentifier
) where


import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)
import Control.Distributed.Process (Process, liftIO)
import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import InfoVis.Parallel.Rendering.Types (DisplayList, DisplayText, DisplayType)
import InfoVis.Parallel.Types (Coloring, Location)
import InfoVis.Parallel.Types.Presentation (TimeAlias)
import InfoVis.Parallel.Types.Configuration (Configuration)
import Linear.Affine (Point)
import Linear.Quaternion (Quaternion)
import Linear.V3 (V3)


class SumTag a where
  sumTag :: a -> Char


class MessageTag a where
  messageTag :: a -> String


type AugmentationType = (DisplayType, String)


type Augmentation = DisplayList AugmentationType Int


data CommonMessage =
    Reconfigure
    {
      commonMessageIdentifier :: Int
    , configuration           :: Configuration
    }
  | Synchronize
    {
      commonMessageIdentifier :: Int
    }
  | Terminate
    {
      commonMessageIdentifier :: Int
    }
    deriving (Binary, Eq, Generic, Ord, Show)

instance MessageTag CommonMessage where
  messageTag Reconfigure{..} = "Common\tReconf\t" ++ show commonMessageIdentifier
  messageTag Synchronize{..} = "Common\tSynch\t"  ++ show commonMessageIdentifier
  messageTag Terminate{..}   = "Common\tTerm\t"   ++ show commonMessageIdentifier


data MasterMessage =
    Ready
    {
      masterMessageIdentifier :: Int
    }
  | Fault
    {
      masterMessageIdentifier :: Int
    , fault                   :: String
    }
  | Exit
    {
      masterMessageIdentifier :: Int
    }
    deriving (Binary, Eq, Generic, Ord, Show)

instance SumTag MasterMessage where
  sumTag Ready{} = '0'
  sumTag Fault{} = '1'
  sumTag Exit{}  = '2'

instance MessageTag MasterMessage where
  messageTag Ready{..} = "Master\tReady\t" ++ show masterMessageIdentifier
  messageTag Fault{..} = "Master\tFault\t" ++ show masterMessageIdentifier
  messageTag Exit{..}  = "Master\tExit\t"  ++ show masterMessageIdentifier


data SelecterMessage =
    AugmentSelection
    {
      selecterMessageIdentifier :: Int
    , selecterAugmentations     :: [Augmentation]
    }
  | UpdateSelection
    {
      selecterMessageIdentifier :: Int
    , selecterPosition          :: Point V3 Double
    , selecterState             :: SelectionAction
    }
  | RelocateSelection
    {
      selecterMessageIdentifier :: Int
    , relocationDisplacement    :: V3 Double
    , relocationRotation        :: Quaternion Double
    }
    deriving (Binary, Eq, Generic, NFData, Ord, Show)

instance SumTag SelecterMessage where
  sumTag AugmentSelection{}   = '0'
  sumTag UpdateSelection{}    = '1'
  sumTag RelocateSelection{}  = '2'

instance MessageTag SelecterMessage where
  messageTag AugmentSelection{..}   = "Select\tAugment\t"  ++ show selecterMessageIdentifier
  messageTag UpdateSelection{..}    = "Select\tUpdate\t"   ++ show selecterMessageIdentifier
  messageTag RelocateSelection{..}  = "Select\tRelocat\t" ++ show selecterMessageIdentifier


type SelecterMessage' = Either CommonMessage SelecterMessage

instance SumTag SelecterMessage' where
  sumTag (Right x            ) = sumTag x
  sumTag (Left  Reconfigure{}) = '7'
  sumTag (Left  Terminate{}  ) = '8'
  sumTag (Left  Synchronize{}) = '9'


data DisplayerMessage =
    SetText
    {
      displayerMessageIdentifier :: Int
    , text                       :: [DisplayText String Location]
    }
  | AugmentDisplay
    {
      displayerMessageIdentifier :: Int
    , augmentations              :: [Augmentation]
    }
  | Track
    {
      displayerMessageIdentifier :: Int
    , eyePosition                :: Point V3 Double
    , eyeOrientation             :: Quaternion Double
    }
  | Relocate
    {
      displayerMessageIdentifier :: Int
    , centerDisplacement         :: V3 Double
    , centerRotation             :: Quaternion Double
    }
  | Select
    {
      displayerMessageIdentifier :: Int
    , currentTime                :: TimeAlias
    , selectorLocation           :: Point V3 Double
    , selectionChanges           :: [((DisplayType, String), [(Int, Coloring)])]
    }
    deriving (Binary, Eq, Generic, NFData, Ord, Show)

instance SumTag DisplayerMessage where
  sumTag SetText{}        = '0'
  sumTag AugmentDisplay{} = '1'
  sumTag Track{}          = '2'
  sumTag Relocate{}       = '3'
  sumTag Select{}         = '4'

instance MessageTag DisplayerMessage where
  messageTag SetText{..}        = "Display\tText\t"     ++ show displayerMessageIdentifier
  messageTag AugmentDisplay{..} = "Display\tAugment\t"  ++ show displayerMessageIdentifier
  messageTag Track{..}          = "Display\tTrack\t"    ++ show displayerMessageIdentifier
  messageTag Relocate{..}       = "Display\tRelocat\t"  ++ show displayerMessageIdentifier
  messageTag Select{..}         = "Display\tSelect\t"   ++ show displayerMessageIdentifier


type DisplayerMessage' = Either CommonMessage DisplayerMessage

instance SumTag DisplayerMessage' where
  sumTag (Right x            ) = sumTag x
  sumTag (Left  Reconfigure{}) = '7'
  sumTag (Left  Terminate{}  ) = '8'
  sumTag (Left  Synchronize{}) = '9'


data SelectionAction = Highlight | Selection | Deselection | Clear | Forward | Backward | Reset
 deriving (Binary, Eq, Generic, NFData, Ord, Show)


makeNextMessageIdentifier :: Int -> Int -> Process (Process Int)
makeNextMessageIdentifier stride offset =
  do
    counterVar <- liftIO $ newTVarIO offset
    return
      $ liftIO
      . atomically
      $ do
        counter <- readTVar counterVar
        writeTVar counterVar $ counter + stride
        return counter

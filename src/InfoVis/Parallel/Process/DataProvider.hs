{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.DataProvider (
  GridsLinks
, provider
) where


import Control.DeepSeq (($!!))
import Control.Distributed.Process (Process, SendPort, liftIO, sendChan)
import InfoVis.Parallel.IO (readDataset)
import InfoVis.Parallel.Presentation.Displaying (prepareGrids, prepareLinks)
import InfoVis.Parallel.Process.Util (Debug(..), frameDebug)
import InfoVis.Parallel.Rendering.Types (DisplayList, DisplayText, DisplayType)
import InfoVis.Parallel.Types (Location)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), messageTag, nextMessageIdentifier)


type GridsLinks = ([DisplayText String Location], [DisplayList (DisplayType, String) Int], [DisplayList (DisplayType, String) Int])


provider :: Configuration -> SendPort SelecterMessage -> SendPort DisplayerMessage -> Process ()
provider Configuration{..} selecterSend multiplexer =
  do
    frameDebug DebugInfo  "Starting data provider."
    rs <- liftIO $ readDataset dataset
    let
      (grids, texts) = prepareGrids world presentation dataset
      links = prepareLinks world presentation dataset rs
      gridsLinks = (texts, grids, links)
    mid1 <- nextMessageIdentifier
    frameDebug DebugMessage $ "DP SC 1\t" ++ messageTag (AugmentSelection mid1 gridsLinks)
    sendChan selecterSend $!! AugmentSelection mid1 gridsLinks
    mid2 <- nextMessageIdentifier
    frameDebug DebugMessage $ "DP SC 2\t" ++ messageTag (AugmentDisplay mid2 gridsLinks)
    sendChan multiplexer $!! AugmentDisplay mid2 gridsLinks

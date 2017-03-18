{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.DataProvider (
  provider
) where


import Control.DeepSeq (($!!))
import Control.Distributed.Process (Process, SendPort, liftIO, sendChan)
import InfoVis.Parallel.IO (readDataset)
import InfoVis.Parallel.Presentation.Displaying (prepareGrids, prepareLinks)
import InfoVis.Parallel.Process.Util (Debug(..), frameDebug)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), messageTag, nextMessageIdentifier)


provider :: Configuration -> SendPort SelecterMessage -> SendPort DisplayerMessage -> Process ()
provider Configuration{..} selecterSend multiplexer =
  do
    frameDebug DebugInfo  "Starting data provider."
    rs <- liftIO $ readDataset dataset
    let
      (grids, texts) = prepareGrids world presentation dataset
      links = prepareLinks world presentation dataset rs
    mid1 <- nextMessageIdentifier
    frameDebug DebugMessage $ "DP SC 1\t" ++ messageTag (AugmentSelection mid1 links)
    sendChan selecterSend $!! AugmentSelection mid1 links
    mid2 <- nextMessageIdentifier
    frameDebug DebugMessage $ "DP SC 2\t" ++ messageTag (AugmentDisplay mid2 grids)
    sendChan multiplexer $!! AugmentDisplay mid2 grids
    mid3 <- nextMessageIdentifier
    frameDebug DebugMessage $ "DP SC 3\t" ++ messageTag (SetText mid3 texts)
    sendChan multiplexer $!! SetText mid3 texts
    mid4 <- nextMessageIdentifier
    frameDebug DebugMessage $ "DP SC 4\t" ++ messageTag (AugmentDisplay mid4 links)
    sendChan multiplexer $!! AugmentDisplay mid4 links

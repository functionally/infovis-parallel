{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.DataProvider (
  provider
) where


import Control.DeepSeq (($!!))
import Control.Distributed.Process (Process, SendPort, liftIO, sendChan)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import InfoVis.Parallel.IO (readDataset)
import InfoVis.Parallel.Presentation.Displaying (prepareGrids, prepareLinks)
import InfoVis.Parallel.Process.Util (Debug(..), Debugger)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Dataset (Dataset(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), messageTag, makeNextMessageIdentifier)


provider :: Debugger -> Configuration -> SendPort SelecterMessage -> SendPort DisplayerMessage -> Process ()
provider frameDebug Configuration{..} selecterSend multiplexer =
  do
    frameDebug DebugInfo  "Starting data provider."
    nextMessageIdentifier <- makeNextMessageIdentifier 10 6
    rs <- liftIO $ readDataset dataset
    let
      (grids, texts) = prepareGrids world presentation dataset
      links = prepareLinks world presentation dataset rs
    mid1 <- nextMessageIdentifier
    frameDebug DebugMessage $ "DP SC 1\t" ++ messageTag (AugmentDisplay mid1 grids)
    sendChan multiplexer $!! AugmentDisplay mid1 grids
    mid2 <- nextMessageIdentifier
    frameDebug DebugMessage $ "DP SC 2\t" ++ messageTag (SetText mid2 texts)
    sendChan multiplexer $!! SetText mid2 texts
    sequence_
      [
        do
          mid3 <- nextMessageIdentifier
          frameDebug DebugMessage $ "DP SC 3\t" ++ messageTag (AugmentSelection mid3 links')
          sendChan selecterSend $!! AugmentSelection mid3 links'
          mid4 <- nextMessageIdentifier
          frameDebug DebugMessage $ "DP SC 4\t" ++ messageTag (AugmentDisplay mid4 links')
          sendChan multiplexer $!! AugmentDisplay mid4 links'
      |
        let Dataset{..} = dataset
      , let maxRecords' = fromMaybe maxBound maxRecords
      , let chunkSize' = fromMaybe maxBound chunkSize
      , links' <- chunksOf chunkSize' $ take maxRecords' links
      ]

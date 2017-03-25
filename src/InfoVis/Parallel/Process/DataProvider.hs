{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.DataProvider (
  provider
) where


import Control.Distributed.Process (Process, SendPort, liftIO)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import InfoVis.Parallel.IO (readDataset)
import InfoVis.Parallel.Presentation.Displaying (prepareGrids, prepareLinks)
import InfoVis.Parallel.Process.Util (Debugger, runProcess, sendChan')
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Dataset (Dataset(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..))


provider :: Debugger -> Configuration -> SendPort SelecterMessage -> SendPort DisplayerMessage -> Process ()
provider frameDebug Configuration{..} selecter multiplexer =
  runProcess "data provider" 3 frameDebug $ \nextMessageIdentifier -> do
    rs <- liftIO $ readDataset dataset
    let
      sendMultiplexer = sendChan' frameDebug nextMessageIdentifier multiplexer
      sendSelecter    = sendChan' frameDebug nextMessageIdentifier selecter
      (grids, texts) = prepareGrids world presentation dataset
      links = prepareLinks world presentation dataset rs
    sendMultiplexer "DP SC 1" $ AugmentDisplay grids
    sendMultiplexer "DP SC 2" $ SetText texts
    sequence_
      [
        do
          sendSelecter "DP SC 3" $ AugmentSelection links'
          sendMultiplexer "DP SC 4" $ AugmentDisplay links'
      |
        let Dataset{..} = dataset
      , let maxRecords' = fromMaybe maxBound maxRecords
      , let chunkSize' = fromMaybe maxBound chunkSize
      , links' <- chunksOf chunkSize' $ take maxRecords' links
      ]

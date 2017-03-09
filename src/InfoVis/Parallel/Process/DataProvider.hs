{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.DataProvider (
  GridsLinks
, provider
) where


import Control.Distributed.Process (Process, SendPort, getSelfPid, liftIO, say, sendChan)
import InfoVis.Parallel.IO (readDataset)
import InfoVis.Parallel.Primitive (prepareGrids, prepareLinks)
import InfoVis.Parallel.Types (Location)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Display (DisplayList, DisplayText, DisplayType)
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..))


type GridsLinks = ([DisplayText String Location], [DisplayList (DisplayType, String) Int], [DisplayList (DisplayType, String) Int])


provider :: Configuration Double -> SendPort SelecterMessage -> SendPort DisplayerMessage -> Process ()
provider Configuration{..} selecterSend multiplexer =
  do
    pid <- getSelfPid
    say $ "Starting data provider <" ++ show pid ++ ">."
    rs <- liftIO $ readDataset dataset
    let
      (grids, texts) = prepareGrids world presentation dataset
      links = prepareLinks world presentation dataset rs
      gridsLinks = (texts, grids, links)
    selecterSend `sendChan` AugmentSelecter gridsLinks
    multiplexer `sendChan` AugmentDisplayer gridsLinks

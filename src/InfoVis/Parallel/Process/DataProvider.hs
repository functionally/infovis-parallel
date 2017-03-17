{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.DataProvider (
  GridsLinks
, provider
) where


import Control.DeepSeq (($!!))
import Control.Distributed.Process (Process, SendPort, getSelfPid, liftIO, say, sendChan)
import InfoVis.Parallel.IO (readDataset)
import InfoVis.Parallel.Presentation.Displaying (prepareGrids, prepareLinks)
import InfoVis.Parallel.Process.Util (Debug(..), frameDebug)
import InfoVis.Parallel.Rendering.Types (DisplayList, DisplayText, DisplayType)
import InfoVis.Parallel.Types (Location)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), messageTag)


type GridsLinks = ([DisplayText String Location], [DisplayList (DisplayType, String) Int], [DisplayList (DisplayType, String) Int])


provider :: Configuration -> SendPort SelecterMessage -> SendPort DisplayerMessage -> Process ()
provider Configuration{..} selecterSend multiplexer =
  do
    pid <- getSelfPid
    say $ "Starting data provider <" ++ show pid ++ ">."
    rs <- liftIO $ readDataset dataset
    let
      (grids, texts) = prepareGrids world presentation dataset
      links = prepareLinks world presentation dataset rs
      gridsLinks = (texts, grids, links)
    frameDebug DebugMessage $ "DP SC 1\t" ++ messageTag (AugmentSelection gridsLinks)
    sendChan selecterSend $!! AugmentSelection gridsLinks
    frameDebug DebugMessage $ "DP SC 2\t" ++ messageTag (AugmentDisplay gridsLinks)
    sendChan multiplexer $!! AugmentDisplay gridsLinks

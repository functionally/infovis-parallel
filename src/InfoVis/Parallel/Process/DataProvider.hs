{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.DataProvider (
  GridsLinks
, provider
) where


import InfoVis.Parallel.IO (readDataset)
import InfoVis.Parallel.Primitive (prepareGrids, prepareLinks)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Display (DisplayList, DisplayType)


type GridsLinks = ([DisplayList (DisplayType, String) Int], [DisplayList (DisplayType, String) Int])


provider :: Configuration Double -> IO GridsLinks
provider Configuration{..} =
  do
    rs <- readDataset dataset
    let
      grids = prepareGrids world presentation
      links = prepareLinks world presentation dataset rs
    return (grids, links)

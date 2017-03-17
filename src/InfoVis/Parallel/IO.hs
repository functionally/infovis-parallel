{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.IO (
  readDataset
) where


import Data.List.Split (splitOn)
import Data.List.Util (elemPermutation)
import Data.Maybe (fromMaybe)
import InfoVis.Parallel.Types.Dataset (Dataset(..), Record, Variable(..))


readDataset :: Dataset -> IO [Record]
readDataset Dataset{..} =
  do
    (header : contents) <- fmap (splitOn "\t") . lines <$> readFile datasetIdentifier
    let
      reordering =
        fromMaybe (error "readDataset: Missing variables.")
          . elemPermutation header
          $ variableName
          <$> variables
      reorder = (<$> reordering) . (read .) . (!!)
    return $ reorder <$> contents

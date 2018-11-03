{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Presentation.Displaying (
  prepareGrids
, prepareLinks
) where


import Control.Arrow (first)
import Data.Function.MapReduce (groupReduceByKey)
import Data.List (elemIndex)
import Data.Tuple.Util (trd3)
import InfoVis.Parallel.Presentation.Presenting (linkPresentation, presentWorld)
import InfoVis.Parallel.Presentation.Scaling (scaleToWorld)
import InfoVis.Parallel.Rendering.Types (DisplayItem(..), DisplayList(..), DisplayText(..), DisplayType(..), fromLocations)
import InfoVis.Parallel.Types (Location)
import InfoVis.Parallel.Types.Dataset (Dataset(..), Record, RecordIdentifier, Variable(variableAlias))
import InfoVis.Parallel.Types.Presentation (Characteristic, GridAlias, Presentation(animationKey, uniqueKey), TimeAlias)
import InfoVis.Parallel.Types.World (World)

import qualified Data.HashMap.Strict as H (HashMap, empty, insert, lookup)


prepareGrids :: World -> Presentation -> Dataset -> ([DisplayList (DisplayType, GridAlias) Int], [DisplayText String Location])
prepareGrids world presentation Dataset{..} =
  let
    (grids, texts) = presentWorld world presentation
  in
    (
      prepare GridType grids
    , [
        text {textContent = textContent}
      |
        text@DisplayText{..} <- texts
      ]
    )


keyUniquely :: Maybe Int -> [Record] -> [(Record, RecordIdentifier)]
keyUniquely Nothing   = flip zip [0..]
keyUniquely (Just iu) =
  let
    assign :: (Int, H.HashMap Double Int, [(Record, RecordIdentifier)]) -> Record -> (Int, H.HashMap Double Int, [(Record, RecordIdentifier)])
    assign (next, table, priors) r =
      let
        k = r !! iu
      in
        case k `H.lookup` table of
          Nothing -> (next + 1, H.insert k next table, (r, next) : priors)
          Just v  -> (next, table, (r, v) : priors)
  in
    trd3 . foldl assign (0, H.empty, [])


prepareLinks :: World -> Presentation -> Dataset -> [Record] -> [DisplayList (DisplayType, TimeAlias) RecordIdentifier]
prepareLinks world presentation dataset rs =
  prepare LinkType
    $ concat
    [
      fmap (\di@DisplayItem{..} -> di {itemIdentifier = first (const t) itemIdentifier}) -- FIXME: Avoid rewriting the display item.
        . linkPresentation presentation n
        $ scaleToWorld world presentation dataset r
    |
      let it = (`elemIndex` (variableAlias <$> variables dataset)) =<< animationKey presentation
    , let iu = (`elemIndex` (variableAlias <$> variables dataset)) =<< uniqueKey    presentation
    , (r, n) <- keyUniquely iu rs
    , let t = maybe "" (show . (r !!)) it
    ]


prepare :: Ord a => DisplayType -> [DisplayItem (a, (b, [Characteristic])) Location] -> [DisplayList (DisplayType, a) b]
prepare dt =
  groupReduceByKey
    (
      \DisplayItem{..} -> (fst itemIdentifier, itemPrimitive)
    )
    (
      \(n, listPrimitive) dis ->
        let
          listIdentifier = (dt, n)
          listCharacteristics = snd . snd . itemIdentifier $ head dis
          listVertexIdentifiers = concatMap (\DisplayItem{..} -> length itemVertices `replicate` fst (snd itemIdentifier)) dis
          listVertices = concatMap itemVertices dis
        in
          DisplayList{..}
    )
    . fromLocations

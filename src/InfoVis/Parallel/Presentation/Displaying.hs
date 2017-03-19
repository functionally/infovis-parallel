{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Presentation.Displaying (
  prepareGrids
, prepareLinks
) where


import Control.Arrow (first)
import Data.Function.MapReduce (groupReduceByKey)
import Data.List (elemIndex)
import InfoVis.Parallel.Presentation.Presenting (linkPresentation, presentWorld)
import InfoVis.Parallel.Presentation.Scaling (scaleToWorld)
import InfoVis.Parallel.Rendering.Types (DisplayItem(..), DisplayList(..), DisplayText(..), DisplayType(..), fromLocations)
import InfoVis.Parallel.Types (Location)
import InfoVis.Parallel.Types.Dataset (Dataset(..), Record, RecordIdentifier, Variable(variableAlias))
import InfoVis.Parallel.Types.Presentation (Characteristic, GridAlias, Presentation(animation), TimeAlias)
import InfoVis.Parallel.Types.World (World)


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


prepareLinks :: World -> Presentation -> Dataset -> [Record] -> [DisplayList (DisplayType, TimeAlias) RecordIdentifier]
prepareLinks world presentation dataset rs =
  prepare LinkType
    $ concat
    [
      fmap (\di@DisplayItem{..} -> di {itemIdentifier = first (const t) itemIdentifier}) -- FIXME: Avoid rewriting the display item.
        . linkPresentation presentation n
        $ scaleToWorld world presentation dataset r
    |
      let it = (`elemIndex` (variableAlias <$> variables dataset)) =<< animation presentation
    , (r, n) <- zip rs [0..]
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

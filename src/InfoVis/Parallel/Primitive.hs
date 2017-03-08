{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Primitive (
  fromLocation
, fromLocations
, prepareGrids
, prepareLinks
) where


import Data.Function.MapReduce (groupReduceByKey)
import Graphics.Rendering.OpenGL (GLfloat, Vertex3(..))
import InfoVis.Parallel.Presenting (linkPresentation, presentWorld)
import InfoVis.Parallel.Scaling (scaleToWorld)
import InfoVis.Parallel.Types (Location)
import InfoVis.Parallel.Types.Dataset (Dataset, Record, RecordIdentifier)
import InfoVis.Parallel.Types.Display (DisplayItem(..), DisplayList(..), DisplayType(..))
import InfoVis.Parallel.Types.Presentation (Characteristic, GridAlias, LinkAlias, Presentation)
import InfoVis.Parallel.Types.World (World)
import Linear.Affine (Point(..))
import Linear.V3 (V3(..))


type Primitive3D = Vertex3 GLfloat


fromLocation :: Location -> Primitive3D
fromLocation (P (V3 x y z)) = realToFrac <$> Vertex3 x y z


fromLocations :: [DisplayItem a Location] -> [DisplayItem a Primitive3D]
fromLocations = fmap (fmap fromLocation)


prepareGrids :: World -> Presentation -> [DisplayList (DisplayType, GridAlias) Int]
prepareGrids world presentation =
  prepare GridType
    $ presentWorld world presentation


prepareLinks :: World -> Presentation -> Dataset -> [Record] -> [DisplayList (DisplayType, LinkAlias) RecordIdentifier]
prepareLinks world presentation dataset rs =
  prepare LinkType
    . concat 
    . zipWith (linkPresentation presentation) [0..]
    $ scaleToWorld world presentation dataset
    <$> rs


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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Presentation.Scaling (
  scaleVariable
, scaleRecord
, scaleToGrid
, scaleToExtent
, scaleToContainer
, scaleToWorldExtent
, scaleToWorld
) where


import Data.List.Util (elemPermutation)
import Data.Maybe (fromMaybe)
import InfoVis.Parallel.Types (Location)
import InfoVis.Parallel.Types.Dataset (Dataset(..), Record, Variable(..))
import InfoVis.Parallel.Types.Presentation (Axis(..), Container(..), Extent(..), Grid(..), GriddedLocation, Presentation(..))
import InfoVis.Parallel.Types.World (World(..), WorldExtent(..))
import Linear.Affine (Point(..), (.+^), (.-.))
import Linear.Matrix ((!+!), (!*), transpose)
import Linear.Metric (quadrance)
import Linear.V3 (V3(..))
import Linear.Vector ((^/), outer, zero)


{-# INLINE scaleRecord #-}
scaleRecord :: Dataset -> Record -> Record
scaleRecord Dataset{..} = zipWith scaleVariable variables


{-# INLINE scaleVariable #-}
scaleVariable :: Variable -> Double -> Double
scaleVariable ContinuousVariable{..} = scaleInterval lowerBound upperBound


scaleInterval :: Maybe Double -> Maybe Double -> Double -> Double
scaleInterval Nothing   Nothing   x = x
scaleInterval (Just x0) Nothing   x = x - x0
scaleInterval Nothing   (Just x1) x = x1 - x
scaleInterval (Just x0) (Just x1) x = (x - x0) / (x1 - x0)


extractAxes :: Foldable t => t Axis -> Dataset -> Record -> Location
extractAxes axes Dataset{..} =
  let
    positions =
      fromMaybe (error "findAxes: Variable alias not found.")
        $ elemPermutation
          (variableAlias <$> variables        )
          (axisVariable  <$> foldr (:) [] axes)
    extractor =
      case length axes of -- FIXME: This could be optimized.
        1 -> P $ V3 (!! head positions) (const 0            ) (const 0             )
        2 -> P $ V3 (!! head positions) (!! (positions !! 1)) (const 0             )
        3 -> P $ V3 (!! head positions) (!! (positions !! 1)) (!! (positions  !! 2))
        _ -> error "findAxes: Invalid number of axes."
  in
    (<$> extractor) . flip id


{-# INLINE scaleToGrid #-}
scaleToGrid :: Grid -> Dataset -> Record -> GriddedLocation
scaleToGrid LineGrid{..}      = ((gridAlias, ) .) . extractAxes axes1D
scaleToGrid RectangleGrid{..} = ((gridAlias, ) .) . extractAxes axes2D
scaleToGrid BoxGrid{..}       = ((gridAlias, ) .) . extractAxes axes3D


{-# INLINE scaleToExtent #-}
scaleToExtent :: Extent -> Location -> Location
scaleToExtent Extent1D{..} = scaleToExtent' origin cornerX zero    zero
scaleToExtent Extent2D{..} = scaleToExtent' origin cornerX cornerY zero
scaleToExtent Extent3D{..} = scaleToExtent' origin cornerX cornerY cornerZ


{-# INLINE scaleToExtent' #-}
scaleToExtent' :: Location -> Location -> Location -> Location -> Location -> Location
scaleToExtent' o cx cy cz l = o .+^ transpose (V3 (cx .-. o) (cy .-. o) (cz .-. o)) !* (l .-. zero) 


scaleToContainer :: Container -> Dataset -> Record -> [GriddedLocation]
scaleToContainer Singleton{..} dataset record =
  (: [])
    $ scaleToExtent extent
    <$> scaleToGrid grid dataset record
scaleToContainer Array{..} dataset record =
  ($ record)
    <$> zipWith (\e g -> fmap (scaleToExtent e) . scaleToGrid g dataset) extents grids
scaleToContainer Collection{..} dataset record =
  concat
    $ ($ record)
    <$> zipWith (\e c -> fmap (scaleToExtent e <$>) . scaleToContainer c dataset) extents containeds


scaleToWorldExtent :: World -> Location -> Location
scaleToWorldExtent World{..} l =
  let
    worldDirections WorldExtent{..} =
      (
        worldOrigin
      , worldCornerX .-. worldOrigin
      , worldCornerY .-. worldOrigin
      , worldCornerZ .-. worldOrigin
      )
    (d0, dx, dy, dz) = worldDirections displayExtent
    (w0, wx, wy, wz) = worldDirections worldExtent
  in
    w0 .+^ (    wx `outer` (dx ^/ quadrance dx)
            !+! wy `outer` (dy ^/ quadrance dy)
            !+! wz `outer` (dz ^/ quadrance dz)
           ) !* (l .-. d0)


scaleToWorld :: World -> Presentation -> Dataset -> Record -> [GriddedLocation]
scaleToWorld world Presentation{..} dataset record =
  let
    scaledRecord = scaleRecord dataset record
    scaleToWorld' container = scaleToContainer container dataset scaledRecord
  in
    fmap (scaleToWorldExtent world)
      <$> concatMap scaleToWorld' containers

{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Presenting (
  linkPresentation
, presentGrid
, presentContainer
, presentWorld
) where


import Data.Maybe (fromMaybe)
import Graphics.Rendering.OpenGL (PrimitiveMode(..))
import InfoVis.Parallel.Scaling (scaleToExtent, scaleToWorldExtent)
import InfoVis.Parallel.Types (Location)
import InfoVis.Parallel.Types.Dataset (RecordIdentifier)
import InfoVis.Parallel.Types.Display (DisplayItem(..))
import InfoVis.Parallel.Types.Scaffold (Characteristic, Container(..), Grid(..), GriddedLocation, GridAlias, Link(..), LinkAlias, Presentation(..), World)
import Linear.Affine (Point(..))
import Linear.V3 (V3(..))


pointsToLines :: RecordIdentifier -> Link -> [Location] -> [DisplayItem (LinkAlias, (RecordIdentifier, [Characteristic])) Location]
pointsToLines n l ls =
  [
    DisplayItem
    {
      itemIdentifier = (linkAlias l, (n, characteristics l))
    , itemPrimitive  = Lines
    , itemVertices   = ps
    }
  |
    ps <- zipWith ((. return) . (:)) (init ls) (tail ls)
  ]
   
 
linkPresentation :: Presentation -> RecordIdentifier -> [GriddedLocation] -> [DisplayItem (LinkAlias, (RecordIdentifier, [Characteristic]))  Location]
linkPresentation Presentation{..} n = concat . (<$> links) . flip (linkPresentation' n)


linkPresentation' :: RecordIdentifier -> Link -> [GriddedLocation] -> [DisplayItem (LinkAlias, (RecordIdentifier, [Characteristic]))  Location]
linkPresentation' n Point{..} gls = -- FIXME: Optimize this.
  [
    DisplayItem
    {
      itemIdentifier = (linkAlias, (n, characteristics))
    , itemPrimitive  = Points
    , itemVertices   = [
                         fromMaybe (error $ "linkPresentation: Grid \"" ++ linkedGrid ++ "\" not found.")
                           $ linkedGrid `lookup` gls
                       ]
    }
  ]
linkPresentation' n l@Polyline{..} gls = -- FIXME: Optimize this.
  pointsToLines n l
    [
      fromMaybe (error $ "linkPresentation: Grid \"" ++ g ++ "\" not found.") $ g `lookup` gls
    |
      g <- linkedGrids
    ]


quadToLines :: Int -> Grid -> [Location] -> [DisplayItem (GridAlias, (Int, [Characteristic])) Location]
quadToLines n g ls =
  DisplayItem
  {
    itemIdentifier = (gridAlias g, (n, faceCharacteristics g))
  , itemPrimitive  = Quads
  , itemVertices   = ls
  } :
  [
    DisplayItem
    {
      itemIdentifier = (gridAlias g, (n, lineCharacteristics g))
    , itemPrimitive  = Lines
    , itemVertices   = ps
    }
    |
    ps <- zipWith ((. return) . (:)) ls (last ls : init ls)
  ]


presentGrid :: Grid -> [DisplayItem (GridAlias, (Int, [Characteristic])) Location]
presentGrid LineGrid{..} =
  [
    DisplayItem
    {
      itemIdentifier = (gridAlias, (n, lineCharacteristics))
    , itemPrimitive  = Lines
    , itemVertices   = [
                         P $ (* d) <$> V3  i      0 0
                       , P $ (* d) <$> V3 (i + 1) 0 0
                       ]
    }
  |
    let d = 1 / (1 + fromIntegral divisions)
  , i' <- [0..divisions]
  , let n = i' + 1
  , let i = fromIntegral i'
  ]
presentGrid g@RectangleGrid{..} =
  concat
    [
      quadToLines n g
        [
          P $ (* d) <$> V3  i       j      0
        , P $ (* d) <$> V3  i      (j + 1) 0
        , P $ (* d) <$> V3 (i + 1) (j + 1) 0
        , P $ (* d) <$> V3 (i + 1)  j      0
        ]
    |
      let d = 1 / (1 + fromIntegral divisions)
    , i' <- [0..divisions]
    , j' <- [0..divisions]
    , let n = (divisions + 1) * i' + j'
    , let i = fromIntegral i'
    , let j = fromIntegral j'
    ]
presentGrid g@BoxGrid{..} =
  concat
    [
      concatMap (quadToLines n g)
        [
          [
            P $ (* d) <$> V3  i       j       k     
          , P $ (* d) <$> V3  i      (j + 1)  k     
          , P $ (* d) <$> V3 (i + 1) (j + 1)  k     
          , P $ (* d) <$> V3 (i + 1)  j       k     
          ]
        , [
            P $ (* d) <$> V3  i       j      (k + 1)
          , P $ (* d) <$> V3  i      (j + 1) (k + 1)
          , P $ (* d) <$> V3 (i + 1) (j + 1) (k + 1)
          , P $ (* d) <$> V3 (i + 1)  j      (k + 1)
          ]
        , [
            P $ (* d) <$> V3  i       j       k     
          , P $ (* d) <$> V3  i       j      (k + 1)
          , P $ (* d) <$> V3 (i + 1)  j      (k + 1)
          , P $ (* d) <$> V3 (i + 1)  j       k     
          ]
        , [
            P $ (* d) <$> V3  i      (j + 1)  k     
          , P $ (* d) <$> V3  i      (j + 1) (k + 1)
          , P $ (* d) <$> V3 (i + 1) (j + 1) (k + 1)
          , P $ (* d) <$> V3 (i + 1) (j + 1)  k     
          ]
        , [
            P $ (* d) <$> V3  i       j       k     
          , P $ (* d) <$> V3  i       j      (k + 1)
          , P $ (* d) <$> V3  i      (j + 1) (k + 1)
          , P $ (* d) <$> V3  i      (j + 1)  k     
          ]
        , [
            P $ (* d) <$> V3 (i + 1)  j       k     
          , P $ (* d) <$> V3 (i + 1)  j      (k + 1)
          , P $ (* d) <$> V3 (i + 1) (j + 1) (k + 1)
          , P $ (* d) <$> V3 (i + 1) (j + 1)  k     
          ]
        ]
    |
      let d = 1 / (1 + fromIntegral divisions)
    , i' <- [0..divisions]
    , j' <- [0..divisions]
    , k' <- [0..divisions]
    , let n = (divisions + 1) * ((divisions + 1) * i' + j') + k'
    , let i = fromIntegral i'
    , let j = fromIntegral j'
    , let k = fromIntegral k'
    ]


scaleItems :: (Location -> Location) -> [DisplayItem a Location] -> [DisplayItem a Location]
scaleItems = fmap . fmap


presentContainer  :: Container -> [DisplayItem (GridAlias, (Int, [Characteristic])) Location]
presentContainer Singleton{..} =
  scaleItems (scaleToExtent extent) $ presentGrid grid
presentContainer Array{..} =
  concat $ zipWith (\e -> scaleItems (scaleToExtent e) . presentGrid) extents grids
presentContainer Collection{..} =
  concat $ zipWith (\e -> scaleItems (scaleToExtent e) . presentContainer) extents containeds


presentWorld :: World -> Presentation -> [DisplayItem (GridAlias, (Int, [Characteristic])) Location]
presentWorld world Presentation{..} =
  concatMap (scaleItems (scaleToWorldExtent world) . presentContainer) containers

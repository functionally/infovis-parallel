{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Presenting (
  linkPresentation
, presentGrid
, presentContainer
, presentWorld
, Corners
) where


import Data.Maybe (fromMaybe)
import Graphics.Rendering.OpenGL (PrimitiveMode(..))
import InfoVis.Parallel.Scaling (scaleToExtent, scaleToWorldExtent)
import InfoVis.Parallel.Types (Location)
import InfoVis.Parallel.Types.Display (DisplayItem(..))
import InfoVis.Parallel.Types.Scaffold (Container(..), Grid(..), GriddedLocation, GridAlias, Link(..), LinkAlias, Presentation(..), World)
import Linear.Affine (Point(..))
import Linear.V3 (V3(..))


pointsToLines :: LinkAlias -> [Location] -> [DisplayItem LinkAlias Location]
pointsToLines a ls =
  [
    DisplayItem
    {
      itemIdentifier = a
    , itemPrimitive  = Lines
    , itemVertices   = ps
    }
  |
    ps <- zipWith ((. return) . (:)) (init ls) (tail ls)
  ]
   
 
linkPresentation :: Presentation -> [GriddedLocation] -> [DisplayItem LinkAlias Location]
linkPresentation Presentation{..} = concat . (<$> links) . flip linkPresentation'


linkPresentation' :: Link -> [GriddedLocation] -> [DisplayItem LinkAlias Location]
linkPresentation' Point{..} gls = -- FIXME: Optimize this.
  [
    DisplayItem
    {
      itemIdentifier = linkAlias
    , itemPrimitive  = Points
    , itemVertices   = [
                         fromMaybe (error $ "linkPresentation: Grid \"" ++ linkedGrid ++ "\" not found.")
                           $ linkedGrid `lookup` gls
                       ]
    }
  ]
linkPresentation' Polyline{..} gls = -- FIXME: Optimize this.
  pointsToLines linkAlias
    [
      fromMaybe (error $ "linkPresentation: Grid \"" ++ g ++ "\" not found.") $ g `lookup` gls
    |
      g <- linkedGrids
    ]


type Corners = [Location]


quadToLines :: GridAlias -> [Location] -> [DisplayItem GridAlias Location]
quadToLines a ls =
  DisplayItem
  {
    itemIdentifier = a
  , itemPrimitive  = Quads
  , itemVertices   = ls
  } :
  [
    DisplayItem
    {
      itemIdentifier = a
    , itemPrimitive  = Lines
    , itemVertices   = ps
    }
    |
    ps <- zipWith ((. return) . (:)) ls (last ls : init ls)
  ]


presentGrid :: Grid -> [DisplayItem GridAlias Location]
presentGrid LineGrid{..} =
  [
    DisplayItem
    {
      itemIdentifier = gridAlias
    , itemPrimitive  = Lines
    , itemVertices   = [
                         P $ (* d) <$> V3  i      0 0
                       , P $ (* d) <$> V3 (i + 1) 0 0
                       ]
    }
  |
    let d = 1 / (1 + fromIntegral divisions)
  , i <- fromIntegral <$> [0..divisions]
  ]
presentGrid RectangleGrid{..} =
  concat
    [
      quadToLines gridAlias
        [
          P $ (* d) <$> V3  i       j      0
        , P $ (* d) <$> V3  i      (j + 1) 0
        , P $ (* d) <$> V3 (i + 1) (j + 1) 0
        , P $ (* d) <$> V3 (i + 1)  j      0
        ]
    |
      let d = 1 / (1 + fromIntegral divisions)
    , i <- fromIntegral <$> [0..divisions]
    , j <- fromIntegral <$> [0..divisions]
    ]
presentGrid BoxGrid{..} =
  concat
    [
      concatMap (quadToLines gridAlias)
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
    , i <- fromIntegral <$> [0..divisions]
    , j <- fromIntegral <$> [0..divisions]
    , k <- fromIntegral <$> [0..divisions]
    ]


scaleItems :: (Location -> Location) -> [DisplayItem a Location] -> [DisplayItem a Location]
scaleItems = fmap . fmap


presentContainer  :: Container -> [DisplayItem GridAlias Location]
presentContainer Singleton{..} =
  scaleItems (scaleToExtent extent) $ presentGrid grid
presentContainer Array{..} =
  concat $ zipWith (\e -> scaleItems (scaleToExtent e) . presentGrid) extents grids
presentContainer Collection{..} =
  concat $ zipWith (\e -> scaleItems (scaleToExtent e) . presentContainer) extents containeds


presentWorld :: World -> Presentation -> [DisplayItem GridAlias Location]
presentWorld world Presentation{..} =
  concatMap (scaleItems (scaleToWorldExtent world) . presentContainer) containers

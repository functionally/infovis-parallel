{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Presentation.Presenting (
  linkPresentation
, presentGrid
, presentContainer
, presentWorld
) where


import Control.Arrow ((***))
import Data.Maybe (fromMaybe)
import InfoVis.Parallel.Presentation.Scaling (scaleToExtent, scaleToWorldExtent)
import InfoVis.Parallel.Rendering.Types (DisplayItem(..), DisplayText(..), PrimitiveMode(..))
import InfoVis.Parallel.Types (Location)
import InfoVis.Parallel.Types.Dataset (RecordIdentifier, VariableAlias)
import InfoVis.Parallel.Types.Presentation (Axis(..), Characteristic, Container(..), Grid(..), GriddedLocation, GridAlias, Link(..), LinkAlias, Presentation(..))
import InfoVis.Parallel.Types.World (World)
import Linear.Affine (Point(..))
import Linear.V1 (V1(..))
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.Vector ((*^), zero)


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


makeText :: Grid -> Axis -> V3 Double -> V3 Double -> DisplayText VariableAlias Location
makeText grid Axis{..} width height =
  DisplayText
  {
    textContent = axisVariable
  , textOrigin  = zero
  , textWidth   = P width
  , textHeight  = P $ labelSize grid *^ height
  , textColor   = labelColor grid
  , textSize    = labelSize  grid
  }


presentGrid :: Grid -> ([DisplayItem (GridAlias, (Int, [Characteristic])) Location], [DisplayText VariableAlias Location])
presentGrid grid@LineGrid{..} =
  (
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
  , [
      let
        V1 axis = axes1D
      in
         makeText grid axis (V3 1 0 0) (V3 0 (-1) 0)
    ]
  )
presentGrid grid@RectangleGrid{..} =
  (
    concat
      [
        quadToLines n grid
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
  , [
      let
        V2 axis _ = axes2D
      in
        makeText grid axis (V3 1 0 0) (V3 0 (-1) 0)
    , let
        V2 _ axis = axes2D
      in
        makeText grid axis (V3 0 1 0) (V3 (-1) 0 0)
    ]
  )
presentGrid grid@BoxGrid{..} =
  (
    concat
      [
        concatMap (quadToLines n grid)
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
  , [
      let
        V3 axis _ _ = axes3D
      in
        makeText grid axis (V3 1 0 0) (V3 0 (- 1 / sqrt 2) (- 1 / sqrt 2))
    , let
        V3 _ axis _ = axes3D
      in
        makeText grid axis (V3 0 1 0) (V3 (- 1 / sqrt 2) 0 (- 1 / sqrt 2))
    , let
        V3 _ _ axis = axes3D
      in
        makeText grid axis (V3 0 0 1) (V3 (- 1 / sqrt 2) (- 1 / sqrt 2) 0)
    ]
  )


scaleItems :: (Location -> Location) -> ([DisplayItem a Location], [DisplayText b Location]) -> ([DisplayItem a Location], [DisplayText b Location])
scaleItems f = (fmap f <$>) *** (fmap f <$>)


concat' :: [([a], [b])] -> ([a], [b])
concat' = (concat *** concat) . unzip


concatMap' :: (a -> ([b], [c])) -> [a] -> ([b], [c])
concatMap' = (concat' .) . map


presentContainer  :: Container -> ([DisplayItem (GridAlias, (Int, [Characteristic])) Location], [DisplayText VariableAlias Location])
presentContainer Singleton{..} =
  scaleItems (scaleToExtent extent) $ presentGrid grid
presentContainer Array{..} =
  concat' $ zipWith (\e -> scaleItems (scaleToExtent e) . presentGrid) extents grids
presentContainer Collection{..} =
  concat' $ zipWith (\e -> scaleItems (scaleToExtent e) . presentContainer) extents containeds


presentWorld :: World -> Presentation -> ([DisplayItem (GridAlias, (Int, [Characteristic])) Location], [DisplayText VariableAlias Location])
presentWorld world Presentation{..} =
  concatMap' (scaleItems (scaleToWorldExtent world) . presentContainer) containers

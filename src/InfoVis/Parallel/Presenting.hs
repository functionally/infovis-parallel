{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Presenting (
  linkPresentation
, presentGrid
, presentContainer
, presentWorld
) where


import Control.Arrow ((***))
import Data.Maybe (fromMaybe)
import Graphics.Rendering.OpenGL (PrimitiveMode(..))
import InfoVis.Parallel.Scaling (scaleToExtent, scaleToWorldExtent)
import InfoVis.Parallel.Types (Location)
import InfoVis.Parallel.Types.Dataset (RecordIdentifier, VariableAlias)
import InfoVis.Parallel.Types.Display (DisplayItem(..), DisplayText(..))
import InfoVis.Parallel.Types.Presentation (Axis(..), Characteristic, Container(..), Grid(..), GriddedLocation, GridAlias, Link(..), LinkAlias, Presentation(..))
import InfoVis.Parallel.Types.World (World)
import Linear.Affine (Point(..))
import Linear.V1 (V1(..))
import Linear.V2 (V2(..))
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


presentGrid :: Grid -> ([DisplayItem (GridAlias, (Int, [Characteristic])) Location], [DisplayText VariableAlias Location])
presentGrid LineGrid{..} =
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
        V1 Axis{..} = axes1D
        textContent = axisVariable
        textOrigin  = P $ V3 0 0             0
        textWidth   = P $ V3 1 0             0
        textHeight  = P $ V3 0 (- labelSize) 0
        textColor   = labelColor
        textSize    = labelSize
      in
        DisplayText{..}
    ]
  )
presentGrid g@RectangleGrid{..} =
  (
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
  , [
      let
        V2 Axis{..} _ = axes2D
        textContent   = axisVariable
        textOrigin    = P $ V3 0 0             0
        textWidth     = P $ V3 1 0             0
        textHeight    = P $ V3 0 (- labelSize) 0
        textColor     = labelColor
        textSize      = labelSize
      in
        DisplayText{..}
    , let
        V2 _ Axis{..} = axes2D
        textContent   = axisVariable
        textOrigin    = P $ V3 0             0 0
        textWidth     = P $ V3 0             1 0
        textHeight    = P $ V3 (- labelSize) 0 0
        textColor     = labelColor
        textSize      = labelSize
      in
        DisplayText{..}
    ]
  )
presentGrid g@BoxGrid{..} =
  (
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
  , [
      let
        V3 Axis{..} _ _ = axes3D
        textContent     = axisVariable
        textOrigin      = P $ V3 0                      0                      0
        textWidth       = P $ V3 1                      0                      0
        textHeight      = P $ V3 0                      (- labelSize / sqrt 2) (- labelSize / sqrt 2)
        textColor       = labelColor
        textSize        = labelSize
      in
        DisplayText{..}
    , let
        V3 _ Axis{..} _ = axes3D
        textContent     = axisVariable
        textOrigin      = P $ V3 0                      0 0
        textWidth       = P $ V3 0                      1 0
        textHeight      = P $ V3 (- labelSize / sqrt 2) 0 (- labelSize / sqrt 2)
        textColor       = labelColor
        textSize        = labelSize
      in
        DisplayText{..}
    , let
        V3 _ _ Axis{..} = axes3D
        textContent     = axisVariable
        textOrigin      = P $ V3 0                      0                      0
        textWidth       = P $ V3 0                      0                      1
        textHeight      = P $ V3 (- labelSize / sqrt 2) (- labelSize / sqrt 2) 0
        textColor       = labelColor
        textSize        = labelSize
      in
        DisplayText{..}
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

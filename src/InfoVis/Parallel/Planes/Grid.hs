{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Planes.Grid (
  Grids
, makeGrids
, drawSelector
, drawGrids
, GridsAction(..)
, updateGrids
, addPoints
) where


import Control.Applicative ((<$>), (<|>))
import Control.Exception (IOException, catch)
import Control.Monad (guard, zipWithM)
import Data.List.Util (domain)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import Data.Tuple.Util (fst3, snd3, trd3)
import Graphics.Rendering.Handa.Shape (Shape, drawShape, makeShape, remakeShape)
import Graphics.Rendering.Handa.Util (coneFaces)
import Graphics.Rendering.OpenGL (DataType(Float), GLfloat, PrimitiveMode(..), Vector3(..), Vertex3(..), color, preservingMatrix, rotate, scale, translate)
import Graphics.UI.GLUT (StrokeFont(Roman), fontHeight, renderString, stringWidth)
import InfoVis.Parallel.Planes.Configuration (Configuration(..))

import qualified Data.Set as Set (delete, empty, fromList, insert, intersection, map, member, notMember, null, partition, singleton, toList, union, unions)


data Grids =
  Grids
  {
    configuration :: Configuration
  , selector      :: Shape
  , grid          :: Shape
  , layers        :: [Layer]
  , projections   :: [Projection]
  }


data PlanesType = BackgroundPlanes | SelectedPlanes | HighlightedPlanes
  deriving (Bounded, Enum, Eq, Ord, Read, Show)


type Layer = (PlanesType, Cells, Shape)


type Cell = (Int, Int, Int)


type Cells = Set Cell


type Projection = (PlanesType, Points, Shape)


type Points = Set [Double]


allCells :: Configuration -> Cells
allCells Configuration{..} =
  Set.fromList
  [
    (i, j, k)
  |
    i <- [1..planes]
  , j <- [1..divisions]
  , k <- [1..divisions]
  ]


addPoints :: Grids -> [[Double]] -> IO Grids
addPoints grids@Grids{..} rawPoints =
  do
    projections' <- zipWithM (remakeProjection configuration) [Set.fromList rawPoints, Set.empty, Set.empty] projections
    remakeGrids (map snd3 layers)
      $ grids {projections = projections'}


remakeProjection :: Configuration -> Points -> Projection -> IO Projection
remakeProjection configuration points' (planesType, _, shape) =
  (planesType, points', )
     <$> remakeLines configuration shape points'


makeGrids :: Configuration -> IO Grids
makeGrids configuration =
  do
    selector <- makeSelector configuration
    grid <- makeGrid configuration
    let
      makeLayer :: PlanesType -> Cells -> IO Layer
      makeLayer planeType cells = (planeType, cells, ) <$> makePlanes configuration planeType cells
      makeProjection :: PlanesType -> IO Projection
      makeProjection planeType = (planeType, Set.empty, ) <$> makeLines configuration planeType Set.empty
    layers <- zipWithM makeLayer domain [allCells configuration, Set.empty, Set.empty]
    projections <- mapM makeProjection domain
    return Grids{..}


remakeGrids :: [Cells] -> Grids -> IO Grids
remakeGrids cellses grids@Grids{..} =
  do
    layers' <- zipWithM (remakeLayer configuration) cellses layers
    let
      pointses = categorizePoints configuration (tail $ map snd3 layers') (Set.unions $ map snd3 projections)
    projections' <- zipWithM (remakeProjection configuration) pointses projections
    return $ grids {layers = layers', projections = projections'}


categorizePoints :: Configuration -> [Cells] -> Points -> [Points]
categorizePoints Configuration{..} [selected, highlighted] points =
  let
    quantize :: Double -> Int
    quantize = ceiling . (fromIntegral divisions *)
    quantize' :: Int -> [Double] -> [Cell]
    quantize' n (x : y : xys) =
      (n, quantize y, quantize (1 - x)) : quantize' (n + 1) xys
    quantize' _ _ = undefined
    checkHighlight :: [Double] -> Bool
    checkHighlight = Set.null . Set.intersection highlighted . Set.fromList . take planes . quantize' 1
    selectedPlanes :: Set Int
    selectedPlanes = Set.map fst3 selected
    checkSelect :: [Double] -> Bool
    checkSelect = all (\cell -> fst3 cell `Set.notMember` selectedPlanes || cell `Set.member` selected) . take planes . quantize' 1
    (candidatePoints, highlightedPoints) = Set.partition checkHighlight points
    (selectedPoints, normalPoints) = Set.partition checkSelect candidatePoints
  in
    [normalPoints, selectedPoints, highlightedPoints]
categorizePoints _ _ _ = undefined


remakeLayer :: Configuration -> Cells -> Layer -> IO Layer
remakeLayer configuration cells' layer@(planesType, cells, shape) =
  if cells' == cells
    then return layer
    else (planesType, cells', ) <$> remakePlanes configuration shape cells'


data GridsAction = SelectGrids | DeselectGrids | HighlightGrids | ClearGrids
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

  
updateGrids :: GridsAction -> Vector3 GLfloat -> Grids -> IO Grids
updateGrids gridsAction location grids@Grids{..} =
  let
    cellses  = map snd3 layers
    selection = findSelection configuration location
  in
    fromMaybe (return grids)
      $ flip remakeGrids grids
        <$> (
              (guard (gridsAction == ClearGrids) >> updateCells cellses gridsAction undefined)
              <|>
              (selection >>= updateCells cellses gridsAction)
              <|>
              (guard (isNothing selection) >> clearHighlights cellses)
            )


clearHighlights :: [Cells] -> Maybe [Cells]
clearHighlights [background, selected, highlighted] =
  do
    guard $ not $ Set.null highlighted
    return [background `Set.union` highlighted, selected, Set.empty]
clearHighlights _ = undefined

    
updateCells :: [Cells] -> GridsAction -> Cell -> Maybe [Cells]
updateCells [background, selected, highlighted] SelectGrids cell =
  do
    guard $ cell `Set.notMember` selected
    return [cell `Set.delete` (background `Set.union` highlighted), cell `Set.insert` selected, Set.empty]
updateCells [background, selected, highlighted] DeselectGrids cell =
  do
    guard $ cell `Set.member` selected
    return [background `Set.union` highlighted, cell `Set.delete` selected, Set.singleton cell]
updateCells [background, selected, highlighted] HighlightGrids cell =
  do
    guard $ cell `Set.member` background
    return [(cell `Set.delete` background) `Set.union` highlighted, selected, Set.singleton cell]
updateCells cellses ClearGrids _ =
  Just [Set.unions cellses, Set.empty, Set.empty]
updateCells _ _ _ = undefined


findSelection :: Configuration -> Vector3 GLfloat -> Maybe Cell
findSelection configuration@Configuration{..} (Vector3 x y z) =
  do
    let
      (spacing, size) = spacingAndSize configuration
      i = (x + 1                    ) / spacing + 1
      j = (y * realToFrac aspect + 1) / size    + 0.5
      k = (z + 1                    ) / size    + 0.5
    guard $ i > 0.9 && i < fromIntegral planes    + 0.1 && abs (i - fromIntegral (round i :: Int)) < 0.1
    guard $ j > 0.9 && j < fromIntegral divisions + 0.1
    guard $ k > 0.9 && k < fromIntegral divisions + 0.1
    return (round i, round j, round k)


drawSelector :: Grids -> IO ()
drawSelector Grids{..} =
  drawShape selector


drawGrids :: Grids -> IO ()
drawGrids Grids{..} =
  preservingMatrix $ do
    scale 1 (1 / aspect configuration) 1
    mapM_ (drawShape . trd3) $ reverse projections
    drawShape grid
    mapM_ (drawShape . trd3) layers
    drawAxisLabels configuration $ axisLabels configuration


drawAxisLabels :: Configuration -> [String] -> IO ()
drawAxisLabels configuration@Configuration{..} labels =
  do
    h <-
     catch (fontHeight Roman)
       ((\_ -> fromIntegral <$> stringWidth Roman "wn") :: IOException -> IO GLfloat)
    let
      (spacing, _) = spacingAndSize configuration
      s = 0.05 * 2
      r = s / h
    color labelColor
    preservingMatrix $ do
      translate (Vector3 (-1) (-1) 1 :: Vector3 GLfloat)
      rotate 90 (Vector3 0 1 0 :: Vector3 GLfloat)
      drawNextLabels s r spacing labels


drawNextLabels :: GLfloat -> GLfloat -> GLfloat -> [String] -> IO ()
drawNextLabels s r spacing (s1 : s2 : ss) =
  do
    preservingMatrix $ do
      translate $ Vector3 0 (-1.2 * s) 0
      scale r r r 
      renderString Roman s1
    preservingMatrix $ do
      rotate 90 (Vector3 0 0 1 :: Vector3 GLfloat)
      translate $ Vector3 0 (0.5 * s) 0
      scale r r r
      renderString Roman s2
    translate $ Vector3 0 0 spacing
    drawNextLabels s r spacing ss
drawNextLabels _ _ _ _ = return ()

  
makeSelector :: Configuration -> IO Shape
makeSelector Configuration{..} =
  let
    faces = concat $ coneFaces selectorHeight selectorRadius
  in
    makeShape 3 Float Triangles (map (\(x,y,z) -> Vertex3 x y z) faces)
      $ color selectorColor


spacingAndSize :: Configuration -> (GLfloat, GLfloat)
spacingAndSize Configuration{..} =
  (
    2 / fromIntegral (planes - 1)
  , 2 / fromIntegral divisions
  )


makeGrid :: Configuration -> IO Shape
makeGrid configuration@Configuration{..} =
  let
    (spacing, size) = spacingAndSize configuration
    grid =
      concat [
        concat $
        [
          [Vector3 x y (-1), Vector3 x y 1]
        |
          j <- [0..divisions]
        , let y = fromIntegral j * size - 1
        ]
        ++
        [
          [Vector3 x (-1) z, Vector3 x 1 z]
        |
          k <- [0..divisions]
        , let z = fromIntegral k * size - 1
        ]
      |
        i <- [1..planes]
      , let x = (fromIntegral i - 1) * spacing - 1
      ]
   in
     makeShape 3 Float Lines grid
       $ color gridColor


makePlanes :: Configuration -> PlanesType -> Cells -> IO Shape
makePlanes configuration planesType visibles =
  makeShape 3 Float Quads (planesGeometry configuration visibles)
    $ colorPlanes configuration planesType


remakePlanes :: Configuration -> Shape -> Cells -> IO Shape
remakePlanes configuration shape visibles =
  remakeShape shape (planesGeometry configuration visibles)
   

colorPlanes :: Configuration -> PlanesType -> IO ()
colorPlanes Configuration{..} planesType =
  color
    $ case planesType of
        BackgroundPlanes  -> planeFaceColor
        SelectedPlanes    -> planeSelectColor
        HighlightedPlanes -> planeHighlightColor


planesGeometry :: Configuration -> Cells -> [Vertex3 GLfloat]
planesGeometry configuration@Configuration{..} visibles =
  let
    (spacing, size) = spacingAndSize configuration
  in
    concat [
      [
        Vertex3 x y  z
      , Vertex3 x y' z
      , Vertex3 x y' z'
      , Vertex3 x y  z'
      ]
    |
      i <- [1..planes]
    , j <- [1..divisions]
    , k <- [1..divisions]
    , let x  = (fromIntegral i - 1) * spacing - 1
          y  = (fromIntegral j - 1) * size    - 1
          z  = (fromIntegral k - 1) * size    - 1
          y' =  fromIntegral j      * size    - 1
          z' =  fromIntegral k      * size    - 1
    , (i, j, k) `Set.member` visibles
    ]


makeLines :: Configuration -> PlanesType -> Points -> IO Shape
makeLines configuration planesType points =
  makeShape 3 Float Lines (concatMap (linesGeometry configuration) $ Set.toList points)
    $ colorLines configuration planesType


remakeLines :: Configuration -> Shape -> Points -> IO Shape
remakeLines configuration shape points =
  remakeShape shape
    $ concatMap (linesGeometry configuration)
    $ Set.toList points
   

colorLines :: Configuration -> PlanesType -> IO ()
colorLines Configuration{..} planesType =
  color
    $ case planesType of
        BackgroundPlanes  -> lineNormalColor
        SelectedPlanes    -> lineSelectedColor
        HighlightedPlanes -> lineHighlightColor


linesGeometry :: Configuration -> [Double] -> [Vertex3 GLfloat]
linesGeometry configuration@Configuration{..} points =
  let
    (spacing, _) = spacingAndSize configuration
    pointGeometry (u : v : ws) n =
      p : p : pointGeometry ws (n + spacing)
        where 
          p = Vertex3 n (realToFrac $ 2 * v - 1) (realToFrac $ 1 - 2 * u)
    pointGeometry _ _ = undefined
  in
    init $ tail $ take (2 * planes) $ pointGeometry points (-1) 

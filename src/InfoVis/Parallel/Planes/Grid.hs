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
import Control.Monad (guard, zipWithM)
import Data.List.Util (domain)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Tuple.Util (snd3, trd3)
import Graphics.Rendering.Handa.Shape (Shape, drawShape, makeShape, remakeShape)
import Graphics.Rendering.Handa.Util (coneFaces)
import Graphics.Rendering.OpenGL (DataType(Float), GLfloat, PrimitiveMode(..), Vector3(..), Vertex3(..), color, preservingMatrix, scale)
import InfoVis.Parallel.Planes.Configuration (Configuration(..))

import qualified Data.Set as Set (delete, empty, fromList, insert, member, notMember, null, singleton, toList, union, unions)


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
  let
    remakeProjection :: Points -> Projection -> IO Projection
    remakeProjection points' (planesType, _, shape) = (planesType, points', ) <$> remakeLines configuration shape points'
  in do
    projections' <- zipWithM remakeProjection [Set.fromList rawPoints, Set.empty, Set.empty] projections
    return $ grids {projections = projections'}


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
  let
    remakeLayer :: Cells -> Layer -> IO Layer
    remakeLayer cells' layer@(planesType, cells, shape) =
      if cells' == cells
        then return layer
        else (planesType, cells', ) <$> remakePlanes configuration shape cells'
  in do
    layers' <- zipWithM remakeLayer cellses layers
    return $ grids {layers = layers'}


data GridsAction = SelectGrids | DeselectGrids | HighlightGrids | ClearGrids
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

  
updateGrids :: GridsAction -> Vector3 GLfloat -> Grids -> IO Grids
updateGrids gridsAction location grids@Grids{..} =
  let
    cellses  = map snd3 layers
  in
    fromMaybe (return grids)
      $ flip remakeGrids grids
        <$> (
              (updateCells cellses gridsAction =<< findSelection configuration location)
              <|>
              clearHighlights cellses
            )


clearHighlights :: [Cells] -> Maybe [Cells]
clearHighlights [background, selected, highlighted] =
  do
    guard $ not $ Set.null highlighted
    return [background `Set.union` highlighted, selected, Set.empty]
clearHighlights _ = undefined

    
updateCells :: [Cells] -> GridsAction -> Cell -> Maybe [Cells]
updateCells [background, selected, _] SelectGrids cell =
  do
    guard $ cell `Set.notMember` selected
    return [cell `Set.delete` background, cell `Set.insert` selected, Set.empty]
updateCells [background, selected, _] DeselectGrids cell =
  do
    guard $ cell `Set.member` selected
    return [background, cell `Set.delete` selected, Set.singleton cell]
updateCells [background, selected, highlighted] HighlightGrids cell =
  do
    guard $ cell `Set.member` background
    return [(cell `Set.delete` background) `Set.union` highlighted, cell `Set.delete` selected, Set.singleton cell]
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
    drawShape grid
    mapM_ (drawShape . trd3) layers
    mapM_ (drawShape . trd3) projections


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
          p = Vertex3 n (realToFrac $ 2 * u - 1) (realToFrac $ 2 * v - 1)
    pointGeometry _ _ = undefined
  in
    init $ tail $ take (2 * planes) $ pointGeometry points (-1) 

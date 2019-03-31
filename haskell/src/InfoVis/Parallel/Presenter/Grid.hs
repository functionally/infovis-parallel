{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Presenter.Grid (
  Projectable(..)
, Presentable(..)
, GridIdentifier
, GridDisplay
, GridAlias
, Grid(..)
, Styling(..)
) where


import Control.Lens.Getter ((^.))
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Binary (Binary)
import Data.Default (def)
import GHC.Generics (Generic)
import InfoVis.Parallel.Dataset (Record, Variable)
import InfoVis.Parallel.Presenter.Axes (Axis(..), Axes1D, Axes2D, Axes3D, AxesProjectable(..))
import InfoVis.Parallel.Types (Color, Geometry(..), Position)
import Linear.Affine (Point(..), (.+^), (.-^))
import Linear.V1 (R1(..))
import Linear.V2 (R2(..))
import Linear.V3 (R3(..), V3(..))
import Linear.Vector ((*^), basis, zero)

import qualified InfoVis.Parallel.Types as S (Shape(Axis, Polylines, Rectangles, Label))


class Projectable a where
  project :: a -> [Variable] -> Record -> [(GridAlias, Position)]


class Presentable a where
  present :: a -> GridDisplay


ex, ey, ez :: V3 Double
[ex, ey, ez] = basis


type GridIdentifier = (GridAlias, Int)


type GridDisplay = [(GridIdentifier, Geometry)]


type GridAlias = String


data Grid =
    LineGrid
    {
      gridAlias   :: GridAlias
    , axes1D      :: Axes1D
    , divisions   :: Int
    , axisColor   :: Color
    , axisSize    :: Double
    , lineStyling :: Styling
    , labelColor  :: Color
    , labelSize   :: Double
    }
  | RectangleGrid
    {
      gridAlias   :: GridAlias
    , axes2D      :: Axes2D
    , divisions   :: Int
    , axisColor   :: Color
    , axisSize    :: Double
    , lineStyling :: Styling
    , faceStyling :: Styling
    , labelColor  :: Color
    , labelSize   :: Double
    }
  | BoxGrid
    {
      gridAlias   :: GridAlias
    , axes3D      :: Axes3D
    , divisions   :: Int
    , axisColor   :: Color
    , axisSize    :: Double
    , lineStyling :: Styling
    , faceStyling :: Styling
    , labelColor  :: Color
    , labelSize   :: Double
    }
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)


instance Projectable Grid where
  project LineGrid{..}      variables = (: []) . (gridAlias, ) . projectAxes axes1D variables
  project RectangleGrid{..} variables = (: []) . (gridAlias, ) . projectAxes axes2D variables
  project BoxGrid{..}       variables = (: []) . (gridAlias, ) . projectAxes axes3D variables


instance Presentable Grid where

  present LineGrid{..} =
    let
      axes =
        [
          label (axes1D ^. _x) zero ex ey labelColor labelSize
        , axis  (axes1D ^. _x) zero ex    axisColor  axisSize
        ]
      deltaX = ex / (1 + fromIntegral divisions)
      edges =
        arrayed divisions deltaX (makeLine lineStyling deltaX) zero
    in
      number gridAlias $ edges ++ axes
  
  present RectangleGrid{..} =
    let
      axes =
        [
          label (axes2D ^. _x) zero ex ey labelColor labelSize
        , axis  (axes2D ^. _x) zero ex    axisColor  axisSize
        , label (axes2D ^. _y) zero ey ex labelColor labelSize
        , axis  (axes2D ^. _y) zero ey    axisColor  axisSize
        ]
      deltaX = ex / (1 + fromIntegral divisions)
      deltaY = ey / (1 + fromIntegral divisions)
      faces =
        arrayed divisions deltaX
          (
            arrayed divisions deltaY
              $ makeRectangle faceStyling deltaX deltaY
          ) zero
      edges = 
           set deltaX deltaY
        ++ set deltaY deltaX
          where
            set u v =
              arrayed (divisions + 1) u
                (
                  arrayed divisions v
                    $ makeLine lineStyling v
                ) zero
    in
      number gridAlias $ faces ++ edges ++ axes
  
  present BoxGrid{..} =
    let
      axes =
        [
          label (axes3D ^. _x) zero ex ((ey + ez) / sqrt 2) labelColor labelSize
        , axis  (axes3D ^. _x) zero ex                      axisColor  axisSize
        , label (axes3D ^. _y) zero ey ((ez + ex) / sqrt 2) labelColor labelSize
        , axis  (axes3D ^. _y) zero ey                      axisColor  axisSize
        , label (axes3D ^. _z) zero ez ((ex + ey) / sqrt 2) labelColor labelSize
        , axis  (axes3D ^. _z) zero ez                      axisColor  axisSize
        ]
      deltaX = ex / (1 + fromIntegral divisions)
      deltaY = ey / (1 + fromIntegral divisions)
      deltaZ = ez / (1 + fromIntegral divisions)
      faces =
           set deltaX deltaY deltaZ
        ++ set deltaY deltaZ deltaX
        ++ set deltaZ deltaX deltaY
          where
            set u v w =
              arrayed (divisions + 1) u
                (
                  arrayed divisions v
                    (
                      arrayed divisions w
                        $ makeRectangle faceStyling v w
                    )
                ) zero
      edges = 
           set deltaX deltaY deltaZ
        ++ set deltaY deltaZ deltaX
        ++ set deltaZ deltaX deltaY
          where
            set u v w =
              arrayed (divisions + 1) u
                (
                  arrayed (divisions + 1) v
                    (
                      arrayed divisions w
                        $ makeLine lineStyling w
                    )
                ) zero
    in
      number gridAlias $ faces ++ edges ++ axes


number :: GridAlias
       -> [Geometry]
       -> GridDisplay
number alias elements =
  [
    ((alias, n), element)
  |
    (n, element) <- zip[1..] elements
  ]


arrayed :: Int
        -> V3 Double
        -> (Point V3 Double -> [Geometry])
        -> Point V3 Double
        -> [Geometry]
arrayed divisions deltaX f origin =
  concatMap
    (f . (origin .+^) . (*^ deltaX) . fromIntegral)
    [0..divisions]


makeLine :: Styling
         -> V3 Double
         -> Point V3 Double
         -> [Geometry]
makeLine Styling{..} deltaX origin =
  [
    def
    {
      shape = S.Polylines [[origin, origin .+^ deltaX]]
    , color = normalColor
    , size  = thickness
    }
  ]


makeRectangle :: Styling
              -> V3 Double
              -> V3 Double
              -> Point V3 Double
              -> [Geometry]
makeRectangle Styling{..} deltaX deltaY origin =
  [
    def
    {
      shape = S.Rectangles [(origin, origin .+^ deltaX, origin .+^ deltaY)]
    , color = normalColor
    , size  = thickness
    }
  ]


axis :: Axis
     -> Point V3 Double
     -> V3 Double
     -> Color
     -> Double
     -> Geometry
axis Axis{..} origin deltaX color' size' =
  def
  {
    shape = S.Axis (origin, origin .+^ 1.10 *^ deltaX)
  , color = color'
  , size  = size'
  , text  = axisVariable
  }


label :: Axis
      -> Point V3 Double
      -> V3 Double
      -> V3 Double
      -> Color
      -> Double
      -> Geometry
label Axis{..} origin deltaX deltaY color' size' =
  Geometry
  {
    shape = let
              origin' = origin .-^ abs size' *^ deltaY
            in
              if size' > 0
              then S.Label (origin', origin' .+^ deltaX, origin )
              else S.Label (origin , origin  .+^ deltaX, origin')
  , color = color'
  , size  = abs size'
  , text  = axisVariable
  }


data Styling =
  Styling
  {
    normalColor    :: Color
  , selectColor    :: Color
  , highlightColor :: Color
  , thickness      :: Double
  }
    deriving (Binary, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

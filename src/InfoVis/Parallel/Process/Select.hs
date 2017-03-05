{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.Select (
  selecter
) where


import Control.Arrow (second)
import Data.Function.MapReduce (mapReduce)
import Graphics.Rendering.OpenGL (Vertex3(..))
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import InfoVis.Parallel.Process.DataProvider (GridsLinks)
import InfoVis.Parallel.Types (Coloring(..))
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Display (DisplayList(..))
import InfoVis.Parallel.Types.Message (SelectionAction(..))
import InfoVis.Parallel.Types.Scaffold (Presentation(..), World(..))
import Linear.Affine (Point(..), (.-.))
import Linear.Conjugate (conjugate)
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))

import qualified Linear.Quaternion as Q (rotate)


selecter :: Configuration Double
         -> GridsLinks
         -> ([(Int, Coloring)], [(Int, Coloring)])
         -> (Point V3 Double, SelectionAction)
         -> (Point V3 Double, Quaternion Double)
         -> (([(Int, Coloring)], [(Int, Coloring)]), [(Int, Coloring)])
selecter Configuration{..} (_, links) (persistentColorings, transientColorings) (P location, press) (P location', orientation') =
  let
    V3 x y z = realToFrac <$> (P $ conjugate orientation' `Q.rotate` location) .-. P location'
    d = realToFrac $ selectorSize presentation * baseSize world / 2
    h w w' = abs (w - w' + d) <= d
    f (Vertex3 x' y' z') = h x x' && h y y' && h z z'
    g DisplayList{..} = map (second f) $ zip listVertexIdentifiers listVertices
    selections = mapReduce id (curry $ second or) $ concatMap g links :: [(Int, Bool)]
    persistentColorings' =
      case press of
        Highlight       -> persistentColorings
        Deselection     -> zipWith (\(i, o) (_, n) -> (i, if n then NormalColoring    else o)) persistentColorings selections
        Selection       -> zipWith (\(i, o) (_, n) -> (i, if n then SelectColoring    else o)) persistentColorings selections
        Clear           -> map (\(i, _) -> (i, NormalColoring))                                persistentColorings
    transientColorings'  = zipWith (\(i, o) (_, n) -> (i, if n then HighlightColoring else o)) persistentColorings selections
  in
    (
      (persistentColorings', transientColorings')
    , fmap snd $ filter (uncurry (/=)) $ zip transientColorings transientColorings'
    )

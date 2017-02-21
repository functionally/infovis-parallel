{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Primitive (
  fromLocation
, fromLocations
, stripIdentifiers
, partitionPrimitives
) where


import Graphics.Rendering.OpenGL (GLfloat, PrimitiveMode(..), Vertex3(..))
import InfoVis.Parallel.Types (Location)
import InfoVis.Parallel.Types.Display (DisplayItem(..))
import Linear.Affine (Point(..))
import Linear.V3 (V3(..))


type Primitive3D = Vertex3 GLfloat


fromLocation :: Location -> Primitive3D
fromLocation (P (V3 x y z)) = realToFrac <$> Vertex3 x y z


fromLocations :: [DisplayItem a Location] -> [DisplayItem a Primitive3D]
fromLocations = fmap (fmap fromLocation)


stripIdentifiers :: [DisplayItem a Location] -> [DisplayItem () Primitive3D]
stripIdentifiers = fmap $ \DisplayItem{..} -> DisplayItem {itemIdentifier = (), itemPrimitive = itemPrimitive, itemVertices = fromLocation <$> itemVertices}


partitionPrimitives :: [DisplayItem a b] -> ([b], [b], [b])
partitionPrimitives = partitionPrimitives' ([], [], [])

partitionPrimitives' :: ([b], [b], [b]) -> [DisplayItem a b] -> ([b], [b], [b])
partitionPrimitives' (ps, ls, qs) [] = (ps, ls, qs)
partitionPrimitives' (ps, ls, qs) (DisplayItem _ Points vs : dis) = partitionPrimitives' (vs ++ ps,       ls,       qs) dis
partitionPrimitives' (ps, ls, qs) (DisplayItem _ Lines  vs : dis) = partitionPrimitives' (      ps, vs ++ ls,       qs) dis
partitionPrimitives' (ps, ls, qs) (DisplayItem _ Quads  vs : dis) = partitionPrimitives' (      ps,       ls, vs ++ qs) dis

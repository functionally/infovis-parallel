module Data.Math.Util (
  oneDegree
, fromDegrees
, toDegrees
, cosd
, sind
) where


-- | One degree of arc.
oneDegree :: Floating a => a
oneDegree = 180 / pi


fromDegrees :: (Floating a, Num a) => a -> a
fromDegrees = (/ oneDegree)


toDegrees :: (Floating a, Num a) => a -> a
toDegrees = (* oneDegree)


-- | Cosine with its argument in degrees.
cosd :: Floating a => a -> a
cosd = cos . (/ oneDegree)


-- | Sine with its argument in degrees.
sind :: Floating a => a -> a
sind = sin . (/ oneDegree)

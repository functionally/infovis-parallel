module Linear.Util (
  rotationFromVectorPair
, projectPlane
, rotationFromVectorPairs
, rotationFromPlane
, fromEuler
, fromEulerd
) where


import Data.Math.Util (fromDegrees)
import Linear.Affine (Point(..), (.-.))
import Linear.Conjugate (Conjugate, conjugate)
import Linear.Epsilon (Epsilon)
import Linear.Metric (dot, normalize)
import Linear.Quaternion (Quaternion(..), axisAngle, rotate)
import Linear.V3 (V3(..), cross)
import Linear.Vector ((^-^), (*^), basis)


rotationFromVectorPair :: (Epsilon a, Floating a) => V3 a -> V3 a -> Quaternion a
rotationFromVectorPair v1 v2 =
  let v1n = normalize v1
      v2n = normalize v2
      v12n = normalize $ v1n + v2n
  in
    Quaternion (v12n `dot` v2n) $ v12n `cross` v2n


projectPlane :: (Epsilon a, Floating a) => V3 a -> V3 a -> V3 a
projectPlane v u =
  let
    un = normalize u
  in
    v ^-^ (v `dot` un) *^ un


rotationFromVectorPairs :: (Conjugate a, Epsilon a, RealFloat a) => V3 a -> V3 a -> V3 a -> V3 a -> Quaternion a
rotationFromVectorPairs u0 v0 u2 v2 =
  let
    q2 = rotationFromVectorPair u0 u2
    v1 = conjugate q2 `rotate` v2
    v0p = v0 `projectPlane` u0
    v1p = v1 `projectPlane` u0
    q1 = rotationFromVectorPair v0p v1p
  in
    normalize $ q2 * q1


rotationFromPlane :: (Conjugate a, Epsilon a, RealFloat a) => V3 a -> V3 a -> Point V3 a -> Point V3 a -> Point V3 a -> Quaternion a
rotationFromPlane xAxis yAxis origin xPoint yPoint =
  rotationFromVectorPairs
    xAxis
    yAxis
    (xPoint .-. origin)
    (yPoint .-. origin)


{-
http://robokitchen.tumblr.com/post/67060392720/finding-a-rotation-quaternion-from-two-pairs-of
http://stackoverflow.com/questions/4670070/deriving-axis-angle-rotation-from-two-pairs-of-three-points-or-two-pairs-of-tw
Before rotation: u0, v0. After rotation: u2, v2.
Quaternion q2 = Quaternion::fromTwoVectors(u0, u2);
Vector v1 = v2.rotate(q2.conjugate());
Vector v0_proj = v0.projectPlane(u0);
Vector v1_proj = v1.projectPlane(u0);
Quaternion q1 = Quaternion::fromTwoVectors(v0_proj, v1_proj);
return (q2 * q1).normalized();
-}


fromEuler :: (Epsilon a, Num a, RealFloat a) => V3 a -> Quaternion a
fromEuler (V3 phi theta psi) =
  let
    [ex, ey, ez] = basis
  in
    ez `axisAngle` psi * ey `axisAngle` theta * ex `axisAngle` phi


fromEulerd :: (Epsilon a, Num a, RealFloat a) => V3 a -> Quaternion a
fromEulerd = fromEuler . fmap fromDegrees

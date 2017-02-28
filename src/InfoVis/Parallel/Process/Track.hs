{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Process.Track (
  trackPov
, trackRelocation
, trackSelection
) where


import Control.Arrow (first, second)
import Control.Concurrent (MVar, putMVar)
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef)
import Graphics.Rendering.Handa.Util (degree)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import InfoVis.Parallel.Process.Display (SelectionAction(..))
import InfoVis.Parallel.Types.Configuration (Input(..))
import Linear.Affine (Point(..))
import Linear.Epsilon (Epsilon)
import Linear.Quaternion (Quaternion(..), axisAngle)
import Linear.V3 (V3(..))
import Linear.Vector (basis)
import Network.UI.Kafka (consumerLoop)
import Network.UI.Kafka.Types (Button(IndexButton), Event(..), Toggle(..))


type PointOfView a = (Point V3 a, Quaternion a)


fromDegrees :: (Floating a, Num a) => a -> a
fromDegrees = (/ degree)


fromEuler :: (Epsilon a, Num a, RealFloat a) => V3 a -> Quaternion a
fromEuler (V3 phi theta psi) =
  let
    [ex, ey, ez] = basis
  in
    ez `axisAngle` psi * ey `axisAngle` theta * ex `axisAngle` phi


processInput' :: MVar () -> (a -> b -> IO ()) -> a -> b -> IO ()
processInput' flag processInput s e =
  do
    processInput s e
    putMVar flag ()

    
trackPov :: (Epsilon a, Fractional a, RealFloat a) => Input -> IORef (PointOfView a) -> MVar () -> IO ()
trackPov Input{..} pov flag =
  case povInput of
    Left (location, orientation) -> modifyIORef pov $ const (P $ realToFrac <$> location, fromEuler $ realToFrac . fromDegrees <$> orientation)
    Right povInput'              -> do
                                      let
                                        processInput sensor (LocationEvent (x, y, z)) =
                                          when (sensor == povInput')
                                            $ modifyIORef pov
                                            $ first
                                            $ const
                                            $ realToFrac
                                            <$> P (V3 x y z)
                                        processInput sensor (OrientationEvent (w, x, y, z)) =
                                          when (sensor == povInput')
                                            $ modifyIORef pov
                                            $ second
                                            $ const
                                            $ realToFrac
                                            <$> Quaternion w (V3 x y z)
                                        processInput _ _ =
                                          return ()
                                      (_, loop) <- consumerLoop kafka $ processInput' flag processInput
                                      result <- loop
                                      either print return result


trackRelocation :: (Epsilon a, Fractional a, RealFloat a) => Input -> IORef (V3 a, Quaternion a) -> MVar () -> IO ()
trackRelocation Input{..} relocation flag =
  do
    let
      processInput sensor (LocationEvent (x, y, z)) =
        when (sensor == relocationInput)
          $ modifyIORef relocation
          $ first
          $ const
          $ realToFrac
          <$> V3 x y z
      processInput sensor (OrientationEvent (w, x, y, z)) =
        when (sensor == relocationInput)
          $ modifyIORef relocation
          $ second
          $ const
          $ realToFrac
          <$> Quaternion w (V3 x y z)
      processInput _ _ =
        return ()
    (_, loop) <- consumerLoop kafka $ processInput' flag processInput
    result <- loop
    either print return result


trackSelection :: (Epsilon a, Fractional a, RealFloat a) => Input -> IORef (V3 a, SelectionAction) -> MVar () -> IO ()
trackSelection Input{..} selection flag =
  do
    let
      processInput sensor (LocationEvent (x, y, z)) =
        when (sensor == selectorInput)
          $ modifyIORef selection
          $ first 
          $ const
          $ realToFrac
          <$> V3 x y z
      processInput sensor (PointerEvent (x, y, z)) =
        when (sensor == selectorInput)
          $ modifyIORef selection
          $ first
          $ const
          $ realToFrac
          <$> V3 x y z
      processInput sensor (ButtonEvent (IndexButton i, Down)) =
        when ((sensor, i) `elem` [selectButton, deselectButton, clearButton])
         $ modifyIORef selection
         $ second
         $ const
         $ if (sensor, i) == selectButton then Select else (if (sensor, i) == deselectButton then Deselect else Clear)
      processInput _ _ =
        return ()
    (_, loop) <- consumerLoop kafka $ processInput' flag processInput
    result <- loop
    either print return result

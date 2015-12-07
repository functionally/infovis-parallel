{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Data.Binary.Instances (
) where


import Data.Binary (Binary(..))
import Data.Tuple.Util (uncurry3)
import Graphics.Rendering.OpenGL (GLdouble, GLfloat, Vector3(..))


instance Binary GLfloat where
  put = put . decodeFloat
  get = uncurry encodeFloat <$> get


instance Binary GLdouble where
  put = put . decodeFloat
  get = uncurry encodeFloat <$> get


instance Binary a => Binary (Vector3 a) where
  put (Vector3 x y z) = put (x, y, z)
  get = uncurry3 Vector3 <$> get

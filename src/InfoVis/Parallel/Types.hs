{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


module InfoVis.Parallel.Types (
  Location
, Color
, Coloring(..)
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Linear.Affine (Point(..))
import Linear.V1 (V1)
import Linear.V2 (V2)
import Linear.V3 (V3)
import Graphics.Rendering.OpenGL (Color4(..), GLfloat)


instance FromJSON a => FromJSON (V1 a)

instance ToJSON a => ToJSON (V1 a)


instance FromJSON a => FromJSON (V2 a)

instance ToJSON a => ToJSON (V2 a)


instance FromJSON a => FromJSON (V3 a)

instance ToJSON a => ToJSON (V3 a)


instance FromJSON (V3 a) => FromJSON (Point V3 a)

instance ToJSON (V3 a) => ToJSON (Point V3 a)


type Location = Point V3 Double


instance Binary a => Binary (Color4 a)

deriving instance Generic (Color4 a)


type Color = Color4 GLfloat

instance FromJSON Color

instance ToJSON Color


data Coloring =
    NormalColoring
  | SelectColoring
  | HighlightColoring
  deriving (Binary, Bounded, Enum, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

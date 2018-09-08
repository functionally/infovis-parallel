{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


module Linear.Util.Instances (
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Linear.Affine (Point(..))
import Linear.V1 (V1)
import Linear.V2 (V2)
import Linear.V3 (V3)
import Linear.V4 (V4)


instance FromJSON a => FromJSON (V1 a)

instance ToJSON a => ToJSON (V1 a)


instance FromJSON a => FromJSON (V2 a)

instance ToJSON a => ToJSON (V2 a)


instance FromJSON a => FromJSON (V3 a)

instance ToJSON a => ToJSON (V3 a)


instance FromJSON a => FromJSON (V4 a)

instance ToJSON a => ToJSON (V4 a)


instance FromJSON (V3 a) => FromJSON (Point V3 a)

instance ToJSON (V3 a) => ToJSON (Point V3 a)


instance FromJSON (V4 a) => FromJSON (Point V4 a)

instance ToJSON (V4 a) => ToJSON (Point V4 a)

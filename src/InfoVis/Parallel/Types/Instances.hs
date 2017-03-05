{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


module InfoVis.Parallel.Types.Instances (
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Binary (Binary(..), getWord8, putWord8)
import GHC.Generics (Generic)
import Linear.Affine (Point(..))
import Linear.V1 (V1)
import Linear.V2 (V2)
import Linear.V3 (V3)
import Graphics.Rendering.OpenGL.GL.PrimitiveMode (PrimitiveMode(..))
import Graphics.Rendering.OpenGL.GL.VertexSpec (Color4(..))
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import Network.UI.Kafka (TopicConnection(..))


instance FromJSON a => FromJSON (V1 a)

instance ToJSON a => ToJSON (V1 a)


instance FromJSON a => FromJSON (V2 a)

instance ToJSON a => ToJSON (V2 a)


instance FromJSON a => FromJSON (V3 a)

instance ToJSON a => ToJSON (V3 a)


instance FromJSON (V3 a) => FromJSON (Point V3 a)

instance ToJSON (V3 a) => ToJSON (Point V3 a)


instance Binary a => Binary (Color4 a)

deriving instance Generic (Color4 a)

instance FromJSON a => FromJSON (Color4 a)

instance ToJSON a => ToJSON (Color4 a)


instance Binary PrimitiveMode where
  get = do
          i <- getWord8
          return
            $ case i of
              0  -> Points
              1  -> Lines        
              2  -> LineLoop     
              3  -> LineStrip    
              4  -> Triangles    
              5  -> TriangleStrip
              6  -> TriangleFan  
              7  -> Quads        
              8  -> QuadStrip    
              9  -> Polygon      
              10 -> Patches
              _  -> error "Unknown serialization of PrimitiveMode."
  put Points        = putWord8 0
  put Lines         = putWord8 1
  put LineLoop      = putWord8 2
  put LineStrip     = putWord8 3
  put Triangles     = putWord8 4
  put TriangleStrip = putWord8 5
  put TriangleFan   = putWord8 6
  put Quads         = putWord8 7
  put QuadStrip     = putWord8 8
  put Polygon       = putWord8 9
  put Patches       = putWord8 10


deriving instance Ord TopicConnection

deriving instance Binary TopicConnection

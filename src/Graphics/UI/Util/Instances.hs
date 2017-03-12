{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Graphics.UI.Util.Instances (
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Data (Data)
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL (Vector3(..), Vertex3(..))
import Graphics.Rendering.OpenGL.GL.PrimitiveMode (PrimitiveMode(..))
import Graphics.Rendering.OpenGL.GL.VertexSpec (Color4(..))


deriving instance Data a => Data (Vector3 a)

deriving instance Generic a => Generic (Vector3 a)

deriving instance (Binary a, Generic a) => Binary (Vector3 a)

deriving instance (FromJSON a, Generic a) => FromJSON (Vector3 a)

deriving instance (ToJSON a, Generic a) => ToJSON (Vector3 a)


deriving instance Data a => Data (Vertex3 a)

deriving instance Generic a => Generic (Vertex3 a)

deriving instance (Binary a, Generic a) => Binary (Vertex3 a)

deriving instance (FromJSON a, Generic a) => FromJSON (Vertex3 a)

deriving instance (ToJSON a, Generic a) => ToJSON (Vertex3 a)


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

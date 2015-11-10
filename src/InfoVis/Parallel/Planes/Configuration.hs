module InfoVis.Parallel.Planes.Configuration (
  Configuration(..)
) where


import Data.Default (Default(..))
import Graphics.Rendering.OpenGL (Color4(..), GLdouble, GLfloat)


data Configuration =
  Configuration
  {
    aspect              :: GLdouble
  , planes              :: Int
  , divisions           :: Int
  , gridColor           :: Color4 GLfloat
  , planeFaceColor      :: Color4 GLfloat
  , planeSelectColor    :: Color4 GLfloat
  , planeHighlightColor :: Color4 GLfloat
  , lineNormalColor     :: Color4 GLfloat
  , lineSelectedColor   :: Color4 GLfloat
  , lineHighlightColor  :: Color4 GLfloat
  , selectorColor       :: Color4 GLfloat
  , selectorRadius      :: GLfloat
  , selectorHeight      :: GLfloat
  }
    deriving (Eq, Read, Show)

instance Default Configuration where
  def =
    Configuration
    {
      aspect              = 1
    , planes              = 5
    , divisions           = 10
    , gridColor           = Color4 0.4 0.4 0.4 0.8
    , planeFaceColor      = Color4 0.2 0.2 0.2 0.6
    , planeSelectColor    = Color4 0.4 0.4 0.0 0.8
    , planeHighlightColor = Color4 0.4 0.0 0.0 0.8
    , lineNormalColor     = Color4 0.1 0.2 0.6 0.8
    , lineSelectedColor   = Color4 0.8 0.8 0.8 1.0
    , lineHighlightColor  = Color4 1.0 0.0 0.0 1.0
    , selectorColor       = Color4 0.5 0.3 0.2 1.0
    , selectorRadius      = 0.03
    , selectorHeight      = 0.12
    }

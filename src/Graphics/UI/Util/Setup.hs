{-# LANGUAGE RecordWildCards    #-}


module Graphics.UI.Util.Setup (
  Stereo(..)
, setup
, idle
) where


import Control.Monad (when, void)
import Graphics.Rendering.DLP (DlpEncoding)
import Graphics.Rendering.OpenGL (BlendingFactor(..), Capability(Enabled), ComparisonFunction(..), ($=), alphaFunc, blend, blendFunc)
import Graphics.UI.GLUT (DisplayMode(..), IdleCallback, createWindow, depthFunc, fullScreen, initialDisplayMode, initialize, postRedisplay)
import Graphics.UI.Util.Types (Display(..), Stereo(..), Viewers(..))

import qualified Graphics.Rendering.DLP as D (DlpEncoding(..))


setup :: Bool
      -> String
      -> String
      -> Viewers Double
      -> Int
      -> IO DlpEncoding
setup debug title program Viewers{..} displayIndex =
  do
    let
      Display{..} = displays !! displayIndex
      dlp = case stereo of
        DLP        -> D.FrameSequential
        QuadBuffer -> D.QuadBuffer
        Cardboard  -> D.SideBySide
        Mono       -> D.LeftOnly
    void
      . initialize program
      . (if debug then ("-gldebug" :) else id)
      . maybe id ((("-display"  :) .) .(:)) identifier
      $ case geometry of
          Nothing           -> []
          Just "fullscreen" -> []
          Just geometry'    -> ["-geometry", geometry']
    initialDisplayMode $=
      (if stereo == QuadBuffer then (Stereoscopic :) else id)
        [WithDepthBuffer, DoubleBuffered]
    void $ createWindow title
    when (geometry == Just "fullscreen")
      fullScreen
    depthFunc $= Just Less 
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    alphaFunc $= Just (Greater, 0)
    return dlp


-- | An idle callback that simply posts a request for redisplay.
idle :: IdleCallback
idle = postRedisplay Nothing

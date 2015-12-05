{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}


module Main (
  main
) where


import Control.Applicative ((<$>))
import Data.Data (Data)
import Data.Default (def)
import Data.Relational (cast, fromString)
import Data.Relational.Value (asRealFloat)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Graphics.UI.Handa.Setup (Stereo(..), Viewer(..))
import Paths_infovis_parallel (version)
import System.Console.CmdArgs ((&=), argPos, cmdArgs, details, help, modes, name, opt, program, summary, typ, typFile)

import qualified Graphics.UI.Handa.Setup as S (Setup(..))
import qualified InfoVis.Parallel.Planes.Control as PP (main)



data Parallel =
  ParallelPlanes
  {
    display    :: String
  , geometry   :: String
  , stereo     :: Stereo
  , viewer     :: Viewer
  , fullscreen :: Bool
  , invert     :: Bool
  , dataset    :: FilePath
  }
    deriving (Data, Show, Typeable)


parallel :: Parallel
parallel =
  modes
    [
      parallelPlanes
    ]
    &= summary ("Information Visualization in Parallel, Version " ++ showVersion version ++ " © 2015 Brian W Bush, All Rights Reserved")
    &= program "infovis-parallel"
    &= help "Information visualization in parallel."


parallelPlanes :: Parallel
parallelPlanes =
  ParallelPlanes
  {
    display     = def
               &= typ "HOST:DISPLAY.SCREEN" 
               &= help "The X11 display."
  , geometry    = def
               &= typ "WxH+X+Y"
               &= help "The X11 geometry."
  , stereo      = def
               &= typ "DLP|QuadBuffer|Cardboard|Mono"
               &= help "The stereo mode."
  , viewer      = def
               &= typ "Laptop|Desktop|Projector|Phone"
               &= help "The frustum and screen geometry."
  , fullscreen  = S.fullscreen def
               &= typ "BOOLEAN"
               &= help "Whether to open in full screen mode."
  , invert      = S.switchEyes def
               &= typ "BOOLEAN"
               &= help "Whether to switch the left and right eyes."
  , dataset     = def
               &= opt "/dev/stdin"
               &= typFile
               &= argPos 0
  }
    &= name "planes"
    &= help "Information visualization using parallel planes."
    &= details ["The input file (or /dev/stdin, if it is not present) must be a dataset in tab-separated-value format whose first row is its header."]


main :: IO ()
main = dispatch =<< cmdArgs parallel


dispatch :: Parallel -> IO ()
dispatch ParallelPlanes{..} =
  do
    let
      config =
        S.Setup
        {
          S.stereo     = stereo
        , S.switchEyes = invert
        , S.viewer     = viewer
        , S.fullscreen = fullscreen
        }
      arguments =
        (if display  == "" then id else ("-display"  :) . (display  :)) $
        (if geometry == "" then id else ("-geometry" :) . (geometry :))
        []
    x <- cast asRealFloat . fromString <$> readFile dataset
    PP.main "infovis-parallel" "Parallel Planes" arguments config x

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}


module Main (
  main
) where


import Control.Distributed.Process.Backend.SimpleWidenet (initializeBackend, startMaster, startSlave)
import Control.Distributed.Process.Node (initRemoteTable)
import Data.Data (Data)
import Data.Default (def)
import Data.Functor.Util (cast)
import Data.Relational.Lists (readTable)
import Data.Relational.Value (asRealFloat)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Data.Yaml (decodeFile)
import Graphics.UI.Handa.Setup (Stereo(..), Viewer(..))
import Paths_infovis_parallel (version)
import System.Console.CmdArgs ((&=), argPos, cmdArgs, details, help, modes, name, opt, program, summary, typ, typFile)

import qualified Graphics.UI.Handa.Setup as S (Setup(..))
import qualified InfoVis.Parallel.Planes.Control.Master as M (__remoteTable, master, peersList)
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
  | ParallelPlanesCave
    {
      host     :: String
    , port     :: String
    , displays :: FilePath
    , dataset  :: FilePath
    }
    deriving (Data, Show, Typeable)


parallel :: Parallel
parallel =
  modes
    [
      parallelPlanes
    , parallelPlanesCave
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
               &= typ "Laptop|Desktop|Projector|Phone|Glasses"
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


parallelPlanesCave :: Parallel
parallelPlanesCave =
  ParallelPlanesCave
  {
    host      = def
             &= typ "HOST"
             &= help "The host name."
  , port      = def
             &= typ "PORT"
             &= help "The port number."
  , displays  = def
             &= typFile
             &= help "YAML or JSON configuration for displays."
  , dataset   = def
             &= opt ""
             &= typFile
             &= argPos 0
  }
    &= name "planes-cave"
    &= help "Cave-based information visualization using parallel planes."
    &= details ["See the example <data/diagnostics.yaml> for an example display configuration file."]


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
        , S.viewer     = Right viewer
        , S.fullscreen = fullscreen
        }
      arguments =
        (if display  == "" then id else ("-display"  :) . (display  :)) $
        (if geometry == "" then id else ("-geometry" :) . (geometry :))
        []
    x <- cast asRealFloat <$> readTable dataset
    PP.main "infovis-parallel" "Parallel Planes" arguments config x

dispatch ParallelPlanesCave{..}
  | displays == "" = do
                       backend <- initializeBackend [] host' port' rtable
                       startSlave backend
  | otherwise      = do
                       Just configuration <- decodeFile displays
                       content <- cast asRealFloat <$> readTable (if dataset == "" then "/dev/stdin" else dataset)
                       backend <- initializeBackend (M.peersList configuration) host' port' rtable
                       (`startMaster` M.master configuration content) backend
    where
      host' = if host == "" then "localhost" else host
      port' = if port == "" then "44444"     else port
      rtable = M.__remoteTable initRemoteTable

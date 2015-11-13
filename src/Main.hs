module Main (
  main
) where


import qualified InfoVis.Parallel.Planes.Control as PP (main)


main :: IO ()
main = getContents >>= PP.main "Parallel Planes"

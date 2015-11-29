module Main (
  main
) where


import Control.Applicative ((<$>))
import Data.Relational (cast, fromString)
import Data.Relational.Value (asRealFloat)

import qualified InfoVis.Parallel.Planes.Control as PP (main)


main :: IO ()
main =
  do
    x <- cast asRealFloat . fromString <$> getContents
    PP.main "Parallel Planes" x

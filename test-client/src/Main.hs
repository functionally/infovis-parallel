module Main (
  main
) where


import Data.ByteString.Base64 (encode)
import Network.WebSockets
import System.Environment

import qualified Data.ByteString as BS
import qualified Data.Text as T


useBinary :: Bool
useBinary = True


main :: IO ()
main =
  runClient "127.0.0.1" 8080 "/Infovis"
    $ \connection ->
    do
      files <- getArgs
      sequence_
        [
          do
            bytes <- BS.readFile file
            if useBinary
              then sendBinaryData connection bytes
              else sendTextData connection $ encode bytes
        |
          file <- files
        ]
      x <- getContents
      print $ length x
      return ()

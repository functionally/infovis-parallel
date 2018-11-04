-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- Copyright   :  (c) 2018 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Sending buffers to Infovis-Parallel servers over WebSockets.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}


module InfoVis.Parallel.Sender (
  sendBuffers
) where


import Control.Exception (finally)
import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Log (Severity(..), logInfo)
import Data.ByteString.Base64 (encode)
import InfoVis (SeverityLog, withLogger)
import InfoVis.Parallel.ProtoBuf (Response)
import Network.WebSockets (receiveData, runClient, sendBinaryData, sendTextData, sendClose)

import qualified Data.ByteString as BS (readFile)
import qualified Data.Text as T (pack)


sendBuffers :: (MonadError String m, MonadIO m, SeverityLog m)
            => String
            -> Int
            -> String
            -> Bool
            -> [FilePath]
            -> m ()
sendBuffers host port path sendText buffers =
  do
    logInfo $ "Opening WebSocket on <ws://" ++ host ++ ":" ++ show port ++ path ++ "> . . ."
    withLogger $ \logger ->
      runClient host port path
        $ \connection ->
        do
          sequence_
            [
              do
                logger Debug $ "Reading " ++ show file ++ " . . ."           
                bytes <- BS.readFile file
                logger Informational $ "Sending " ++ show file ++ " . . ."           
                if sendText
                  then sendBinaryData connection          bytes
                  else sendTextData   connection $ encode bytes
            |
              file <- buffers
            ]
          let
            loop :: IO ()
            loop =
              do
                x <- receiveData connection
                logger Debug $ show (x :: Response)
                loop
          loop
            `finally` sendClose connection (T.pack "Infovis done.")
    logInfo $ "Closing WebSocket on <ws://" ++ host ++ ":" ++ show port ++ path ++ "> . . ."

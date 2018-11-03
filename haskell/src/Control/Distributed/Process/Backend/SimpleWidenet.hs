module Control.Distributed.Process.Backend.SimpleWidenet (
  L.Backend(..)
, initializeBackend
, L.startSlave
, L.terminateSlave
, L.findSlaves
, L.terminateAllSlaves
, L.startMaster
) where


import Control.Distributed.Process (NodeId(..), RemoteTable)
import Data.ByteString.Char8 (pack)
import Data.List (nub, sort)
import Network.Socket (HostName, ServiceName)
import Network.Transport (EndPointAddress(..))

import qualified Control.Distributed.Process.Backend.SimpleLocalnet as L


initializeBackend :: [(HostName, ServiceName)] -> HostName -> ServiceName -> RemoteTable -> IO L.Backend
initializeBackend peers host port rtable =
  do
    backend <- L.initializeBackend host port rtable
    return backend
      {
        L.findPeers = \timeout ->
          do
            let
              explicitPeers = map makeNodeId peers
            discoveredPeers <- L.findPeers backend timeout
            return $ nub $ sort $ explicitPeers ++ discoveredPeers
      }


makeNodeId :: (HostName, ServiceName) -> NodeId
makeNodeId (host, port) =
  NodeId
    . EndPointAddress
    . pack
    $ host ++ ":" ++ port ++ ":0"

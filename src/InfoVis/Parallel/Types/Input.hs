{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}


module InfoVis.Parallel.Types.Input (
  Input(..)
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

#ifdef INFOVIS_KAFKA
import InfoVis.Parallel.Types.Input.Kafka (InputKafka)
#endif

#ifdef INFOVIS_VRPN
import InfoVis.Parallel.Types.Input.VRPN (InputVRPN)
#endif


data Input =
    NoInput
#ifdef INFOVIS_KAFKA
  | InputKafka InputKafka
#endif
#ifdef INFOVIS_VRPN
  | InputVRPN InputVRPN
#endif
  deriving (Binary, Eq, FromJSON, Generic, Hashable, Ord, Read, Show, ToJSON)

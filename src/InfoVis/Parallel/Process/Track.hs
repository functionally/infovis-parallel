{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.Track (
  trackPov
, trackRelocation
, trackSelection
) where


import Control.Distributed.Process (Process, SendPort)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Input (Input(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage, SelecterMessage)

#ifdef INFOVIS_KAFKA
import qualified InfoVis.Parallel.Process.Track.Kafka as Kafka (trackPov, trackRelocation, trackSelection)
#endif

#ifdef INFOVIS_VRPN
import qualified InfoVis.Parallel.Process.Track.VRPN as VRPN (trackPov, trackRelocation, trackSelection)
#endif


trackPov :: Configuration ->  SendPort DisplayerMessage -> Process ()
trackPov configuration@Configuration{..} = 
  case input of
    NoInput -> error "trackPov: No input specified."
#ifdef INFOVIS_KAFKA
    InputKafka{} -> Kafka.trackPov configuration
#endif
#ifdef INFOVIS_VRPN
    InputVRPN{} -> VRPN.trackPov configuration
#endif


trackRelocation :: Configuration ->  SendPort SelecterMessage -> Process ()
trackRelocation configuration@Configuration{..} =
  case input of
    NoInput -> error "trackRelocation: No input specified."
#ifdef INFOVIS_KAFKA
    InputKafka{} -> Kafka.trackRelocation configuration
#endif
#ifdef INFOVIS_VRPN
    InputVRPN{} -> VRPN.trackRelocation configuration
#endif


trackSelection :: Configuration ->  SendPort SelecterMessage -> Process ()
trackSelection configuration@Configuration{..} =
  case input of
    NoInput -> error "trackSelection: No input specified."
#ifdef INFOVIS_KAFKA
    InputKafka{} -> Kafka.trackSelection configuration
#endif
#ifdef INFOVIS_VRPN
    InputVRPN{} -> VRPN.trackSelection configuration
#endif

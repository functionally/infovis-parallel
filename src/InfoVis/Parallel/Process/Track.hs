{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.Track (
  trackPov
, trackRelocation
, trackSelection
) where


import Control.Distributed.Process (Process, SendPort)
import InfoVis.Parallel.Process.Util (Debugger)
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Input (Input(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage, SelecterMessage)

#ifdef INFOVIS_KAFKA
import qualified InfoVis.Parallel.Process.Track.Kafka as Kafka (trackPov, trackRelocation, trackSelection)
#endif

#ifdef INFOVIS_VRPN
import qualified InfoVis.Parallel.Process.Track.VRPN as VRPN (trackPov, trackRelocation, trackSelection)
#endif


trackPov :: Debugger -> Configuration ->  SendPort DisplayerMessage -> Process ()
trackPov frameDebug configuration@Configuration{..} = 
  case input of
    NoInput -> error "trackPov: No input specified."
#ifdef INFOVIS_KAFKA
    InputKafka{} -> Kafka.trackPov frameDebug configuration
#endif
#ifdef INFOVIS_VRPN
    InputVRPN{} -> VRPN.trackPov frameDebug configuration
#endif


trackRelocation :: Debugger -> Configuration ->  SendPort SelecterMessage -> Process ()
trackRelocation frameDebug configuration@Configuration{..} =
  case input of
    NoInput -> error "trackRelocation: No input specified."
#ifdef INFOVIS_KAFKA
    InputKafka{} -> Kafka.trackRelocation frameDebug configuration
#endif
#ifdef INFOVIS_VRPN
    InputVRPN{} -> VRPN.trackRelocation frameDebug configuration
#endif


trackSelection :: Debugger -> Configuration ->  SendPort SelecterMessage -> Process ()
trackSelection frameDebug configuration@Configuration{..} =
  case input of
    NoInput -> error "trackSelection: No input specified."
#ifdef INFOVIS_KAFKA
    InputKafka{} -> Kafka.trackSelection frameDebug configuration
#endif
#ifdef INFOVIS_VRPN
    InputVRPN{} -> VRPN.trackSelection frameDebug configuration
#endif

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


trackPov :: SendPort DisplayerMessage -> Debugger -> Configuration -> Process ()
trackPov port frameDebug configuration@Configuration{..} = 
  case input of
    NoInput -> error "trackPov: No input specified."
#ifdef INFOVIS_KAFKA
    InputKafka{} -> Kafka.trackPov port frameDebug configuration
#endif
#ifdef INFOVIS_VRPN
    InputVRPN{} -> VRPN.trackPov port frameDebug configuration
#endif


trackRelocation :: SendPort SelecterMessage -> Debugger -> Configuration -> Process ()
trackRelocation port frameDebug configuration@Configuration{..} =
  case input of
    NoInput -> error "trackRelocation: No input specified."
#ifdef INFOVIS_KAFKA
    InputKafka{} -> Kafka.trackRelocation port frameDebug configuration
#endif
#ifdef INFOVIS_VRPN
    InputVRPN{} -> VRPN.trackRelocation port frameDebug configuration
#endif


trackSelection :: SendPort SelecterMessage -> Debugger -> Configuration -> Process ()
trackSelection port frameDebug configuration@Configuration{..} =
  case input of
    NoInput -> error "trackSelection: No input specified."
#ifdef INFOVIS_KAFKA
    InputKafka{} -> Kafka.trackSelection port frameDebug configuration
#endif
#ifdef INFOVIS_VRPN
    InputVRPN{} -> VRPN.trackSelection port frameDebug configuration
#endif

{-# LANGUAGE CPP             #-}


module InfoVis.Parallel.Process.Track (
  trackPov
, trackRelocation
, trackSelection
) where


import Control.Distributed.Process (Process, SendPort)
import InfoVis.Parallel.Types.Input (Input(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage, SelecterMessage)

#ifdef INFOVIS_KAFKA
import qualified InfoVis.Parallel.Process.Track.Kafka as Kafka (trackPov, trackRelocation, trackSelection)
#endif

#ifdef INFOVIS_VRPN
import qualified InfoVis.Parallel.Process.Track.VRPN as VRPN (trackPov, trackRelocation, trackSelection)
#endif


trackPov :: Input ->  SendPort DisplayerMessage -> Process ()
trackPov NoInput = error "trackPov: No input specified."
#ifdef INFOVIS_KAFKA
trackPov InputKafka{} = Kafka.trackPov
#endif
#ifdef INFOVIS_VRPN
trackPov InputVRPN{} = VRPN.trackPov
#endif


trackRelocation :: Input ->  SendPort SelecterMessage -> Process ()
trackRelocation NoInput = error "trackRelocation: No input specified."
#ifdef INFOVIS_KAFKA
trackRelocation InputKafka{} = Kafka.trackRelocation
#endif
#ifdef INFOVIS_VRPN
trackRelocation InputVRPN{} = VRPN.trackRelocation
#endif


trackSelection :: Input ->  SendPort SelecterMessage -> Process ()
trackSelection NoInput = error "trackSelection: No input specified."
#ifdef INFOVIS_KAFKA
trackSelection InputKafka{} = Kafka.trackSelection
#endif
#ifdef INFOVIS_VRPN
trackSelection InputVRPN{} = VRPN.trackSelection
#endif

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.Track (
  trackPov
, trackRelocation
, trackSelection
) where


import Control.Concurrent (threadDelay)
import Control.Distributed.Process (Process, SendPort, liftIO)
import InfoVis.Parallel.Process.Util (Debugger, runProcess, sendChan')
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Input (Input(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), SelectionAction(..))
import Linear.Affine (Point(..), (.+^))
import Linear.Metric (normalize)
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Linear.Vector (zero)
import System.Random (randomIO)


#ifdef INFOVIS_KAFKA
import qualified InfoVis.Parallel.Process.Track.Kafka as Kafka (trackPov, trackRelocation, trackSelection)
#endif

#ifdef INFOVIS_VRPN
import qualified InfoVis.Parallel.Process.Track.VRPN as VRPN (trackPov, trackRelocation, trackSelection)
#endif


trackPov :: SendPort DisplayerMessage -> Debugger -> Configuration -> Process ()
trackPov port frameDebug configuration@Configuration{..} = 
  case input of
#ifdef INFOVIS_KAFKA
    InputKafka{} -> Kafka.trackPov port frameDebug configuration
#endif
#ifdef INFOVIS_VRPN
    InputVRPN{} -> VRPN.trackPov port frameDebug configuration
#endif
    NoInput     -> runProcess "point-of-view tracker" 5 frameDebug $ \nextMessageIdentifier ->
                     do
                       let
                         sendListener = sendChan' frameDebug nextMessageIdentifier port
                         loop p o =
                           do
                             dt <- liftIO randomIO :: Process Double
                             vx <- (+ (-0.5)) <$> liftIO randomIO
                             vy <- (+ (-0.5)) <$> liftIO randomIO
                             vz <- (+ (-0.5)) <$> liftIO randomIO
                             ow <- (+ (-0.5)) <$> liftIO randomIO
                             ox <- (+ (-0.5)) <$> liftIO randomIO
                             oy <- (+ (-0.5)) <$> liftIO randomIO
                             oz <- (+ (-0.5)) <$> liftIO randomIO
                             let
                               p' = p .+^ 0.2 * V3 vx vy vz
                               o' = normalize $ o .+^ 0.01 * Quaternion ow (V3 ox oy oz)
                             sendListener "TP SC 1" $ Track p' o'
                             liftIO . threadDelay . floor $ 100000 * dt
                             loop p' o'
                       loop (P $ V3 0 150 500) (Quaternion 1 zero)


trackRelocation :: SendPort SelecterMessage -> Debugger -> Configuration -> Process ()
trackRelocation port frameDebug configuration@Configuration{..} =
  case input of
#ifdef INFOVIS_KAFKA
    InputKafka{} -> Kafka.trackRelocation port frameDebug configuration
#endif
#ifdef INFOVIS_VRPN
    InputVRPN{} -> VRPN.trackRelocation port frameDebug configuration
#endif
    NoInput     -> runProcess "relocation tracker" 5 frameDebug $ \nextMessageIdentifier ->
                     do
                       let
                         sendListener = sendChan' frameDebug nextMessageIdentifier port
                         loop = --p o =
                           do
                             dt <- liftIO randomIO :: Process Double
--                           vx <- (+ (-0.5)) <$> liftIO randomIO
--                           vy <- (+ (-0.5)) <$> liftIO randomIO
--                           vz <- (+ (-0.5)) <$> liftIO randomIO
--                           ow <- (+ (-0.5)) <$> liftIO randomIO
--                           ox <- (+ (-0.5)) <$> liftIO randomIO
--                           oy <- (+ (-0.5)) <$> liftIO randomIO
--                           oz <- (+ (-0.5)) <$> liftIO randomIO
--                           let
--                             p' = p .+^ 0.2 * V3 vx vy vz
--                             o' = normalize $ o .+^ 0.01 * Quaternion ow (V3 ox oy oz)
--                           sendListener "TR SC 2" $ RelocateSelection p' o'
                             liftIO . threadDelay . floor $ 100000 * dt
                             loop -- p' o'
                       loop -- zero (Quaternion 1 zero)


trackSelection :: SendPort SelecterMessage -> Debugger -> Configuration -> Process ()
trackSelection port frameDebug configuration@Configuration{..} =
  case input of
#ifdef INFOVIS_KAFKA
    InputKafka{} -> Kafka.trackSelection port frameDebug configuration
#endif
#ifdef INFOVIS_VRPN
    InputVRPN{} -> VRPN.trackSelection port frameDebug configuration
#endif
    NoInput     -> runProcess "selection tracker" 5 frameDebug $ \nextMessageIdentifier ->
                     do
                       let
                         sendListener = sendChan' frameDebug nextMessageIdentifier port
                         loop = -- p =
                           do
                             dt <- liftIO randomIO :: Process Double
--                           vx <- (+ (-0.5)) <$> liftIO randomIO
--                           vy <- (+ (-0.5)) <$> liftIO randomIO
--                           vz <- (+ (-0.5)) <$> liftIO randomIO
--                           let
--                             p' = p .+^ 0.2 * V3 vx vy vz
--                           sendListener "TS SC 3" $ UpdateSelection p' Highlight
--                           liftIO . threadDelay . floor $ 100000 * dt
                             loop -- p'
                       loop -- . P $ V3 (-35) 75 0

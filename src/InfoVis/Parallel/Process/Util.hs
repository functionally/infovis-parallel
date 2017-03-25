{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.Util (
  waitForTermination
, traceEnabled
, runProcess
, sendChan'
, receiveChan'
, receiveChanTimeout'
, matchChan'
, debugTime
, debugTimeIO
, asHalfFrames
, makeTimer
, Debug(..)
, Debugger
, DebuggerIO
, makeDebugger
, makeDebuggerIO
) where


import Control.DeepSeq (NFData, ($!!))
import Control.Distributed.Process (Match, Process, ReceivePort, SendPort, getSelfPid, match, matchAny, matchChan, receiveChan, receiveChanTimeout, receiveWait, register, sendChan)
import Control.Distributed.Process.Debug (TraceArg(..), TraceFlags(..), setTraceFlags, traceLogFmt, traceOff, traceOn)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.List (intercalate)
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..))
import InfoVis.Parallel.Types.Message (CommonMessage(Terminate), MessageTag(..), makeNextMessageIdentifier)
import System.Clock (Clock(Monotonic), TimeSpec(..), getTime, toNanoSecs)
import Text.Printf (printf)


waitForTermination :: Debugger -> Process ()
waitForTermination frameDebug =
  receiveWait
    [
      match $ \message -> case message of
                            Terminate{} -> frameDebug DebugInfo ["Received termination."]
                            _           -> frameDebug DebugInfo ["Received unexpected message.", show message]
    , matchAny $ \message -> frameDebug DebugInfo ["received unexpected message.", show message]
    ]


sendChan' :: (MessageTag a, NFData a, Serializable a) => Debugger -> Process Int -> SendPort a -> String -> (Int -> a) -> Process ()
sendChan' frameDebug nextMessageIdentifier chan tag message =
  do
    message' <- message <$> nextMessageIdentifier
    frameDebug DebugMessage [tag, messageTag message']
    sendChan chan $!! message'


receiveChan' :: (MessageTag a, Serializable a) => Debugger -> ReceivePort a -> String -> Process a
receiveChan' frameDebug chan tag =
  do
    message <- receiveChan chan
    frameDebug DebugMessage [tag, messageTag message]
    return message


receiveChanTimeout' :: (MessageTag a, Serializable a) => Debugger -> ReceivePort a -> String -> Int -> Process (Maybe a)
receiveChanTimeout' frameDebug chan tag timeout =
  do
    maybeMessage <- receiveChanTimeout timeout chan
    maybe (return ()) (frameDebug DebugMessage . (tag :) . return . messageTag) maybeMessage
    return maybeMessage

matchChan' :: (MessageTag a, Serializable a) => Debugger -> ReceivePort a -> String -> (a -> Process b) -> Match b
matchChan' frameDebug chan tag action =
  matchChan chan
    $ \message ->
    do
     frameDebug DebugMessage [tag, messageTag message]
     action message


debugTime :: Debugger -> Process Double -> [String] -> Double -> Process Double
debugTime frameDebug currentHalfFrame tags f0 =
  do
    f1 <- currentHalfFrame
    frameDebug DebugTiming $ tags ++ [printf "%.3f" $ f1 - f0]
    return f1


debugTimeIO :: DebuggerIO -> IO Double -> [String] -> Double -> IO Double
debugTimeIO frameDebugIO currentHalfFrameIO tags f0 =
  do
    f1 <- currentHalfFrameIO
    frameDebugIO DebugDisplay $ tags ++ [printf "%.3f" $ f1 - f0]
    return f1


runProcess :: String -> Int -> Debugger -> (Process Int -> Process ()) -> Process ()
runProcess name offset frameDebug action =
  do
    register name =<< getSelfPid
    frameDebug DebugInfo ["Starting " ++ name ++ "."]
    nextMessageIdentifier <- makeNextMessageIdentifier 10 offset
    action nextMessageIdentifier


makeTimer :: MonadIO m => m (m Double)
makeTimer =
  liftIO
    $ do
      f0 <- getTime Monotonic
      return
        $ liftIO
        $ do
          f1 <- getTime Monotonic
          return . asHalfFrames $ f1 - f0


asHalfFrames :: TimeSpec -> Double
asHalfFrames = (* 120) . (/ 1e9) . fromIntegral . toNanoSecs


data Debug =
    DebugInfo
  | DebugTiming
  | DebugMessage
  | DebugDisplay
    deriving (Bounded, Enum, Eq, Ord)

instance Show Debug where
  show DebugInfo    = "Info"
  show DebugTiming  = "Time"
  show DebugMessage = "Mess"
  show DebugDisplay = "Disp"


type Debugger = Debug -> [String] -> Process ()

type DebuggerIO = Debug -> [String] -> IO ()


traceEnabled :: Maybe AdvancedSettings -> Bool
traceEnabled _ = True -- FIXME: Add quiet mode.


makeDebugger :: Maybe AdvancedSettings -> Process Debugger
makeDebugger advancedSettings =
  do
    let
      asTrace True  = traceOn
      asTrace False = traceOff
      trace' = maybe traceOff (asTrace . trace) advancedSettings
      frameDebug :: [String] -> Process ()
      frameDebug tags =
        do
          pid <- getSelfPid
          traceLogFmt "\t" $ TraceStr <$> show pid : tags
    when (traceEnabled advancedSettings)
      . setTraceFlags
      $ TraceFlags trace' trace' trace' trace' traceOff traceOff True True
    makeDebuggerImpl frameDebug advancedSettings


makeDebuggerIO :: Maybe AdvancedSettings -> IO DebuggerIO
makeDebuggerIO = makeDebuggerImpl  $ putStrLn . intercalate "\t"


makeDebuggerImpl :: MonadIO m => ([String] -> m ()) -> Maybe AdvancedSettings -> m (Debug -> [String] -> m ())
makeDebuggerImpl output (Just AdvancedSettings{..}) =
  do
    f0 <- liftIO $ getTime Monotonic
    output $ ["0.000", "Info", "Initial time offset.", printf "%.3f" (asHalfFrames f0)]
    let
      debuggings =
        fmap snd
        . filter fst
        $ [
            (True         , DebugInfo   )
          , (debugTiming  , DebugTiming )
          , (debugMessages, DebugMessage)
          , (debugDisplay , DebugDisplay)
          ]
    return
      $ \debug message ->
          when (debug `elem` debuggings)
          $ do
            f1 <- liftIO $ getTime Monotonic
            output $ printf "%.3f" (asHalfFrames $ f1 - f0) : show debug : message
makeDebuggerImpl _ Nothing = error "Advanced settings required for debugging."

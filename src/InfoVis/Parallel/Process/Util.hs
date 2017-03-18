{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.Util (
  asHalfFrames
, initialTime
, currentHalfFrame
, currentHalfFrameIO
, Debug(..)
, initializeDebug
, frameDebug
, frameDebugIO
, collectChanMessages
) where


import Control.Distributed.Process (Process, ReceivePort, liftIO, receiveChan, receiveChanTimeout, say)
import Control.Distributed.Process.Serializable (Serializable)
import Data.Function (on)
import Data.Function.MapReduce (groupReduceFlatten)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (groupBy, sortBy)
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..))
import InfoVis.Parallel.Types.Message (MessageTag(..), SumTag(..))
import System.Clock (Clock(Monotonic), TimeSpec(..), getTime, toNanoSecs)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)


asHalfFrames :: TimeSpec -> Double
asHalfFrames = (* 120) . (/ 1e9) . fromIntegral . toNanoSecs


initialTimeVar :: IORef TimeSpec
{-# NOINLINE initialTimeVar #-}
initialTimeVar = unsafePerformIO $ newIORef =<< getTime Monotonic


initialTime :: IO TimeSpec
initialTime = readIORef initialTimeVar


currentHalfFrame :: Process Double
{-# INLINE currentHalfFrame #-}
currentHalfFrame = liftIO currentHalfFrameIO


currentHalfFrameIO :: IO Double
{-# INLINE currentHalfFrameIO #-}
currentHalfFrameIO =
  do
    f0 <- initialTime
    f1 <- getTime Monotonic
    return . asHalfFrames $ f1 - f0


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


debuggingVar  :: IORef [Debug]
{-# NOINLINE debuggingVar #-}
debuggingVar  = unsafePerformIO $ newIORef [DebugInfo]


initializeDebug :: AdvancedSettings -> Process ()
initializeDebug AdvancedSettings{..} =
  do
    t0 <- liftIO initialTime
    say $ "0.000\tInfo\tInitial time offset = " ++ printf "%.3f" (asHalfFrames $ t0 - TimeSpec 5200000 0)
    liftIO
      . writeIORef debuggingVar
      . fmap snd
      . filter fst
      $ [
          (True         , DebugInfo   )
        , (debugTiming  , DebugTiming )
        , (debugMessages, DebugMessage)
        , (debugDisplay , DebugDisplay)
        ]


frameDebug :: Debug -> String -> Process ()
{-# INLINE frameDebug #-}
frameDebug debug message =
  do
    s <- liftIO $ frameDebugString debug message
    maybe (return ()) say s


frameDebugIO :: Debug -> String -> IO ()
{-# INLINE frameDebugIO #-}
frameDebugIO debug message =
  do
    s <- frameDebugString debug message
    maybe (return ()) putStrLn s


frameDebugString :: Debug -> String -> IO (Maybe String)
frameDebugString debug message =
  do
    debuggings <- readIORef debuggingVar
    if debug `elem` debuggings
      then do
           df <- currentHalfFrameIO
           return . Just $ printf "%.3f" df ++ "\t" ++ show debug ++ "\t" ++ message
      else return Nothing


collectChanMessages :: (MessageTag a, Serializable a, SumTag a) => Int -> ReceivePort a -> (a -> a -> Bool) -> Process [a]
collectChanMessages count chan grouper =
  do
    let
      collectMessages' 0 = return []
      collectMessages' count' =
        do
          maybeMessage <- receiveChanTimeout 1 chan
          case maybeMessage of
            Nothing      -> return []
            Just message -> do
                              frameDebug DebugMessage $ "CM RC 1\t" ++ messageTag message
                              (message :) <$> collectMessages' (count' - 1)
      collapser = fmap head . groupBy (grouper `on` snd)
    message <- receiveChan chan
    frameDebug DebugMessage $ "CM RC 2\t" ++ messageTag message
    messages <- collectMessages' count
    return
      . map snd
      . sortBy (compare `on` fst)
      . groupReduceFlatten (sumTag . snd) collapser
      . zip [(0::Int),-1..]
      $ message : messages

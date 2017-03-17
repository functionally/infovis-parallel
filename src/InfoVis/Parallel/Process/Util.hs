{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.Util (
  initialHalfFrame
, currentHalfFrame
, Debug(..)
, initializeDebug
, frameDebug
, frameDebugIO
, collectChanMessages
) where


import Control.Distributed.Process (Process, ReceivePort, liftIO, receiveChan, receiveChanTimeout)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad ( when)
import Data.Function (on)
import Data.Function.MapReduce (groupReduceFlatten)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (groupBy, sortBy)
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..))
import InfoVis.Parallel.Types.Message (MessageTag(..), SumTag(..))
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)


timeAsHalfFrame :: IO Double
timeAsHalfFrame = (* 120) . (/ 1e9) . fromIntegral . toNanoSecs <$> getTime Monotonic


initialHalfFrameVar :: IORef Double
{-# NOINLINE initialHalfFrameVar #-}
initialHalfFrameVar = unsafePerformIO $ newIORef =<< timeAsHalfFrame


initialHalfFrame :: IO Double
initialHalfFrame = readIORef initialHalfFrameVar


currentHalfFrame :: IO Double
currentHalfFrame =
  do
    f0 <- initialHalfFrame
    f1 <- timeAsHalfFrame
    return $ f1 - f0


data Debug =
    Debug
  | DebugTiming
  | DebugMessage
  | DebugDisplay
    deriving (Bounded, Enum, Eq, Ord, Read, Show)


debuggingVar  :: IORef [Debug]
{-# NOINLINE debuggingVar #-}
debuggingVar  = unsafePerformIO $ newIORef [Debug]


initializeDebug :: AdvancedSettings -> Process ()
initializeDebug AdvancedSettings{..} =
  liftIO
    . writeIORef debuggingVar
    . fmap snd
    . filter fst
    $ [
        (True          , Debug       )
      , (debugTiming   , DebugTiming )
      , (debugMessages , DebugMessage)
      , (debugDisplayer, DebugDisplay)
      ]


frameDebug :: Debug -> String -> Process ()
frameDebug = (liftIO .) . frameDebugIO


frameDebugIO :: Debug -> String -> IO ()
frameDebugIO debug message =
  do
    debuggings <- readIORef debuggingVar
    when (debug `elem` debuggings)
      $ do
        df <- currentHalfFrame
        putStrLn $ printf "%.2f" df ++ "\t" ++ show debug ++ "\t" ++ message


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

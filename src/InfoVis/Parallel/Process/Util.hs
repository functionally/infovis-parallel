module InfoVis.Parallel.Process.Util (
  timestamp
, collectChanMessages
) where


import Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar, putMVar, readMVar)
import Control.Distributed.Process (Process, ReceivePort, receiveChan, receiveChanTimeout)
import Control.Distributed.Process.Serializable (Serializable)
import Data.Function (on)
import Data.Function.MapReduce (groupReduceFlatten)
import Data.List (groupBy, sortBy)
import InfoVis.Parallel.Types.Message (SumTag(..))
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)
import System.IO.Unsafe (unsafePerformIO)



timestamp :: String -> IO Double
timestamp s =
  do
    t0' <- t0
    t <- toNanoSecs <$> getTime Monotonic
    putStrLn $ show (fromIntegral (t - t0') * 120 / 1e9 :: Double) ++ "\t" ++ s
    return $ fromIntegral t * 120 / 1e9


t0Var :: MVar Integer
{-# NOINLINE t0Var #-}
t0Var = unsafePerformIO newEmptyMVar


t0 :: IO Integer
t0 =
  do
    e <- isEmptyMVar t0Var
    if e
      then do
                 t0' <- toNanoSecs <$> getTime Monotonic
                 putMVar t0Var t0'
                 return t0'
      else readMVar t0Var


collectChanMessages :: (Serializable a, SumTag a) => Int -> ReceivePort a -> (a -> a -> Bool) -> Process [a]
collectChanMessages count chan grouper =
  do
    let
      collectMessages' 0 = return []
      collectMessages' count' =
        do
          maybeMessage <- receiveChanTimeout 1 chan
          case maybeMessage of
            Nothing      -> return []
            Just message -> (message :) <$> collectMessages' (count' - 1)
      collapser = fmap head . groupBy (grouper `on` snd)
    message <- receiveChan chan
    messages <- collectMessages' count
    return
      . map snd
      . sortBy (compare `on` fst)
      . groupReduceFlatten (sumTag . snd) collapser
      . zip [(0::Int),-1..]
      $ message : messages

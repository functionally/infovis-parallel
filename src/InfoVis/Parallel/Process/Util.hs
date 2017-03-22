{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Process.Util (
  asHalfFrames
, makeTimer
, Debug(..)
, Debugger
, makeDebugger
, makeDebuggerIO
) where


import Control.Distributed.Process (Process, say)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..))
import System.Clock (Clock(Monotonic), TimeSpec(..), getTime, toNanoSecs)
import Text.Printf (printf)


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


type Debugger = Debug -> String -> Process ()


makeDebugger :: Maybe AdvancedSettings -> Process Debugger
makeDebugger = makeDebuggerImpl say


makeDebuggerIO :: Maybe AdvancedSettings -> IO (Debug -> String -> IO ())
makeDebuggerIO = makeDebuggerImpl putStrLn


makeDebuggerImpl :: MonadIO m => (String -> m ()) -> Maybe AdvancedSettings -> m (Debug -> String -> m ())
makeDebuggerImpl output (Just AdvancedSettings{..}) =
  do
    f0 <- liftIO $ getTime Monotonic
    output $ "0.000\tInfo\tInitial time offset = " ++ printf "%.3f" (asHalfFrames f0)
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
            output $ printf "%.3f" (asHalfFrames $ f1 - f0) ++ "\t" ++ show debug ++ "\t" ++ message
makeDebuggerImpl _ Nothing = error "Advanced settings required for debugging."

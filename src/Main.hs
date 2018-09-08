{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Main (
  main
) where


import Control.Exception (Exception(displayException))
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError, MonadIO, liftIO, runExceptT, throwError)
import Control.Monad.Log (LoggingT, MonadLog, Severity(..), WithSeverity(..), logCritical, logDebug, logInfo, renderWithSeverity, runLoggingT)
import Data.String (IsString(..))
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import InfoVis.Parallel.Service (MonadService, guardBracket, guardIO, runServiceToIO, throwService)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPrint, stderr)
import System.IO.Error (tryIOError)

import qualified Graphics.UI.GLFW as GLFW


service :: MonadService m => m ()
service = do
  _ <- guardIO $ readFile "Hello"
  return ()


withVulkan :: MonadService m
           => String
           -> Int
           -> Int
           -> (VkInstance -> m a)
           -> m a
withVulkan title width height action =
  guardBracket
    (
      do
        initialized <- guardIO GLFW.init
        if initialized
          then logDebug "GLFW initialized."
          else throwService "GLFW initialization failed."
    )
    (
      const
        $ guardIO GLFW.terminate
        >> logDebug "GLFW terminated."
    )
    (const $ action undefined)


main :: IO ()
main =
  do
    result <- runServiceToIO Debug service
    case result of
      Left{}  -> exitFailure
      Right{} -> exitSuccess

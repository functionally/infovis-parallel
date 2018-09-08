{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module InfoVis.Parallel.Service (
  ServiceM
, MonadService
, runServiceToIO
, ServiceException(..)
, throwService
, throwVulkan
, guardIO
, guardBracket
) where


import Control.Exception (Exception(displayException))
import Control.Monad (void, when)
import Control.Monad.Except (ExceptT, MonadError, MonadIO, catchError, liftIO, runExceptT, throwError)
import Control.Monad.Log (LoggingT, MonadLog, Severity(..), WithSeverity(..), logCritical, renderWithSeverity, runLoggingT)
import Data.String (IsString(..))
import Graphics.Vulkan.Core_1_0 (VkResult)
import System.IO (hPrint, stderr)
import System.IO.Error (tryIOError)


data ServiceException =
    ServiceException
    {
      serviceMessage :: String
    }
  | VulkanException
    {
      vulkanCode    :: Maybe VkResult
    , vulkanMessage :: String
    }
  | SystemException
    {
      systemException :: String
    }
    deriving (Eq, Read, Show)


throwService :: MonadError ServiceException m => String -> m a
throwService = throwError . ServiceException


throwVulkan :: MonadError ServiceException m => Maybe VkResult -> String -> m a
throwVulkan = (throwError .) . VulkanException


newtype ServiceM a = ServiceM {runServiceM :: ExceptT ServiceException (LoggingT (WithSeverity String) IO) a}
  deriving (Applicative, Functor, Monad, MonadError ServiceException, MonadIO, MonadLog (WithSeverity String))


type MonadService m = (MonadError ServiceException m, MonadIO m, MonadLog (WithSeverity String) m)


runServiceToIO :: Severity -> ServiceM a -> IO (Either ServiceException a)
runServiceToIO threshold =
  let
    report x@(Left exception) = logCritical (show exception) >> return x
    report x                  = return x
    logHandler message =
      when (msgSeverity message <= threshold)
        $ liftIO . hPrint stderr $ renderWithSeverity fromString message
  in
    flip runLoggingT logHandler
      . (>>= report)
      . runExceptT
      . runServiceM


guardIO :: (MonadError ServiceException m, MonadIO m) => IO a -> m a
guardIO =
  (either (throwError . SystemException . displayException) return =<<)
    . liftIO
    . tryIOError


guardBracket :: (MonadError ServiceException m, MonadIO m)
             => m a
             -> (a -> m b)
             -> (a -> m c)
             -> m c
guardBracket before after action =
  do
    x <- before
    let
      finalize = void $ after x
    y <- action x `catchError` (\e -> finalize >> throwError e)
    finalize
    return y

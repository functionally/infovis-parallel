{-# LANGUAGE FlexibleContexts #-}


module Main (
  main
) where


import Control.Monad.Log (Severity(..))
import InfoVis.Parallel.Service (MonadService, runServiceToIO)
import InfoVis.Parallel.Service.Creation (withVulkan)
import System.Exit (exitFailure, exitSuccess)


service :: MonadService m => m ()
service =
  withVulkan "InfoVis Parallel" 800 600 $ \_window _vulkan -> return ()


main :: IO ()
main =
  do
    result <- runServiceToIO Debug service
    case result of
      Left{}  -> exitFailure
      Right{} -> exitSuccess

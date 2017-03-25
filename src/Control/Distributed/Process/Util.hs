{-# LANGUAGE TupleSections #-}


module Control.Distributed.Process.Util (
  spawnLocalMonitor
) where


import Control.Distributed.Process (MonitorRef, Process, ProcessId, monitor, spawnLocal)


spawnLocalMonitor :: Process () -> Process (ProcessId, MonitorRef)
spawnLocalMonitor action =
  do
    pid <- spawnLocal action
    (pid, ) <$> monitor pid

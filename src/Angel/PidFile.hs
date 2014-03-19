module Angel.PidFile ( startMaybeWithPidFile
                     , startWithPidFile
                     , clearPIDFile) where

import Control.Exception.Base (finally)

import Control.Monad (when)
import System.Process ( CreateProcess
                      , createProcess
                      , ProcessHandle )

-- Wish I didn't have to do this :(
import System.Process.Internals ( PHANDLE
                                , ProcessHandle__(OpenHandle, ClosedHandle)
                                , withProcessHandle
                                )
import System.Posix.Files ( removeLink
                          , fileExist)

startMaybeWithPidFile :: CreateProcess -> Maybe FilePath -> (ProcessHandle -> IO a) -> IO a
startMaybeWithPidFile procSpec (Just pidFile) = startWithPidFile procSpec pidFile
startMaybeWithPidFile procSpec Nothing        = withPHandle procSpec

startWithPidFile :: CreateProcess -> FilePath -> (ProcessHandle -> IO a) -> IO a
startWithPidFile procSpec pidFile action =
  withPHandle procSpec $ \pHandle -> do
    mPid               <-  getPID pHandle
    maybe (return ()) write mPid
    action pHandle `finally` clearPIDFile pidFile
  where write = writePID pidFile

withPHandle :: CreateProcess -> (ProcessHandle -> IO a) -> IO a
withPHandle procSpec action = do
  (_, _, _, pHandle) <- createProcess procSpec
  action pHandle

writePID :: FilePath -> PHANDLE -> IO ()
writePID pidFile = writeFile pidFile . show

clearPIDFile :: FilePath -> IO ()
clearPIDFile pidFile = do ex <- fileExist pidFile
                          when ex rm
  where rm = removeLink pidFile

getPID :: ProcessHandle -> IO (Maybe PHANDLE)
getPID pHandle = withProcessHandle pHandle getPID'
  where getPID' h @ (OpenHandle t) = return (Just t)
        getPID' h @ (ClosedHandle t) = return Nothing

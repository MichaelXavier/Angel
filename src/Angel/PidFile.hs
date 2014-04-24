module Angel.PidFile ( startMaybeWithPidFile
                     , startWithPidFile
                     , clearPIDFile) where

import Control.Exception.Base ( finally
                              , onException )

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

startMaybeWithPidFile :: CreateProcess
                      -> Maybe FilePath
                      -> (ProcessHandle -> IO a)
                      -> (ProcessHandle -> IO a)
                      -> IO a
startMaybeWithPidFile procSpec (Just pidFile) action onPidError = startWithPidFile procSpec pidFile action onPidError
startMaybeWithPidFile procSpec Nothing action _ = withPHandle procSpec action

startWithPidFile :: CreateProcess
                 -> FilePath
                 -> (ProcessHandle -> IO a)
                 -> (ProcessHandle -> IO a)
                 -> IO a
startWithPidFile procSpec pidFile action onPidError =
  withPHandle procSpec $ \pHandle -> do
    mPid               <-  getPID pHandle
    case mPid of
      Just pid -> write pid pHandle
      Nothing  -> proceed pHandle
  where
    write pid pHandle = do
      writePID pidFile pid `onException` onPidError pHandle -- re-raises
      proceed pHandle
    proceed pHandle = action pHandle `finally` clearPIDFile pidFile

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

module Main (main) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.Posix.Signals (installHandler, sigTERM, Handler(Catch))

main :: IO ()
main = do
  sig <- newEmptyMVar
  installHandler sigTERM (Catch $ putMVar sig ExitSuccess) Nothing
  exitWith =<< takeMVar sig

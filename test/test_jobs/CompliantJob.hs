module Main (main) where

import GHC.IO.Handle
import System.IO (stdout)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.Posix.Signals (installHandler, sigTERM, Handler(Catch))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  sig <- newEmptyMVar
  installHandler sigTERM (Catch $ print "term" >> putMVar sig ExitSuccess) Nothing
  exitWith =<< takeMVar sig

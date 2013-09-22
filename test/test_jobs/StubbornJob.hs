module Main (main) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import System.Posix.Signals (installHandler, sigTERM, Handler(Catch))

main :: IO ()
main = do
  sig <- newEmptyMVar
  installHandler sigTERM (Catch $ return ()) Nothing
  forkIO $ threadDelay maxBound >> putMVar sig ()
  () <- takeMVar sig
  return ()

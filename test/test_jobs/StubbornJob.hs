module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import System.Posix.Signals (installHandler, sigTERM, Handler(Catch))

main :: IO ()
main = do
  sig <- newEmptyMVar
  installHandler sigTERM (Catch $ threadDelay maxBound >> putMVar sig ()) Nothing
  () <- takeMVar sig
  return ()

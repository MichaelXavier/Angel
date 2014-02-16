module Main (main) where

import GHC.IO.Handle
import System.IO (stdout)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import System.Posix.Signals (installHandler, sigTERM, Handler(Catch))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Stubborn job started"
  sig <- newEmptyMVar
  installHandler sigTERM (Catch $ print "term, ignoring" >> return ()) Nothing
  forkIO $ threadDelay maxBound >> putMVar sig ()
  () <- takeMVar sig
  return ()

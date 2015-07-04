#!/usr/bin/env runhaskell
import Control.Monad
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Program.Run
import Distribution.Simple.UserHooks
import Distribution.PackageDescription
import Distribution.Verbosity
-- import System.Process


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    {
      preTest = myTestHook
    }


-------------------------------------------------------------------------------
myTestHook
    :: Args
    -> TestFlags
    -> IO HookedBuildInfo
myTestHook _ _ = do
    buildTestFixtures
    return emptyHookedBuildInfo


-------------------------------------------------------------------------------
buildTestFixtures :: IO ()
buildTestFixtures = do
  runScript "ghc" ["--make", "test/test_jobs/StubbornJob.hs", "-o", "test/test_jobs/StubbornJob"]
  runScript "ghc" ["--make", "test/test_jobs/CompliantJob.hs", "-o", "test/test_jobs/CompliantJob"]


-------------------------------------------------------------------------------
runScript :: String -> [String] -> IO ()
runScript cmd args = do
  putStrLn $ unwords [cmd, unwords args]
  runProgramInvocation normal $ simpleProgramInvocation cmd args

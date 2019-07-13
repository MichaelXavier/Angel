module Main (main) where

-------------------------------------------------------------------------------
import qualified Angel.ConfigSpec
import qualified Angel.JobSpec
import qualified Angel.LogSpec
import qualified Angel.PidFileSpec
import qualified Angel.UtilSpec
-------------------------------------------------------------------------------
import SpecHelper
-------------------------------------------------------------------------------
import Control.Exception (throwIO)
import System.Exit (ExitCode (..))
import System.Process (rawSystem)


buildTestFixtures :: IO ()
buildTestFixtures = do
  run "ghc" ["--make", "test/test_jobs/StubbornJob.hs", "-o", "test/test_jobs/StubbornJob"]
  run "ghc" ["--make", "test/test_jobs/CompliantJob.hs", "-o", "test/test_jobs/CompliantJob"]
  where
    run a b = do
      e <- rawSystem a b
      case e of
        ExitSuccess ->
          return ()
        ExitFailure _ ->
          throwIO e

main :: IO ()
main = do
  buildTestFixtures
  defaultMain $ testGroup "angel" [
      Angel.ConfigSpec.spec
    , Angel.JobSpec.spec
    , Angel.LogSpec.spec
    , Angel.PidFileSpec.spec
    , Angel.UtilSpec.spec
    ]

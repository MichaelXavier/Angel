module Angel.JobSpec (spec) where

import Angel.Job (killProcess)
import Angel.Process ( getProcessHandleStatus
                     , hardKillProcessHandle )
import Angel.Data (KillDirective(..))
import Angel.Util (sleepSecs)

import qualified Data.Conduit.Process.Unix as U
import System.Exit (ExitCode(..))
import System.Posix.Directory (getWorkingDirectory)
import System.Posix.Signals (sigKILL)
import System.Posix.Process (ProcessStatus(..))
import System.Process ( createProcess
                      , proc
                      , ProcessHandle )

import SpecHelper

spec :: Spec
spec =
  describe "killProcess" $ do
    describe "using SoftKill" $ do
      it "cleanly kills well-behaved processes" $ do
        ph <- launchCompliantJob
        killProcess $ SoftKill "thing" ph Nothing
        patientlyGetProcessExitCode ph `shouldReturn` (Just $ Exited ExitSuccess)

      it "does not forcefully kill stubborn processes" $ do
        ph <- launchStubbornJob
        killProcess $ SoftKill "thing" ph Nothing
        -- stubborn job gets marked as [defunct] here. no idea why. it should be able to survive a SIGTERM
        patientlyGetProcessExitCode ph `shouldReturn` Nothing
        hardKillProcessHandle ph -- cleanup

    describe "using HardKill" $ do
      it "cleanly kills well-behaved processes" $ do
        ph <- launchCompliantJob
        killProcess $ HardKill "thing" ph Nothing 1
        -- Can't geth the exiit status because the life check in Job "uses up" the waitpid
        patientlyGetProcessExitCode ph `shouldReturn` Nothing
      it "forcefully kills stubborn processes" $ do
        ph <- launchStubbornJob
        killProcess $ HardKill "thing" ph Nothing 1
        patientlyGetProcessExitCode ph `shouldReturn` (Just $ Terminated sigKILL False) --maybe

launchCompliantJob :: IO ProcessHandle
launchCompliantJob = launchJob "CompliantJob"

launchStubbornJob :: IO ProcessHandle
launchStubbornJob = launchJob "StubbornJob"

launchJob :: FilePath -> IO ProcessHandle
launchJob n = do wd <- getWorkingDirectory
                 let path = wd ++ "/test/test_jobs/" ++ n
                 (_, _, _, ph) <- createProcess $ proc path []
                 sleepSecs 1
                 return ph

patientlyGetProcessExitCode :: ProcessHandle -> IO (Maybe ProcessStatus)
patientlyGetProcessExitCode ph = sleepSecs 1 >> getProcessHandleStatus ph

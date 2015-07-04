{-# LANGUAGE CPP #-}
module Angel.JobSpec (spec) where

import Angel.Job (killProcess)
import Angel.Process ( getProcessHandleStatus
                     , hardKillProcessHandle )
import Angel.Data hiding (Spec, spec)
import Angel.Util (sleepSecs)

import Control.Monad.IO.Class
import System.Exit (ExitCode(..))
import System.Posix.Directory (getWorkingDirectory)
import System.Posix.Signals (sigKILL)
import System.Posix.Process (ProcessStatus(..))
import System.Process ( createProcess
                      , proc
                      , ProcessHandle )

import SpecHelper

spec :: TestTree
spec = testGroup "Angel.Jon"
  [
    testGroup "killProcess"
    [
      testGroup "using SoftKill"
      [
        testCase "cleanly kills well-behaved processes" $ runAngelM dummyOptions $ do
          ph <- liftIO launchCompliantJob
          killProcess $ SoftKill "thing" ph Nothing
          liftIO $
            patientlyGetProcessExitCode ph `shouldReturn` (Just $ Exited ExitSuccess)

        testCase "does not forcefully kill stubborn processes" $ runAngelM dummyOptions $ do
          ph <- liftIO launchStubbornJob
          killProcess $ SoftKill "thing" ph Nothing
          -- stubborn job gets marked as [defunct] here. no idea why. it should be able to survive a SIGTERM
          liftIO $ do
            patientlyGetProcessExitCode ph `shouldReturn` Nothing
            hardKillProcessHandle ph -- cleanup
      ],
      testGroup "using HardKill"
      [
        testCase "cleanly kills well-behaved processes" $ runAngelM dummyOptions $ do
          ph <- liftIO launchCompliantJob
          killProcess $ HardKill "thing" ph Nothing 1
          -- Can't geth the exiit status because the life check in Job "uses up" the waitpid
          liftIO $
            patientlyGetProcessExitCode ph `shouldReturn` Nothing
      , testCase "forcefully kills stubborn processes" $ runAngelM dummyOptions $ do
          ph <- liftIO launchStubbornJob
          killProcess $ HardKill "thing" ph Nothing 1
          liftIO $
#if MIN_VERSION_unix(2,7,0)
            patientlyGetProcessExitCode ph `shouldReturn` (Just $ Terminated sigKILL False)
#else
            patientlyGetProcessExitCode ph `shouldReturn` (Just $ Terminated sigKILL)
#endif
      ],
      testGroup "with a logger"
      [
        testCase "cleanly kills well-behaved loggers" $ runAngelM dummyOptions $ do
          ph <- liftIO launchCompliantJob
          lph <- liftIO launchCompliantJob
          killProcess $ SoftKill "thing" ph (Just lph)
          liftIO $
            patientlyGetProcessExitCode lph `shouldReturn` (Just $ Exited ExitSuccess)
      ]
    ]
  ]


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

dummyOptions :: Options
dummyOptions = Options {
      configFile = ""
    , verbosity  = V0
    }

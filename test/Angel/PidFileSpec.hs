module Angel.PidFileSpec (spec) where

import Angel.PidFile

import Data.Char (isNumber)
import System.Process (proc)
import System.Posix.Files (fileExist)

import Test.Hspec
import qualified Test.Hspec as H (Spec)

spec :: H.Spec
spec = do
  describe "startWithPidFile" $ do
    it "creates the pidfile and cleans up" $ do
      startWithPidFile procSpec fileName $ \pHandle -> do
        fileShouldExist fileName
        pid <- readFile fileName
        null pid `shouldBe` False
        all isNumber pid `shouldBe` True
      fileShouldNotExist fileName
  where fileName = "temp.pid"
        procSpec = proc "pwd" []
        fileShouldExist name    = fileExist fileName `shouldReturn` True
        fileShouldNotExist name = fileExist fileName `shouldReturn` False

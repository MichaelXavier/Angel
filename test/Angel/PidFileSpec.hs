{-# LANGUAGE ScopedTypeVariables #-}
module Angel.PidFileSpec (spec) where

import Angel.PidFile

import Control.Exception.Base ( try
                              , SomeException )
import Control.Monad (void)
import Data.Char (isNumber)
import Data.IORef ( newIORef
                  , readIORef
                  , writeIORef )
import System.Process (proc)
import System.Posix.Files (fileExist)

import SpecHelper

spec :: Spec
spec = do
  describe "startWithPidFile" $ do
    it "creates the pidfile and cleans up" $ do
      startWithPidFile procSpec fileName jogOn $ \pHandle -> do
        fileShouldExist fileName
        pid <- readFile fileName
        null pid `shouldBe` False
        all isNumber pid `shouldBe` True
      fileShouldNotExist fileName
    it "calls the error callback when pidfile can't be created and re-raises" $ do
      called <- newIORef False
      let onPidError = const $ writeIORef called True
      (res :: Either SomeException ()) <- try $ startWithPidFile procSpec badPidFile onPidError jogOn
      readIORef called `shouldReturn` True
      isLeft res `shouldBe` True
  where
    fileName = "temp.pid"
    badPidFile = "/bogus/path/to/pidfile"
    procSpec = proc "pwd" []
    fileShouldExist name    = fileExist fileName `shouldReturn` True
    fileShouldNotExist name = fileExist fileName `shouldReturn` False
    jogOn = const $ return ()
    isLeft (Left _) = True
    isLeft _        = False

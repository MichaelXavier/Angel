{-# LANGUAGE OverloadedStrings #-}
module Angel.ConfigSpec (spec) where

import Angel.Data hiding (spec)
import Angel.Config

import Control.Exception.Base
import Data.Configurator.Types (Value(..))
import qualified Data.HashMap.Lazy as HM

import Test.Hspec
import qualified Test.Hspec as H (Spec)

spec :: H.Spec
spec = do
  describe "modifyProg" $ do
    it "modifies exec" $
      modifyProg prog "exec" (String "foo") `shouldBe`
      prog { exec = Just "foo"}

    it "errors for non-string execs" $
      evaluate (modifyProg prog "exec" (Bool True)) `shouldThrow`
      anyErrorCall

    it "modifies delay for positive numbers" $
      modifyProg prog "delay" (Number 1) `shouldBe`
      prog { delay = Just 1}
    it "modifies delay for 0" $
      modifyProg prog "delay" (Number 0) `shouldBe`
      prog { delay = Just 0}
    it "errors on negative delays" $
      evaluate (modifyProg prog "delay" (Number (-1))) `shouldThrow`
      anyErrorCall

    it "modifies stdout" $
      modifyProg prog "stdout" (String "foo") `shouldBe`
      prog { stdout = Just "foo"}
    it "errors for non-string stdout" $
      evaluate (modifyProg prog "stdout" (Bool True)) `shouldThrow`
      anyErrorCall

    it "modifies stderr" $
      modifyProg prog "stderr" (String "foo") `shouldBe`
      prog { stderr = Just "foo"}
    it "errors for non-string stderr" $
      evaluate (modifyProg prog "stderr" (Bool True)) `shouldThrow`
      anyErrorCall

    it "modifies directory" $
      modifyProg prog "directory" (String "foo") `shouldBe`
      prog { workingDir = Just "foo"}
    it "errors for non-string directory" $
      evaluate (modifyProg prog "directory" (Bool True)) `shouldThrow`
      anyErrorCall

    it "modifies pidfile" $
      modifyProg prog "pidfile" (String "foo.pid") `shouldBe`
      prog { pidFile = Just "foo.pid"}
    it "errors for non-string path" $
      evaluate (modifyProg prog "pidfile" (Bool True)) `shouldThrow`
      anyErrorCall

    it "appends env to the empty list" $
      modifyProg prog "env.foo" (String "bar") `shouldBe`
      prog { env = [("foo", "bar")]}
    it "errors for non-string value" $
      evaluate (modifyProg prog "env.foo" (Bool True)) `shouldThrow`
      anyErrorCall
    it "prepends env to an existing list" $
      modifyProg prog { env = [("previous", "value")]} "env.foo" (String "bar") `shouldBe`
      prog { env = [("foo", "bar"), ("previous", "value")]}

    it "does nothing for all other cases" $
      modifyProg prog "bogus" (String "foo") `shouldBe`
      prog

  describe "expandByCount" $ do
    it "doesn't affect empty hashes" $
      expandByCount HM.empty `shouldBe`
        HM.empty
    it "doesn't affect hashes without counts" $
      expandByCount (HM.fromList [baseProgPair]) `shouldBe`
        HM.fromList [baseProgPair]
    it "errors on mistyped count field" $
      evaluate (expandByCount (HM.fromList [baseProgPair
                                           , ("prog.count", String "wat")])) `shouldThrow`
        anyErrorCall
    it "errors on negative count field" $
      evaluate (expandByCount (HM.fromList [ baseProgPair
                                           , ("prog.count", Number (-1))])) `shouldThrow`
        anyErrorCall
    it "generates no configs with a count of 0" $
      expandByCount (HM.fromList [ baseProgPair
                                 , ("prog.count", Number 0)]) `shouldBe`
        HM.empty
    it "expands with a count of 1" $
      expandByCount (HM.fromList [baseProgPair, ("prog.count", Number 1)]) `shouldBe`
        HM.fromList [ ("prog-1.exec", String "foo")
                    , ("prog-1.env.ANGEL_PROCESS_NUMBER", String "1")]
    it "expands with a count of > 1" $
      expandByCount (HM.fromList [baseProgPair, ("prog.count", Number 2)]) `shouldBe`
        HM.fromList [ ("prog-1.exec", String "foo")
                    , ("prog-1.env.ANGEL_PROCESS_NUMBER", String "1")
                    , ("prog-2.exec", String "foo")
                    , ("prog-2.env.ANGEL_PROCESS_NUMBER", String "2")]
    it "preserves explicit env variables" $
      expandByCount (HM.fromList [baseProgPair, ("prog.env.FOO", String "bar")]) `shouldBe`
        HM.fromList [ ("prog.exec",    String "foo")
                    , ("prog.env.FOO", String "bar")]
    it "expands pidfiles with a count of 1" $
      expandByCount (HM.fromList [ baseProgPair
                                 , ("prog.count", Number 1)
                                 , ("prog.pidfile", String "foo.pid")]) `shouldBe`
        HM.fromList [ ("prog-1.exec", String "foo")
                    , ("prog-1.env.ANGEL_PROCESS_NUMBER", String "1")
                    , ("prog-1.pidfile", String "foo-1.pid")] --TODO: try without expanding if count == 1
  where prog = defaultProgram
        baseProgPair = ("prog.exec", (String "foo"))

{-# LANGUAGE OverloadedStrings #-}
module Angel.ConfigSpec (spec) where

import Angel.Data hiding (spec, Spec)
import Angel.Config

import Control.Exception.Base
import Data.Configurator.Types (Value(..))
import qualified Data.HashMap.Lazy as HM

import SpecHelper

spec :: TestTree
spec = testGroup "Angel.Config"
  [
    testGroup "modifyProg"
    [
      testCase "modifies exec" $
        modifyProg prog "exec" (String "foo") @?=
        prog { exec = Just "foo"}

    , testCase "errors for non-string execs" $
        evaluate (modifyProg prog "exec" (Bool True)) `shouldThrow`
        anyErrorCall

    , testCase "modifies delay for positive numbers" $
        modifyProg prog "delay" (Number 1) @?=
        prog { delay = Just 1}
    , testCase "modifies delay for 0" $
        modifyProg prog "delay" (Number 0) @?=
        prog { delay = Just 0}
    , testCase "errors on negative delays" $
        evaluate (modifyProg prog "delay" (Number (-1))) `shouldThrow`
        anyErrorCall

    , testCase "modifies stdout" $
        modifyProg prog "stdout" (String "foo") @?=
        prog { stdout = Just "foo"}
    , testCase "errors for non-string stdout" $
        evaluate (modifyProg prog "stdout" (Bool True)) `shouldThrow`
        anyErrorCall

    , testCase "modifies stderr" $
        modifyProg prog "stderr" (String "foo") @?=
        prog { stderr = Just "foo"}
    , testCase "errors for non-string stderr" $
        evaluate (modifyProg prog "stderr" (Bool True)) `shouldThrow`
        anyErrorCall

    , testCase "modifies directory" $
        modifyProg prog "directory" (String "foo") @?=
        prog { workingDir = Just "foo"}
    , testCase "errors for non-string directory" $
        evaluate (modifyProg prog "directory" (Bool True)) `shouldThrow`
        anyErrorCall

    , testCase "modifies pidfile" $
        modifyProg prog "pidfile" (String "foo.pid") @?=
        prog { pidFile = Just "foo.pid"}
    , testCase "errors for non-string path" $
        evaluate (modifyProg prog "pidfile" (Bool True)) `shouldThrow`
        anyErrorCall

    , testCase "appends env to the empty list" $
        modifyProg prog "env.foo" (String "bar") @?=
        prog { env = [("foo", "bar")]}
    , testCase "errors for non-string value" $
        evaluate (modifyProg prog "env.foo" (Bool True)) `shouldThrow`
        anyErrorCall
    , testCase "prepends env to an existing list" $
        modifyProg prog { env = [("previous", "value")]} "env.foo" (String "bar") @?=
        prog { env = [("foo", "bar"), ("previous", "value")]}

    , testCase "interprets boolean False as Nothing" $
        modifyProg prog "termgrace" (Bool False) @?=
        prog { termGrace = Nothing }
    , testCase "interprets 0 as Nothing" $
        modifyProg prog "termgrace" (Number 0) @?=
        prog { termGrace = Nothing }
    , testCase "interprets > 0 as a set termGrace" $
        modifyProg prog "termgrace" (Number 2) @?=
        prog { termGrace = Just 2 }
    , testCase "interprets boolean True as an error" $
        evaluate (modifyProg prog "termgrace" (Bool True)) `shouldThrow`
        anyErrorCall
    , testCase "interprets negative numbers as an error" $
        evaluate (modifyProg prog "termgrace" (Number (-1))) `shouldThrow`
        anyErrorCall
    , testCase "interprets anything else as an error" $
        evaluate (modifyProg prog "termgrace" (String "yeah")) `shouldThrow`
        anyErrorCall

    , testCase "does nothing for all other cases" $
        modifyProg prog "bogus" (String "foo") @?=
        prog
    ]

  , testGroup "expandByCount"
    [
      testCase "doesn't affect empty hashes" $
        expandByCount HM.empty @?=
          HM.empty
    , testCase "doesn't affect hashes without counts" $
        expandByCount (HM.fromList [baseProgPair]) @?=
          HM.fromList [baseProgPair]
    , testCase "errors on mistyped count field" $
        evaluate (expandByCount (HM.fromList [baseProgPair
                                             , ("prog.count", String "wat")])) `shouldThrow`
          anyErrorCall
    , testCase "errors on negative count field" $
        evaluate (expandByCount (HM.fromList [ baseProgPair
                                             , ("prog.count", Number (-1))])) `shouldThrow`
          anyErrorCall
    , testCase "generates no configs with a count of 0" $
        expandByCount (HM.fromList [ baseProgPair
                                   , ("prog.count", Number 0)]) @?=
          HM.empty
    , testCase "expands with a count of 1" $
        expandByCount (HM.fromList [baseProgPair, ("prog.count", Number 1)]) @?=
          HM.fromList [ ("prog-1.exec", String "foo")
                      , ("prog-1.env.ANGEL_PROCESS_NUMBER", String "1")]
    , testCase "expands with a count of > 1" $
        expandByCount (HM.fromList [baseProgPair, ("prog.count", Number 2)]) @?=
          HM.fromList [ ("prog-1.exec", String "foo")
                      , ("prog-1.env.ANGEL_PROCESS_NUMBER", String "1")
                      , ("prog-2.exec", String "foo")
                      , ("prog-2.env.ANGEL_PROCESS_NUMBER", String "2")]
    , testCase "preserves explicit env variables" $
        expandByCount (HM.fromList [baseProgPair, ("prog.env.FOO", String "bar")]) @?=
          HM.fromList [ ("prog.exec",    String "foo")
                      , ("prog.env.FOO", String "bar")]
    , testCase "expands pidfiles with a count of 1" $
        expandByCount (HM.fromList [ baseProgPair
                                   , ("prog.count", Number 1)
                                   , ("prog.pidfile", String "foo.pid")]) @?=
          HM.fromList [ ("prog-1.exec", String "foo")
                      , ("prog-1.env.ANGEL_PROCESS_NUMBER", String "1")
                      , ("prog-1.pidfile", String "foo-1.pid")] --TODO: try without expanding if count == 1
    ]

  , testGroup "processConfig internal API"
    [
      testCase "can parse the example config" $
        shouldReturnRight $ processConfig "example.conf"
    ]
  ]
  where prog = defaultProgram
        baseProgPair = ("prog.exec", String "foo")
        shouldReturnRight a = flip shouldSatisfy isRight =<< a

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

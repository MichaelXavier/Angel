module Angel.LogSpec (spec) where

import Angel.Log
import System.Time (CalendarTime(..),
                    Day(..),
                    Month(..))

import Test.Hspec
import qualified Test.Hspec as H (Spec)

spec :: H.Spec
spec = do
  describe "cleanCalendar" $ do
    it "formats the time correctly" $ cleanCalendar time `shouldBe` "2012/09/12 03:14:59"
  where time = CalendarTime 2012 September 12 3 14 59 0 Tuesday 263 "Pacific" 25200 True

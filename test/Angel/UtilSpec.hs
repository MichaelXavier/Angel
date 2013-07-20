module Angel.UtilSpec (spec) where

import Angel.Util

import System.Posix.User (getEffectiveUserID,
                          getUserEntryForID,
                          UserEntry(..))

import SpecHelper

spec :: Spec
spec = do
  describe "expandPath" $ do
    it "generates the correct path for just a tilde" $ do
      UserEntry { homeDirectory = home } <- getUserEntry
      path <- expandPath "~/foo"
      path `shouldBe` home ++ "/foo"
    it "generates the correct path for tilde with a specific user" $ do
      UserEntry { homeDirectory = home,
                  userName      = user } <- getUserEntry
      path <- expandPath $ "~" ++ user ++ "/foo"
      path `shouldBe` home ++ "/foo"
    it "leaves paths without tildes alone" $ do
      path <- expandPath "/foo"
      path `shouldBe` "/foo"
  where getUserEntry = getUserEntryForID =<< getEffectiveUserID

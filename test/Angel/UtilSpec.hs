module Angel.UtilSpec (spec) where

import Angel.Util

import System.Posix.User (getEffectiveUserID,
                          getUserEntryForID,
                          UserEntry(..))

import SpecHelper

spec :: TestTree
spec = testGroup "Angel.Util"
  [
    testGroup "expandPath"
    [
      testCase "generates the correct path for just a tilde" $ do
        UserEntry { homeDirectory = home } <- getUserEntry
        path <- expandPath "~/foo"
        path @?= home ++ "/foo"
    , testCase "generates the correct path for tilde with a specific user" $ do
        UserEntry { homeDirectory = home,
                    userName      = user } <- getUserEntry
        path <- expandPath $ "~" ++ user ++ "/foo"
        path @?= home ++ "/foo"
    , testCase "leaves paths without tildes alone" $ do
        path <- expandPath "/foo"
        path @?= "/foo"
    ]
  , testGroup "split"
    [
      testProperty "produces no null values" $ \(a :: Char) (xs :: [Char]) ->
        none null $ split a xs
    , testProperty "produces no instances of the split element" $ \(a :: Char) (xs :: [Char]) ->
        none (elem a) $ split a xs
    , testCase "splits" $
        split ' ' "  foo  bar     baz  " @?= ["foo", "bar", "baz"]
    ]
  ]
  where getUserEntry = getUserEntryForID =<< getEffectiveUserID
        none p = not . any p

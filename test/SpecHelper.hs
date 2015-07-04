module SpecHelper
    ( module X
    , module SpecHelper
    ) where

import Control.Exception
import Test.Tasty as X
import Test.Tasty.HUnit as X
import Test.Tasty.QuickCheck as X



-------------------------------------------------------------------------------
shouldReturn :: (Show a, Eq a) => IO a -> a -> Assertion
shouldReturn f v = do
  v' <- f
  v' @?= v


-------------------------------------------------------------------------------
shouldThrow :: Exception e => IO a -> (e -> Bool) -> Assertion
shouldThrow f p = do
  res <- try f
  either (`shouldSatisfy` p) (const $ assertFailure "Did not throw an exception") res


-------------------------------------------------------------------------------
anyErrorCall :: ErrorCall -> Bool
anyErrorCall = const True


-------------------------------------------------------------------------------
shouldSatisfy :: (Show a) => a -> (a -> Bool) -> Assertion
shouldSatisfy a p = assertBool ("predicate failed on " ++ show a ) (p a)

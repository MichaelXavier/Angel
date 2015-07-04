module Main (main) where

-------------------------------------------------------------------------------
import qualified Angel.ConfigSpec
import qualified Angel.JobSpec
import qualified Angel.LogSpec
import qualified Angel.PidFileSpec
import qualified Angel.UtilSpec
-------------------------------------------------------------------------------



main :: IO ()
main = defaultMain $ testGroup "angel"
  [
    Angel.ConfigSpec.spec
  , Angel.JobSpec.spec
  , Angel.LogSpec.spec
  , Angel.PidFileSpec.spec
  , Angel.UtilSpec.spec
  ]

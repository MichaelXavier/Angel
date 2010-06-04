module Angel.Data where

import qualified Data.Map as M
import System.Process (createProcess, proc, waitForProcess, ProcessHandle)
import System.Process (terminateProcess, CreateProcess(..), StdStream(..))

data GroupConfig = GroupConfig {
    spec :: SpecKey,
    running :: RunKey
}

type SpecKey = M.Map ProgramId Program
type RunKey = M.Map ProgramId (Program, Maybe ProcessHandle)
type ProgramId = String

data Program = Program {
    name :: String,
    exec :: String,
    delay :: Int,
    stdout :: String,
    stderr :: String
} deriving (Show, Eq, Ord)

type Spec = [Program]
type Kw = Maybe (String, String)

defaultProgram = Program{
    name="",
    exec="",
    delay=5,
    stdout="/dev/null",
    stderr="/dev/null"
}

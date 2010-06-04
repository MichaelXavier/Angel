module Angel.Data where

import qualified Data.Map as M
import System.Process (createProcess, proc, waitForProcess, ProcessHandle)
import System.Process (terminateProcess, CreateProcess(..), StdStream(..))

-- |the whole shared state of the program; spec is what _should_
-- |be running, while running is what actually _is_ running_ currently
data GroupConfig = GroupConfig {
    spec :: SpecKey,
    running :: RunKey
}

-- |map program ids to relevant structure
type SpecKey = M.Map ProgramId Program
type RunKey = M.Map ProgramId (Program, Maybe ProcessHandle)
type ProgramId = String

-- |the representation of a program is these 5 values, 
-- |read from the config file
data Program = Program {
    name :: String,
    exec :: String,
    delay :: Int,
    stdout :: String,
    stderr :: String
} deriving (Show, Eq, Ord)

-- |Lower-level atoms in the configuration process
type Spec = [Program]

-- |a template for an empty program; the variable set to ""
-- |are required, and must be overridden in the config file
defaultProgram = Program{
    name="",
    exec="",
    delay=5,
    stdout="/dev/null",
    stderr="/dev/null"
}

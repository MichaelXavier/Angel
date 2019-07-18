{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Angel.Files ( getFile ) where

import System.IO ( Handle
                 , openFile
                 , IOMode(AppendMode) )

import Angel.Data ( GroupConfig )
import Angel.Prelude

getFile :: String -> GroupConfig -> IO Handle
getFile path _ = openFile path AppendMode

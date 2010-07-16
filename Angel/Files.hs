module Angel.Files (getFile, startFileManager) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan (readTChan, writeTChan, TChan, newTChan, newTChanIO)
import Control.Monad (forever)
import System.IO (Handle, hClose, openFile, IOMode(..), hIsClosed)
import GHC.IO.Handle (hDuplicate)
import Debug.Trace (trace)
import Angel.Data (GroupConfig(..), FileRequest)

startFileManager req = forever $ fileManager req

fileManager :: TChan FileRequest -> IO ()
fileManager req = do 
    (path, resp) <- atomically $ readTChan req
    hand <- openFile path AppendMode
    hand' <- hDuplicate hand
    hClose hand
    atomically $ writeTChan resp hand'
    fileManager req

getFile :: String -> GroupConfig -> IO Handle
getFile path cfg = do
    resp <- newTChanIO
    atomically $ writeTChan (fileRequest cfg) (path, resp)
    hand <- atomically $ readTChan resp
    return hand

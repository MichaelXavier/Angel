module Angel.Files (getFile, startFileManager) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan (readTChan, writeTChan, TChan, newTChan, newTChanIO)
import Control.Monad (forever)
import System.IO (Handle, hClose, openFile, IOMode(..), hIsClosed)
import GHC.IO.Handle (hDuplicate)
import Debug.Trace (trace)
import Angel.Data (GroupConfig(..), FileRequest)
import Angel.Log

startFileManager req = forever $ fileManager req

fileManager :: TChan FileRequest -> IO ()
fileManager req = do 
    (path, resp) <- atomically $ readTChan req
    mh <- catch  (openFile path AppendMode >>= \h-> return $ Just h) (\e-> return Nothing)
    case mh of
        Just hand -> do
            hand' <- hDuplicate hand
            hClose hand
            atomically $ writeTChan resp (Just hand')
        Nothing -> atomically $ writeTChan resp Nothing
    fileManager req

getFile :: String -> GroupConfig -> IO Handle
getFile path cfg = do
    resp <- newTChanIO
    atomically $ writeTChan (fileRequest cfg) (path, resp)
    mh <- atomically $ readTChan resp
    hand <- case mh of
        Just hand -> return hand
        Nothing -> do
                   logger "get-file" $ "Could not open stdout/stderr file " ++ path
                   error $ "could not open stdout/stderr file " ++ path
    return hand

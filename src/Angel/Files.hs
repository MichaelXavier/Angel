{-# LANGUAGE ScopedTypeVariables #-}
module Angel.Files ( getFile
                   , startFileManager ) where

import Control.Exception ( try
                         , SomeException )
import Control.Concurrent.STM ( readTChan
                              , writeTChan
                              , atomically
                              , TChan
                              , newTChan
                              , newTChanIO )
import Control.Monad (forever)
import System.IO ( Handle
                 , hClose
                 , openFile
                 , IOMode(..) )
import GHC.IO.Handle (hDuplicate)
import Angel.Data ( GroupConfig(..)
                  , FileRequest )

startFileManager req = forever $ fileManager req

fileManager :: TChan FileRequest -> IO ()
fileManager req = do 
    (path, resp) <- atomically $ readTChan req
    mh <- try $ openFile path AppendMode
    case mh of
        Right hand -> do
            hand' <- hDuplicate hand
            hClose hand
            atomically $ writeTChan resp (Just hand')
        Left (e :: SomeException) -> atomically $ writeTChan resp Nothing
    fileManager req

getFile :: String -> GroupConfig -> IO Handle
getFile path cfg = do
    resp <- newTChanIO
    atomically $ writeTChan (fileRequest cfg) (path, resp)
    mh <- atomically $ readTChan resp
    hand <- case mh of
        Just hand -> return hand
        Nothing -> error $ "could not open stdout/stderr file " ++ path
    return hand

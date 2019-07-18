{-# LANGUAGE NoImplicitPrelude #-}
-- |various utility functions
module Angel.Util ( sleepSecs
                  , waitForWake
                  , expandPath
                  , split
                  , strip
                  , nnull ) where

import Angel.Prelude
import Control.Concurrent.STM (atomically
                              , retry
                              , TVar
                              , readTVar
                              , writeTVar)
import Control.Concurrent (threadDelay)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import System.Posix.User (getEffectiveUserName,
                          UserEntry(homeDirectory),
                          getUserEntryForName)

-- |sleep for `s` seconds in an thread
sleepSecs :: Int -> IO ()
sleepSecs s = threadDelay $ s * 1000000

-- |wait for the STM TVar to be non-nothing
waitForWake :: TVar (Maybe Int) -> IO ()
waitForWake wakeSig = atomically $ do
    state <- readTVar wakeSig
    case state of
        Just _ -> writeTVar wakeSig Nothing
        Nothing -> retry

expandPath :: FilePath -> IO FilePath
expandPath ('~':rest) = do home <- getHome =<< getUser
                           return $ home ++ relativePath
  where (userName, relativePath) = span (/= '/') rest
        getUser                  = if null userName
                                     then getEffectiveUserName
                                     else return userName
        getHome user             = homeDirectory `fmap` getUserEntryForName user
expandPath path = return path

nnull :: [a] -> Bool
nnull = not . null

split :: Eq a => a -> [a] -> [[a]]
split a = catMaybes . foldr go []
  where
    go x acc = case (x == a, acc) of
      (True, xs) -> Nothing:xs
      (False, []) -> [Just [x]]
      (False, Nothing:rest) -> Just [x]:Nothing:rest
      (False, Just xs:rest) -> Just (x:xs):rest

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

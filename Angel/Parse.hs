-- |Parse Angel configuration files--a parsec parser
module Angel.Parse where

import Data.String.Utils (strip)
import Text.ParserCombinators.Parsec
import Data.Maybe (isJust)
import Data.Either (Either(..))
import Control.Monad (foldM)

import Angel.Data

type Kw = Maybe (String, String)

reqInt = manyTill (oneOf ['0'..'9']) (char '\n') <?> "integer"
configString = manyTill anyChar $ char '\n'

configLine :: GenParser Char st Kw
configLine = do name <- manyTill (noneOf "[") (char ' ')
                val <- case name of 
                        "exec" -> configString
                        "delay" -> reqInt
                        "stdout" -> configString
                        "stderr" -> configString
                        otherwise -> fail $ "unknown config verb '" ++ name ++ "'"
                return $ Just (strip name, strip val)

commentLine :: GenParser Char st Kw
commentLine = do manyTill (oneOf " \t") $ char '#'
                 manyTill anyChar $ char '\n'
                 return Nothing

emptyLine :: GenParser Char st Kw
emptyLine = manyTill (oneOf " \t\r") (char '\n') >> return Nothing

header = do char '[' <?> "start program id"
            name <- manyTill anyChar $ char ']'
            manyTill (oneOf " \t\r") $ char '\n'
            return name

program = do given_id <- header
             kw <- many $ (emptyLine <|> commentLine <|> configLine)
             let real_kw = filter isJust kw
             let prg = defaultProgram{name=given_id}
             prg <- foldM setAttr prg real_kw
             mapM_ (check_set prg) [("exec", exec), ("name", name)] 
             return prg

    where 
    
        setAttr prg (Just (n, v))  = case n of 
                                    "exec" -> return prg{exec=v}
                                    "delay" -> return prg{delay=(read v)::Int}
                                    "stdout" -> return prg{stdout=v}
                                    "stderr" -> return prg{stderr=v}
                                    otherwise -> fail $ "unknown config keyword " ++ n
        setAttr _ _ = fail "non-just in setAttr argument??"

        check_set prg (name, f) = if f prg == f defaultProgram 
                            then fail $ name ++ " must be set for all programs" 
                            else return ()

configFile :: GenParser Char st Spec
configFile = do many $ (emptyLine <|> commentLine)
                return =<< manyTill program eof

parseConfig :: String -> Either ParseError Spec
parseConfig input = parse configFile "(config)" input

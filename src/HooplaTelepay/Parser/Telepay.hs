module HooplaTelepay.Parser.Telepay(Telepay, parseTelepay) where

import HooplaTelepay.Parser.Record
import           Text.ParserCombinators.Parsec


data Telepay =
  Telepay { tRecords :: [Record] }
  deriving Show

parseTelepay :: Parser Telepay
parseTelepay =
  do records <- many parseRecord
     char '\n'
     eof
     return $ Telepay records

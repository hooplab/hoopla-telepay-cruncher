module HooplaTelepay.Parser.Telepay(Telepay, parseTelepay) where

import           HooplaTelepay.Model
import           HooplaTelepay.Parser.Record
import           Text.ParserCombinators.Parsec

parseTelepay :: Parser Telepay
parseTelepay =
  do records <- many parseRecord
     char '\n'
     eof
     return $ Telepay records

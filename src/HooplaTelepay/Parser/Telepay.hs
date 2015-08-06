module HooplaTelepay.Parser.Telepay(Telepay, parseTelepay) where

import HooplaTelepay.Parser.Record
import           Text.ParserCombinators.Parsec


data Telepay =
  Telepay { tBETFOR00 :: Record
          , tRecords :: [Record]
          , tBETFOR99 :: Record
          }


parseTelepay :: Parser Telepay
parseTelepay =
  do betfor00 <- parseRecord "00"
     --betfor99 <- parseRecord "99"
     return $ Telepay betfor00 [] betfor00

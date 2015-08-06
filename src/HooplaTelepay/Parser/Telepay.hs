module HooplaTelepay.Parser.Telepay(Telepay, parseTelepay) where

import HooplaTelepay.Parser.Record
import           Text.ParserCombinators.Parsec


data Telepay =
  Telepay { tBETFOR00 :: Record
          , tRecords :: [Record]
          , tBETFOR99 :: Record
          }
  deriving Show

parseTelepay :: Parser Telepay
parseTelepay =
  do betfor00 <- parseRecord
     string "PELLEPELLEPELLEPELLE"
     betfor21 <- parseRecord
     string "SEPERATOR"
     betfor23 <- parseRecord
     string "seperator"
     betfor99 <- parseRecord
     string "pelle"
     return $ Telepay betfor00 [betfor21, betfor23] betfor99

module HooplaTelepay.Parser.Record(Record, SpecificRecord(..)) where

import           Control.Monad                          (replicateM)
import           HooplaTelepay.Parser.ApplicationHeader
import           Text.ParserCombinators.Parsec

data Record = Record Applikasjonsheader SpecificRecord
            deriving Show

data SpecificRecord =
  BETFOR00 { r_foretaksnummer      :: String
           , r_divisjon            :: String
           , r_sekvenskontrollfelt :: Int
           , r_produksjonsdato     :: String
           }

  deriving Show

testBETFOR00 :: Record
testBETFOR00 =
  Record testApplikasjonsheader $
  BETFOR00 { r_foretaksnummer = "00000000000"
           , r_divisjon       = "           "
           , r_sekvenskontrollfelt = 1
           , r_produksjonsdato = "0101"
           }

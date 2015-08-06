module HooplaTelepay.Parser.Record(Record, parseRecord) where

import           Control.Monad                           (replicateM)
import           HooplaTelepay.Parser.Applikasjonsheader
import           Text.ParserCombinators.Parsec

data Record = BETFOR00 { r00_applikasjonsheader  :: Applikasjonsheader
                       , r00_foretaksnummer      :: String
                       , r00_divisjon            :: String
                       , r00_sekvenskontrollfelt :: Int
                       , r00_produksjonsdato     :: String
                       , r00_passord             :: String
                       , r00_nytt_passord        :: String
                       , r00_operatornummer      :: String
                       , r00_egenreferanse       :: String
                       }
            | BETFOR21 { r21_applikasjonsheader :: Applikasjonsheader }
            | BETFOR23 { r23_applikasjonsheader :: Applikasjonsheader }
            | BETFOR99 { r99_applikasjonsheader :: Applikasjonsheader
                       , r99_foretaksnummer     :: String
                       }
            deriving Show


parseRecord :: String -> Parser Record
parseRecord r =
  do applikasjonsheader <- parseApplikasjonsheader
     -- recordStr <- replicateM (80 * ah_antall_a_80 applikasjonsheader) anyChar
     -- redundant?
     parseSpecificRecord r applikasjonsheader

parseSpecificRecord :: String -> Applikasjonsheader -> Parser Record
parseSpecificRecord "00" applikasjonsheader =
  do string "BETFOR00"
     foretaksnummer <- replicateM 11 digit
     divisjon       <- replicateM 11 anyChar
     skf <- replicateM 4 digit
     let sekvenskontrollfelt = read skf
     replicateM 6 space
     produksjonsdato <- replicateM 4 digit
     passord <- replicateM 10 anyChar
     rutineversjon <- string "VERSJON002"
     nytt_passord <- replicateM 10 anyChar
     operatornummer <- replicateM 11 anyChar

     -- skip sigill
     replicateM 28 anyChar

     -- reservert
     replicateM 143 anyChar

     egenreferanse <- replicateM 15 anyChar

     -- reservert
     replicateM 9 anyChar

     return $ BETFOR00 { r00_applikasjonsheader = applikasjonsheader
                       , r00_foretaksnummer     = foretaksnummer
                       , r00_divisjon = divisjon
                       , r00_sekvenskontrollfelt = sekvenskontrollfelt
                       , r00_produksjonsdato = produksjonsdato
                       , r00_passord = passord
                       , r00_nytt_passord = nytt_passord
                       , r00_operatornummer = operatornummer
                       , r00_egenreferanse = egenreferanse
                       }

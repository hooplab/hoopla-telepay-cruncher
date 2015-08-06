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
            | BETFOR99 { r99_applikasjonsheader  :: Applikasjonsheader
                       , r99_foretaksnummer      :: String
                       , r99_sekvenskontrollfelt :: Int
                       , r99_produksjonsdato     :: String
                       , r99_antall_oppdrag      :: Int
                       , r99_totalsum_fil        :: Int
                       , r99_antall_records      :: Int
                       , r99_versjon_software    :: String
                       , r99_versjon_bank        :: String
                       }

            deriving Show


parseRecord :: Parser Record
parseRecord =
  do applikasjonsheader <- parseApplikasjonsheader
     recordid <- replicateM 8 anyChar
     parseSpecificRecord recordid applikasjonsheader

parseSpecificRecord :: String -> Applikasjonsheader -> Parser Record
parseSpecificRecord "BETFOR00" applikasjonsheader =
  do foretaksnummer <- parseForetaksnummer
     divisjon       <- replicateM 11 anyChar
     sekvenskontrollfelt <- parseSekvenskontrollfelt
     replicateM 6 space
     produksjonsdato <- parseProduksjonsdato
     passord <- replicateM 10 anyChar
     rutineversjon <- string "VERSJON002"
     nytt_passord <- replicateM 10 anyChar
     operatornummer <- replicateM 11 anyChar

     -- skip sigill
     replicateM 28 anyChar

     -- reservert
     replicateM 143 space

     egenreferanse <- replicateM 15 anyChar

     -- reservert
     replicateM 9 space

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

parseSpecificRecord "BETFOR99" applikasjonsheader =
  do foretaksnummer <- parseForetaksnummer

     -- reservert
     replicateM 11 space

     sekvenskontrollfelt <- parseSekvenskontrollfelt

     -- reservert
     replicateM 6 space

     produksjonsdato <- parseProduksjonsdato

     antall_oppdrag <- fmap read $ replicateM 4 digit

     totalsum_fil <- fmap read $ replicateM 15 digit

     antall_records <- fmap read $ replicateM 5 digit

     -- reservert
     replicateM 163 space

     -- skip sigill
     replicateM 25 anyChar

     versjon_software <- replicateM 16 anyChar

     versjon_bank     <- replicateM 8  anyChar


     return $ BETFOR99 { r99_applikasjonsheader = applikasjonsheader
                       , r99_foretaksnummer = foretaksnummer
                       , r99_sekvenskontrollfelt = sekvenskontrollfelt
                       , r99_produksjonsdato = produksjonsdato
                       , r99_antall_oppdrag = antall_oppdrag
                       , r99_totalsum_fil = totalsum_fil
                       , r99_antall_records = antall_records
                       , r99_versjon_software = versjon_software
                       , r99_versjon_bank = versjon_bank
                       }


parseForetaksnummer :: Parser String
parseForetaksnummer = replicateM 11 digit

parseSekvenskontrollfelt :: Parser Int
parseSekvenskontrollfelt = fmap read $ replicateM 4 digit

parseProduksjonsdato :: Parser String
parseProduksjonsdato = replicateM 4 digit

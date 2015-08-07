module HooplaTelepay.Parser.Applikasjonsheader(Applikasjonsheader(..), parseApplikasjonsheader) where


import           Control.Monad                 (replicateM, replicateM_)
import           Text.ParserCombinators.Parsec


data Applikasjonsheader =
  Applikasjonsheader { ah_id          :: String
                     , ah_versjon     :: String
                     , ah_returkode   :: String
                     , ah_rutineid    :: String
                     , ah_transdato   :: String
                     , ah_trans_seknr :: Int
                     , ah_antall_a_80 :: Int
                    }
  deriving Show

parseApplikasjonsheader :: Parser Applikasjonsheader
parseApplikasjonsheader =
  do -- AH- ID Applikasjonsheader (Posisjon 1-2)
     id <- string "AH"
     -- AH-VERSJON Applikasjonsheader (Posisjon 3)
     versjon <- string "2"
     -- AH-RETURKODE Applikasjonsheader (Posisjon 4-5)
     returkode <- parseReturkode
     -- AH-Rutine-ID Applikasjonsheader (Posisjon 6-9)
     rutineid  <- parseRutineid
     -- AH-transdato i alle BETFORXX Applikasjonsheader (Posisjon 10-13)
     transdato <- parseTransdato
     -- AH-TRANS-SEKVNR. Applikasjonsheader (Posisjon 14-19)
     trans_seknr <- parseTransseknr

     replicateM_ 19 space -- 19 blank spaces

     antall_a_80 <- parseAntalla80

     return Applikasjonsheader { ah_id = id
                               , ah_versjon = versjon
                               , ah_returkode = returkode
                               , ah_rutineid = rutineid
                               , ah_transdato = transdato
                               , ah_trans_seknr = trans_seknr
                               , ah_antall_a_80 = antall_a_80
                               }

parseReturkode :: Parser String
parseReturkode = choice $ map trystring returkoder
  where returkoder =
          [ "00", "01", "02", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "25", "26"
          , "27", "28", "29", "30", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47"
          , "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "95" ]

parseRutineid :: Parser String
parseRutineid = replicateM 4 anyChar

parseTransdato :: Parser String
parseTransdato =
  do m <- month
     d <- day
     return $ m++d
  where month = choice $ map trystring months
        months = [ "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"]

        -- validerer ikke om dagene passer i maaneden
        day = choice $ map trystring days
        days = [ "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"]

parseTransseknr :: Parser Int
parseTransseknr =
  do n <- replicateM 6 digit
     return $ read n

parseAntalla80 :: Parser Int
parseAntalla80 =
  do n <- replicateM 2 digit
     return $ read n

-- string matching with backgracking.
trystring = try . string

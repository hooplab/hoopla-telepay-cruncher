module HooplaTelepay.Parser.ApplicationHeader(Applikasjonsheader, testApplikasjonsheader, parseApplikasjonsheader) where


import           Control.Monad                 (replicateM)
import           Text.ParserCombinators.Parsec


data Applikasjonsheader =
  Applikasjonsheader { ah_id          :: String
                     , ah_versjon     :: String
                     , ah_returkode   :: String
                     , ah_rutine_id   :: String
                     , ah_transdato   :: String
                     , ah_trans_seknr :: Int
                     , ah_antall_a_80 :: Int
                    }
  deriving Show

testApplikasjonsheader :: Applikasjonsheader
testApplikasjonsheader =
  Applikasjonsheader { ah_id          = "AH"
                     , ah_versjon     = "2"
                     , ah_returkode   = "00"
                     , ah_rutine_id   = "TBII"
                     , ah_transdato   = "0101"
                     , ah_trans_seknr = 1
                     , ah_antall_a_80 = 1
                     }

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
     transseknr <- parseTransseknr

     replicateM 17 space -- 17 blank spaces

     antall_a <- parseAntall_a
     return $
       Applikasjonsheader id versjon returkode rutineid transdato (read transseknr) (read antall_a)

parseReturkode :: Parser String
parseReturkode = choice $ map trystring returkoder
  where returkoder =
          [ "00", "01", "02", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "25", "26"
          , "27", "28", "29", "30", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47"
          , "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "95" ]

parseRutineid :: Parser String
parseRutineid = choice $ map trystring [ "TBII", "TBRI", "TBIO", "TBRO", "TBIU", "TBRU" ]

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
  do n1 <- digit
     n2 <- digit
     n3 <- digit
     n4 <- digit
     n5 <- digit
     if n1 == '0' &&
        n2 == '0' &&
        n3 == '0' &&
        n4 == '0' &&
        n5 == '0'
       then nonzerodigit
       else digit
  where nonzerodigit = oneOf ['1','2','3','4','5','6','7','8','9']


-- string matching with backgracking.
trystring = try . string

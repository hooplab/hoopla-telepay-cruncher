module HooplaTelepay.Parser.Record(Record, parseRecord) where

import           Control.Monad                           (replicateM)
import           HooplaTelepay.Parser.Applikasjonsheader
import           Text.ParserCombinators.Parsec

data Record = BETFOR00 { r00_applikasjonsheader  :: Applikasjonsheader
                       , r00_foretaksnummer      :: String
                       , r00_divisjon            :: String
                       , r00_sekvenskontrollfelt :: Int
                       , r00_produksjonsdato     :: String
                       }
            | BETFOR21 { r21_applikasjonsheader :: Applikasjonsheader }
            | BETFOR23 { r23_applikasjonsheader :: Applikasjonsheader }
            | BETFOR99 { r99_applikasjonsheader :: Applikasjonsheader
                       , r99_foretaksnummer     :: String
                       }
            deriving Show


parseRecord :: String -> Parser Record
parseRecord n =
  do applikasjonsheader <- parseApplikasjonsheader
     recordStr <- replicateM (80 * ah_antall_a_80 applikasjonsheader) anyChar
     return $ BETFOR21 applikasjonsheader

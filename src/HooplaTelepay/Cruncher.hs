module HooplaTelepay.Cruncher(crunchTelepay) where

import           Data.Char           (isSpace)
import           Data.List           (intersperse)
import           Data.List.Split     (chunksOf)
import           HooplaTelepay.Model


crunchTelepay :: Telepay -> IO ()
crunchTelepay tp = mapM_ summarizeRecord $ tRecords tp



summarizeRecord :: Record -> IO ()
summarizeRecord (BETFOR21 _ _ fra_kontonr sekvenskontrollfelt _ betalingsdato
                 egenref_oppdrag mottaker_kontonr mottaker_navn
                 adresse1 adresse2 postnr poststed _ _ _ _ belop _ _ _ _ _ ) =
  putStrLn summary
  where summary = "Sekvenskontrollfelt: " ++ show sekvenskontrollfelt ++ "\n" ++
                  "----------------------------------------------\n" ++
                  "Fra kontonr: " ++ fra_kontonr ++ " paa dato (yy/mm/dd) " ++ prettydato betalingsdato ++
                  " vil vi betale totalt " ++ prettybelop belop ++ " kr til:\n" ++
                  trim mottaker_navn ++ ", konto: " ++ mottaker_kontonr ++ "\n" ++
                  (prettyadr adresse1 adresse2) ++ ", " ++ postnr ++ " " ++ trim poststed


        prettydato :: String -> String
        prettydato (y:y':m:m':d:d':[]) = y:y':'/':m:m':'/':d:d':[]
        prettydato _ = error "invalid date format"

        prettyadr :: String -> String -> String
        prettyadr adr1 adr2
          | null adr2' = adr1'
          | otherwise  = adr1' ++ " " ++ adr2'
          where adr1' = trim adr1
                adr2' = trim adr2

        prettybelop :: Integer -> String
        prettybelop ore
          | lpb  == 1 = "0.0" ++ pb
          | lpb == 2  = "0."  ++ pb
          | lpb == 3  = head pb:"." ++ tail pb
          | otherwise = (concat $ reverse $ intersperse " " $ map reverse $ chunksOf 3 $ reverse (take (lpb-2) pb)) ++ "." ++ drop (lpb-2) pb
          where pb = show ore
                lpb = length pb


summarizeRecord _ = return ()



-- veldig ineffektiv, XXX: if slow then use Text and not String.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

module HooplaTelepay.Parser.Record(Record, parseRecord) where

import           Control.Monad                           (replicateM,
                                                          replicateM_)
import           HooplaTelepay.Model
import           HooplaTelepay.Parser.Applikasjonsheader
import           Text.ParserCombinators.Parsec


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
     replicateM_ 6 space
     produksjonsdato <- parseProduksjonsdato
     passord <- replicateM 10 anyChar
     rutineversjon <- string "VERSJON002"
     nytt_passord <- replicateM 10 anyChar
     operatornummer <- replicateM 11 anyChar

     -- skip sigill
     replicateM_ 28 anyChar
     -- reservert
     replicateM_ 143 space

     egenreferanse <- replicateM 15 anyChar

     -- reservert
     replicateM_ 9 space

     return BETFOR00 { r00_applikasjonsheader = applikasjonsheader
                     , r00_foretaksnummer     = foretaksnummer
                     , r00_divisjon = divisjon
                     , r00_sekvenskontrollfelt = sekvenskontrollfelt
                     , r00_produksjonsdato = produksjonsdato
                     , r00_passord = passord
                     , r00_nytt_passord = nytt_passord
                     , r00_operatornummer = operatornummer
                     , r00_egenreferanse = egenreferanse
                     }

parseSpecificRecord "BETFOR21" applikasjonsheader =
  do foretaksnummer <- parseForetaksnummer
     kontonummer <- replicateM 11 digit
     sekvenskontrollfelt <- parseSekvenskontrollfelt
     referansenummer <- replicateM 6 anyChar
     betalingsdato  <- replicateM 6 digit
     egenrefoppdrag <- replicateM 30 anyChar

     -- reservert
     space

     mottaker_kontonr <- replicateM 11 digit
     mottaker_navn  <- replicateM 30 anyChar
     adresse_1 <- replicateM 30 anyChar
     adresse_2 <- replicateM 30 anyChar
     postnr <- replicateM 4 digit
     poststed <- replicateM 26 anyChar
     belop_til_egen_konto <- replicateM 15 digit
     tekstkode <- replicateM 3 digit
     transaksjonstype <- replicateM 1 anyChar
     slettekode <- replicateM 1 anyChar
     totalbelop <- fmap read $ replicateM 15 digit
     klientreferanse <- replicateM 5 anyChar
     valuteringsdato <- replicateM 6 digit
     valuteringmottat <- replicateM 6 digit
     slettearsak <- replicateM 1 anyChar

     -- reservert
     replicateM_ 9 space

     blankettnummer <- replicateM 10 digit

     return BETFOR21 { r21_applikasjonsheader = applikasjonsheader
                     , r21_foretaksnummer = foretaksnummer
                     , r21_kontonummer = kontonummer
                     , r21_sekvenskontrollfelt = sekvenskontrollfelt
                     , r21_referansenummer = referansenummer
                     , r21_betalingsdato = betalingsdato
                     , r21_egenrefoppdrag = egenrefoppdrag
                     , r21_mottaker_kontonr = mottaker_kontonr
                     , r21_mottaker_navn = mottaker_navn
                     , r21_adresse_1 = adresse_1
                     , r21_adresse_2 = adresse_2
                     , r21_postnr = postnr
                     , r21_poststed = poststed
                     , r21_belop_til_egen_konto = belop_til_egen_konto
                     , r21_tekstkode = tekstkode
                     , r21_transaksjonstype = transaksjonstype
                     , r21_slettekode = slettekode
                     , r21_totalbelop = totalbelop
                     , r21_klientreferanse = klientreferanse
                     , r21_valuteringsdato = valuteringsdato
                     , r21_valuteringmottat = valuteringmottat
                     , r21_slettearsak = slettearsak
                     , r21_blankettnummer = blankettnummer
                     }


parseSpecificRecord "BETFOR23" applikasjonsheader =
  do foretaksnummer <- parseForetaksnummer
     kontonummer    <- replicateM 11 digit
     sekvenskontrollfelt <- parseSekvenskontrollfelt
     referansenummer <- replicateM 6 anyChar
     melding_1 <- replicateM 40 anyChar
     melding_2 <- replicateM 40 anyChar
     melding_3 <- replicateM 40 anyChar
     kid <- replicateM 27 anyChar
     egenreffaktura <- replicateM 30 anyChar
     faktura_belop <- replicateM 15 digit
     debit_kredit_kode <- replicateM 1 anyChar
     fakturanummer <- replicateM 20 anyChar
     lopenummer <- replicateM 3 digit
     slettearsak <- replicateM 1 anyChar
     kundenummer <- replicateM 15 anyChar
     fakturadato <- replicateM 8 digit

     return BETFOR23 { r23_applikasjonsheader = applikasjonsheader
                     , r23_foretaksnummer = foretaksnummer
                     , r23_kontonummer = kontonummer
                     , r23_sekvenskontrollfelt = sekvenskontrollfelt
                     , r23_referansenummer = referansenummer
                     , r23_melding_1 = melding_1
                     , r23_melding_2 = melding_2
                     , r23_melding_3 = melding_3
                     , r23_kid = kid
                     , r23_egenreffaktura = egenreffaktura
                     , r23_faktura_belop = faktura_belop
                     , r23_debit_kredit_kode = debit_kredit_kode
                     , r23_fakturanummer = fakturanummer
                     , r23_lopenummer = lopenummer
                     , r23_slettearsak = slettearsak
                     , r23_kundenummer = kundenummer
                     , r23_fakturadato = fakturadato
                     }


parseSpecificRecord "BETFOR99" applikasjonsheader =
  do foretaksnummer <- parseForetaksnummer

     -- reservert
     replicateM_ 11 space

     sekvenskontrollfelt <- parseSekvenskontrollfelt
     -- reservert
     replicateM_ 6 space

     produksjonsdato <- parseProduksjonsdato

     antall_oppdrag <- fmap read $ replicateM 4 digit

     totalsum_fil <- fmap read $ replicateM 15 digit

     antall_records <- fmap read $ replicateM 5 digit

     -- reservert
     replicateM_ 163 space

     -- skip sigill
     replicateM_ 25 anyChar

     versjon_software <- replicateM 16 anyChar

     versjon_bank     <- replicateM 8  anyChar


     return BETFOR99 { r99_applikasjonsheader = applikasjonsheader
                     , r99_foretaksnummer = foretaksnummer
                     , r99_sekvenskontrollfelt = sekvenskontrollfelt
                     , r99_produksjonsdato = produksjonsdato
                     , r99_antall_oppdrag = antall_oppdrag
                     , r99_totalsum_fil = totalsum_fil
                     , r99_antall_records = antall_records
                     , r99_versjon_software = versjon_software
                     , r99_versjon_bank = versjon_bank
                     }



parseSpecificRecord _ _ = error "Unrecognized BETFOR format."

parseForetaksnummer :: Parser String
parseForetaksnummer = replicateM 11 digit

parseSekvenskontrollfelt :: Parser Integer
parseSekvenskontrollfelt = fmap read $ replicateM 4 digit

parseProduksjonsdato :: Parser String
parseProduksjonsdato = replicateM 4 digit

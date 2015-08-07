module HooplaTelepay.Model where


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

data Record = BETFOR00 { r00_applikasjonsheader  :: Applikasjonsheader
                       , r00_foretaksnummer      :: String
                       , r00_divisjon            :: String
                       , r00_sekvenskontrollfelt :: Integer
                       , r00_produksjonsdato     :: String
                       , r00_passord             :: String
                       , r00_nytt_passord        :: String
                       , r00_operatornummer      :: String
                       , r00_egenreferanse       :: String
                       }

            | BETFOR21 { r21_applikasjonsheader   :: Applikasjonsheader
                       , r21_foretaksnummer       :: String
                       , r21_kontonummer          :: String
                       , r21_sekvenskontrollfelt  :: Integer
                       , r21_referansenummer      :: String
                       , r21_betalingsdato        :: String
                       , r21_egenrefoppdrag       :: String
                       , r21_mottaker_kontonr     :: String
                       , r21_mottaker_navn        :: String
                       , r21_adresse_1            :: String
                       , r21_adresse_2            :: String
                       , r21_postnr               :: String
                       , r21_poststed             :: String
                       , r21_belop_til_egen_konto :: String
                       , r21_tekstkode            :: String
                       , r21_transaksjonstype     :: String
                       , r21_slettekode           :: String
                       , r21_totalbelop           :: Integer
                       , r21_klientreferanse      :: String
                       , r21_valuteringsdato      :: String
                       , r21_valuteringmottat     :: String
                       , r21_slettearsak          :: String
                       , r21_blankettnummer       :: String
                       }

            | BETFOR23 { r23_applikasjonsheader  :: Applikasjonsheader
                       , r23_foretaksnummer      :: String
                       , r23_kontonummer         :: String
                       , r23_sekvenskontrollfelt :: Integer
                       , r23_referansenummer     :: String
                       , r23_melding_1           :: String
                       , r23_melding_2           :: String
                       , r23_melding_3           :: String
                       , r23_kid                 :: String
                       , r23_egenreffaktura      :: String
                       , r23_faktura_belop       :: String
                       , r23_debit_kredit_kode   :: String
                       , r23_fakturanummer       :: String
                       , r23_lopenummer          :: String
                       , r23_slettearsak         :: String
                       , r23_kundenummer         :: String
                       , r23_fakturadato         :: String
                       }

            | BETFOR99 { r99_applikasjonsheader  :: Applikasjonsheader
                       , r99_foretaksnummer      :: String
                       , r99_sekvenskontrollfelt :: Integer
                       , r99_produksjonsdato     :: String
                       , r99_antall_oppdrag      :: Integer
                       , r99_totalsum_fil        :: Integer
                       , r99_antall_records      :: Integer
                       , r99_versjon_software    :: String
                       , r99_versjon_bank        :: String
                       }

            deriving Show


data Telepay =
  Telepay { tRecords :: [Record] }
  deriving Show

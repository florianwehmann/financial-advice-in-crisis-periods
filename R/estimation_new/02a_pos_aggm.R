
library(duckdb)
library(glue)
library(data.table)
library(arrow)

rm(list=ls()); gc()

con <- dbConnect(duckdb(), config = list(
  threads = as.character(parallel::detectCores()),
  preserve_insertion_order = "false"   # allows parallel result assembly
))

path <- "../../data/pos_merged.parquet"

dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM read_parquet('%s')", path))
dbGetQuery(con, sprintf("SELECT DISTINCT Instrumentengruppe FROM read_parquet('%s')", path))

# dbGetQuery(con, sprintf("SELECT DISTINCT Instrumentengruppe FROM read_parquet('%s')", path))
# # dbGetQuery(con, sprintf("SELECT DISTINCT KontoproduktGL1
# #                         FROM read_parquet('%s')
# #                         where Instrumentengruppe IN ('Kredit')", path))
# # 
# 
# 
# 
# # Kontoprodukte
# kt <- dbGetQuery(con, sprintf("SELECT DISTINCT KontoproduktGL2, KontoproduktGL1, Kontoprodukt, BilanzpositionGL2, Instrumentengruppe,
#                         SUM(Kundeneinlagen_CHF)/10^9 as dep
#                         FROM read_parquet('%s')
#                         GROUP BY KontoproduktGL2, KontoproduktGL1, Kontoprodukt, BilanzpositionGL2, Instrumentengruppe
#                         ORDER BY KontoproduktGL2, KontoproduktGL1, Kontoprodukt", path))
# setDT(kt)
# # Bilanzpositionen
# dbGetQuery(con, sprintf("SELECT DISTINCT BilanzpositionGL2, BilanzpositionGL1, Bilanzposition
#                         FROM read_parquet('%s')
#                         ORDER BY BilanzpositionGL2, BilanzpositionGL1, Bilanzposition", path))
# 
# 
# dt <- dbGetQuery(con, sprintf("SELECT DISTINCT KontoproduktGL2, BilanzpositionGL2, KontoproduktGL1, BilanzpositionGL1, Kontoprodukt, Bilanzposition, Instrumentengruppe
#                         FROM read_parquet('%s')
#                         ORDER BY KontoproduktGL2, BilanzpositionGL2, KontoproduktGL1, BilanzpositionGL1, Kontoprodukt, Bilanzposition", path))
# setDT(dt)
# fwrite(dt,"kontoprod_bilanzpos.csv")
# 
# # 
# # dt <- dbGetQuery(con, sprintf("SELECT *
# #                         FROM read_parquet('%s')
# #                         where Instrumentengruppe IN ('Kredit')", path))
# # 
# dbGetQuery(con, sprintf("SELECT DISTINCT Instrumentengruppe, KontoproduktGL2, KontoproduktGL1, Kontoprodukt
#                         FROM read_parquet('%s')
#                         where Bilanzposition IN ('Verpflichtungen Kunden Anlageform')", path))
# 
# 
# 
# pos_cols <- c("Bp_ID","Person_ID","Cont_ID","Period_ID","Hauptbetreuer_ID","Nebenbetreuer_ID",
#               "Kontoprodukt","KontoproduktGL1","BilanzpositionGL1",
#               "Depotprodukt","Instrumentengruppe","Fondsart","StruktProd_Klasse",
#               "Asset_ID","Asset","Asset_Waehrung","Referenzwaehrung","Positionswaehrung",
#               "Menge","Vermoegen_CHF","Geschaeftsvolumen_CHF","Hypothekarforderungen_CHF",
#               "Kundenausleihungen_CHF","Kundeneinlagen_CHF","Depotvol_Beratungsmandat_CHF",
#               "Depotvol_Verwaltungsmandat_C","Bestand_Treuhandanlagen_CHF","AUM_CHF",
#               "DA_Titelkursabweichung_CHF","DA_Devisenkursabweichung_CHF","DA_Mengenabweichung_CHF","DA_Delta_CHF","DA_Gesamtabweichung_CHF",
#               "Geburtsjahr","Hauptbankkunde","Geschlecht","Anlagepaket","Anlegerprofil",
#               "Beginn_Bankbeziehung","Ende_Bankbeziehung")
# 
# cols_sql <- paste(sprintf('"%s"', pos_cols), collapse = ", ")
# 
# 
# dt <- dbGetQuery(con, sprintf("SELECT %s
#                         FROM read_parquet('%s')
#                         where Hauptbankkunde == 'Ja'", cols_sql, path))
# 
# sql <- sprintf("SELECT %s
#                         FROM read_parquet('%s')
#                         where Hauptbankkunde == 'Ja'", cols_sql, path)
# 
# 
# 
# res <- dbSendQuery(con, sql, arrow = TRUE)
# dt  <- as.data.table(as.data.frame(duckdb::duckdb_fetch_arrow(res)))
# dbClearResult(res)
# 
# 
# 
# 
# con <- dbConnect(duckdb(), config = list(preserve_insertion_order = "false"))
# dbExecute(con, "CREATE VIEW pos AS SELECT * FROM read_parquet('../data/pos_merged.parquet')")
# 
# cols_sql <- paste(DBI::dbQuoteIdentifier(con, pos_cols), collapse = ", ")
# dt <- dbGetQuery(con, sprintf("SELECT %s FROM pos WHERE Hauptbankkunde = 'Ja'", cols_sql))
# setDT(dt)


## ============================================================================
## SQL Query

# SQL <- sprintf("
# SELECT Bp_ID, Cont_ID, Period_ID, Person_ID,
#        first(Hauptbetreuer_ID) as advisor_id,
#        first(Nebenbetreuer_ID) as sec_advisor_id,
#        first(Geburtsjahr) as birth_year,
#        first(Beginn_Bankbeziehung) as start_bank_rel,
#        first(Geschlecht) as sex,
#        first(Anlagepaket) as anlagepaket,
#        first(Anlegerprofil) as anlegerprofil,
#        
#        SUM(CASE WHEN Instrumentengruppe = 'Aktien'
#                   OR (Instrumentengruppe = 'Fonds' AND Fondsart IN
#                       ('Fund - Shares (09)','Fund - Exchange Traded (03)','Fund - Index (04)'))
#                 THEN Geschaeftsvolumen_CHF ELSE 0 END) AS equity,
#        SUM(CASE WHEN Instrumentengruppe = 'Aktien'
#                   OR (Instrumentengruppe = 'Fonds' AND Fondsart IN
#                       ('Fund - Shares (09)','Fund - Exchange Traded (03)','Fund - Index (04)'))
#                 THEN DA_Titelkursabweichung_CHF ELSE 0 END) AS dp_equity,
#        SUM(CASE WHEN Instrumentengruppe = 'Aktien'
#                   OR (Instrumentengruppe = 'Fonds' AND Fondsart IN
#                       ('Fund - Shares (09)','Fund - Exchange Traded (03)','Fund - Index (04)'))
#                 THEN DA_Mengenabweichung_CHF ELSE 0 END) AS dq_equity,
#                 
#        SUM(CASE WHEN Instrumentengruppe = 'Obligationen'
#                   OR (Instrumentengruppe = 'Fonds' AND Fondsart = 'Fund - Bond (12)')
#                 THEN Geschaeftsvolumen_CHF ELSE 0 END) AS bond,
#        SUM(CASE WHEN Instrumentengruppe = 'Obligationen'
#                   OR (Instrumentengruppe = 'Fonds' AND Fondsart = 'Fund - Bond (12)')
#                 THEN DA_Titelkursabweichung_CHF ELSE 0 END) AS dp_bond,
#        SUM(CASE WHEN Instrumentengruppe = 'Obligationen'
#                   OR (Instrumentengruppe = 'Fonds' AND Fondsart = 'Fund - Bond (12)')
#                 THEN DA_Mengenabweichung_CHF ELSE 0 END) AS dq_bond,
# 
#        SUM(CASE WHEN Instrumentengruppe = 'Fonds' AND Fondsart = 'Fund - Real Estate (01)'
#                 THEN Geschaeftsvolumen_CHF ELSE 0 END) AS reales,
# 
#        SUM(CASE WHEN Instrumentengruppe = 'Fonds'
#                  AND Fondsart NOT IN ('Fund - Shares (09)','Fund - Exchange Traded (03)',
#                                       'Fund - Index (04)','Fund - Bond (12)','Fund - Real Estate (01)')
#                 THEN Geschaeftsvolumen_CHF ELSE 0 END) AS fund_mixed,
# 
#        SUM(CASE WHEN Instrumentengruppe IN ('Strukturierte Prod./Zertifikate','Optionen',
#                                             'Warrants','Futures')
#                 THEN Geschaeftsvolumen_CHF ELSE 0 END) AS deriv,
# 
#        SUM(CASE WHEN Instrumentengruppe IN ('Metall','Kryptowaehrung','Kryptowährung',
#                                             'Ansprueche','Ansprüche','Anrechte',
#                                             'Waehrung','Währung')
#                 THEN Geschaeftsvolumen_CHF ELSE 0 END) AS alt,
#                 
#        SUM(CASE WHEN Instrumentengruppe NOT IN ('Bar','Money Market Deposit','Nicht zugeteilt (Dummy)')
#                 THEN Geschaeftsvolumen_CHF ELSE 0 END) AS tot_pf,
#        SUM(CASE WHEN Instrumentengruppe NOT IN ('Bar','Money Market Deposit','Nicht zugeteilt (Dummy)')
#                 THEN DA_Titelkursabweichung_CHF ELSE 0 END) AS dp_tot_pf,
#        SUM(CASE WHEN Instrumentengruppe NOT IN ('Bar','Money Market Deposit','Nicht zugeteilt (Dummy)')
#                 THEN DA_Devisenkursabweichung_CHF ELSE 0 END) AS dfx_tot_pf,
#        SUM(CASE WHEN Instrumentengruppe NOT IN ('Bar','Money Market Deposit','Nicht zugeteilt (Dummy)')
#                 THEN DA_Titelkursabweichung_CHF+DA_Devisenkursabweichung_CHF ELSE 0 END) AS dpfx_tot_pf,
#        SUM(CASE WHEN Instrumentengruppe NOT IN ('Bar','Money Market Deposit','Nicht zugeteilt (Dummy)')
#                 THEN DA_Mengenabweichung_CHF ELSE 0 END) AS dq_tot_pf,
#        SUM(CASE WHEN Instrumentengruppe NOT IN ('Bar','Money Market Deposit','Nicht zugeteilt (Dummy)')
#                 THEN Menge ELSE 0 END) AS dqsum_tot_pf,
#        SUM(CASE WHEN Instrumentengruppe NOT IN ('Bar','Money Market Deposit','Nicht zugeteilt (Dummy)') AND
#                 Menge > 0
#                 THEN Menge ELSE 0 END) AS buy_tot_pf,
#        SUM(CASE WHEN Instrumentengruppe NOT IN ('Bar','Money Market Deposit','Nicht zugeteilt (Dummy)') AND
#                 Menge < 0
#                 THEN Menge ELSE 0 END) AS sell_tot_pf,
#        
#        SUM(CASE WHEN Kontoprodukt = 'Edelmetallkonto' THEN Geschaeftsvolumen_CHF ELSE 0 END) as metal_acc,
#        SUM(CASE WHEN Kontoprodukt = 'Vermögensverwaltungskonto' THEN Geschaeftsvolumen_CHF ELSE 0 END) as vv_acc,
#        
#        
#        
#        SUM(CASE WHEN Instrumentengruppe IN ('Bar','Money Market Deposit') AND
#                   Kontoprodukt NOT IN ('Edelmetallkonto','Sparen 3-Konto','Mieterkautionssparkonto','Grabunterhaltskonto') AND
#                   Bilanzposition NOT IN ('Verpflichtungen Kunden 3. Säule') AND
#                   BilanzpositionGL2 NOT IN ('Forderungen gegenüber Kunden')
#                 THEN Geschaeftsvolumen_CHF ELSE 0 END) AS cash_liq,
#        SUM(CASE WHEN Instrumentengruppe IN ('Bar','Money Market Deposit') AND
#                   (
#                   Kontoprodukt IN ('Mieterkautionssparkonto','Grabunterhaltskonto') OR
#                   Bilanzposition IN ('Verpflichtungen Kunden 3. Säule')
#                   ) AND
#                   BilanzpositionGL2 NOT IN ('Forderungen gegenüber Kunden')
#                 THEN Geschaeftsvolumen_CHF ELSE 0 END) AS cash_locked,
#        
#        SUM(CASE WHEN BilanzpositionGL2 = 'Hypothekarforderungen' THEN Geschaeftsvolumen_CHF ELSE 0 END) as hypo,
#       
#       
#        SUM(Case WHEN Instrumentengruppe NOT IN ('Limite','Kredit') THEN Vermoegen_CHF ELSE 0 END) as tot_wealth,
#       
#       
#        SUM(Kundeneinlagen_CHF) as deposit_cash_check,
#        SUM(Kundenausleihungen_CHF) as credit_check,
#        SUM(Hypothekarforderungen_CHF) as hypo_check,
#        SUM(Depotvol_Beratungsmandat_CHF) as depotvol_advisor_mandate,
#        SUM(Depotvol_Verwaltungsmandat_C) as depotvol_disc_mandate
#       
#       
# FROM read_parquet('%s')
# WHERE Hauptbankkunde = 'Ja'
# GROUP BY Bp_ID, Cont_ID, Period_ID, Person_ID
# ", path)


# better structure:

SQL <- sprintf(r"(
WITH base AS (
    SELECT
        *,
        -- ---- asset class flags (non-exclusive, as in the original) ----
        (    Instrumentengruppe = 'Aktien'
          OR (Instrumentengruppe = 'Fonds' AND Fondsart IN
              ('Fund - Shares (09)','Fund - Exchange Traded (03)','Fund - Index (04)'))
        ) AS is_equity,

        (    Instrumentengruppe = 'Obligationen'
          OR (Instrumentengruppe = 'Fonds' AND Fondsart = 'Fund - Bond (12)')
        ) AS is_bond,

        (Instrumentengruppe = 'Fonds' AND Fondsart = 'Fund - Real Estate (01)')
        AS is_reales,

        (    Instrumentengruppe = 'Fonds'
         AND Fondsart NOT IN ('Fund - Shares (09)','Fund - Exchange Traded (03)',
                              'Fund - Index (04)','Fund - Bond (12)','Fund - Real Estate (01)')
        ) AS is_fund_mixed,

        (Instrumentengruppe IN ('Strukturierte Prod./Zertifikate','Optionen',
                                'Warrants','Futures'))
        AS is_deriv,

        (Instrumentengruppe IN ('Metall','Kryptowaehrung','Kryptowährung',
                                'Ansprueche','Ansprüche','Anrechte',
                                'Waehrung','Währung'))
        AS is_alt,

        -- ---- portfolio / cash / wealth scopes ----
        (Instrumentengruppe NOT IN ('Bar','Money Market Deposit','Nicht zugeteilt (Dummy)','Kredit','Limite'))
        AS is_pf,

        (    Instrumentengruppe IN ('Bar','Money Market Deposit')
         AND Kontoprodukt NOT IN ('Edelmetallkonto','Sparen 3-Konto',
                                  'Mieterkautionssparkonto','Grabunterhaltskonto')
         AND Bilanzposition   NOT IN ('Verpflichtungen Kunden 3. Säule')
         AND BilanzpositionGL2 NOT IN ('Forderungen gegenüber Kunden')
        ) AS is_cash_liq,

        (    Instrumentengruppe IN ('Bar','Money Market Deposit')
         AND (   Kontoprodukt   IN ('Mieterkautionssparkonto','Grabunterhaltskonto')
              OR Bilanzposition IN ('Verpflichtungen Kunden 3. Säule'))
         AND BilanzpositionGL2 NOT IN ('Forderungen gegenüber Kunden')
        ) AS is_cash_locked,

        (Instrumentengruppe NOT IN ('Limite','Kredit')) AS is_wealth,

        contains(Depotprodukt, 'Vermögensverwaltungsdepot') AS is_vv_depot
    FROM read_parquet('%s')
    WHERE Hauptbankkunde = 'Ja'
)

SELECT
    -- ================= keys =================
    Bp_ID, Period_ID, Person_ID,

    -- ================= static attributes =================
    first(Hauptbetreuer_ID)      AS advisor_id,
    first(Nebenbetreuer_ID)      AS sec_advisor_id,
    first(Geburtsjahr)           AS birth_year,
    first(Beginn_Bankbeziehung)  AS start_bank_rel,
    first(Geschlecht)            AS sex,
    first(Anlagepaket)           AS anlagepaket,
    first(Anlegerprofil)         AS anlegerprofil,

    -- ================= equity =================
    SUM(CASE WHEN is_equity THEN Geschaeftsvolumen_CHF      ELSE 0 END) AS equity,
    SUM(CASE WHEN is_equity THEN DA_Titelkursabweichung_CHF ELSE 0 END) AS dp_equity,
    SUM(CASE WHEN is_equity THEN DA_Mengenabweichung_CHF    ELSE 0 END) AS dq_equity,

    -- ================= bonds =================
    SUM(CASE WHEN is_bond   THEN Geschaeftsvolumen_CHF      ELSE 0 END) AS bond,
    SUM(CASE WHEN is_bond   THEN DA_Titelkursabweichung_CHF ELSE 0 END) AS dp_bond,
    SUM(CASE WHEN is_bond   THEN DA_Mengenabweichung_CHF    ELSE 0 END) AS dq_bond,

    -- ================= other asset classes =================
    SUM(CASE WHEN is_reales     THEN Geschaeftsvolumen_CHF ELSE 0 END) AS reales,
    SUM(CASE WHEN is_reales   THEN DA_Titelkursabweichung_CHF ELSE 0 END) AS dp_reales,
    SUM(CASE WHEN is_reales   THEN DA_Mengenabweichung_CHF    ELSE 0 END) AS dq_reales,
    SUM(CASE WHEN is_fund_mixed THEN Geschaeftsvolumen_CHF ELSE 0 END) AS fund_mixed,
    SUM(CASE WHEN is_fund_mixed THEN DA_Titelkursabweichung_CHF ELSE 0 END) AS dp_fund_mixed,
    SUM(CASE WHEN is_fund_mixed THEN DA_Mengenabweichung_CHF    ELSE 0 END) AS dq_fund_mixed,
    SUM(CASE WHEN is_deriv      THEN Geschaeftsvolumen_CHF ELSE 0 END) AS deriv,
    SUM(CASE WHEN is_deriv   THEN DA_Titelkursabweichung_CHF ELSE 0 END) AS dp_deriv,
    SUM(CASE WHEN is_deriv   THEN DA_Mengenabweichung_CHF    ELSE 0 END) AS dq_deriv,
    SUM(CASE WHEN is_alt        THEN Geschaeftsvolumen_CHF ELSE 0 END) AS alt,
    SUM(CASE WHEN is_alt   THEN DA_Titelkursabweichung_CHF ELSE 0 END) AS dp_alt,
    SUM(CASE WHEN is_alt   THEN DA_Mengenabweichung_CHF    ELSE 0 END) AS dq_alt,

    -- ================= total portfolio =================
    SUM(CASE WHEN is_pf THEN Geschaeftsvolumen_CHF         ELSE 0 END) AS tot_pf,
    SUM(CASE WHEN is_pf THEN DA_Titelkursabweichung_CHF    ELSE 0 END) AS dp_tot_pf,
    SUM(CASE WHEN is_pf THEN DA_Devisenkursabweichung_CHF  ELSE 0 END) AS dfx_tot_pf,
    SUM(CASE WHEN is_pf THEN DA_Titelkursabweichung_CHF
                           + DA_Devisenkursabweichung_CHF  ELSE 0 END) AS dpfx_tot_pf,
    SUM(CASE WHEN is_pf THEN DA_Mengenabweichung_CHF       ELSE 0 END) AS dq_tot_pf,

    SUM(CASE WHEN is_pf                 THEN Menge ELSE 0 END) AS dqsum_tot_pf,
    SUM(CASE WHEN is_pf AND Menge > 0   THEN Menge ELSE 0 END) AS buy_tot_pf,
    SUM(CASE WHEN is_pf AND Menge < 0   THEN Menge ELSE 0 END) AS sell_tot_pf,

    -- ================= account products =================
    SUM(CASE WHEN Kontoprodukt = 'Edelmetallkonto'
             THEN Geschaeftsvolumen_CHF ELSE 0 END) AS metal_acc,
    SUM(CASE WHEN Kontoprodukt = 'Vermögensverwaltungskonto'
             THEN Geschaeftsvolumen_CHF ELSE 0 END) AS vv_acc,

    -- ================= cash =================
    SUM(CASE WHEN is_cash_liq    THEN Geschaeftsvolumen_CHF ELSE 0 END) AS cash_liq,
    SUM(CASE WHEN is_cash_locked THEN Geschaeftsvolumen_CHF ELSE 0 END) AS cash_locked,

    -- ================= lending & wealth =================
    SUM(CASE WHEN BilanzpositionGL2 = 'Hypothekarforderungen'
             THEN Geschaeftsvolumen_CHF ELSE 0 END) AS hypo,
    SUM(CASE WHEN is_wealth THEN Vermoegen_CHF ELSE 0 END) AS tot_wealth,

    -- ================= cross-checks against bank aggregates =================
    SUM(Kundeneinlagen_CHF)              AS deposit_cash_check,
    SUM(Kundenausleihungen_CHF)          AS credit_check,
    SUM(Hypothekarforderungen_CHF)       AS hypo_check,
    SUM(Depotvol_Beratungsmandat_CHF)    AS depotvol_advisor_mandate,
    SUM(Depotvol_Verwaltungsmandat_C)    AS depotvol_disc_mandate,

    -- ================= mandate flag =================
    MAX(CASE WHEN is_vv_depot THEN 1 ELSE 0 END) AS vv_depot

FROM base
GROUP BY Bp_ID, Period_ID, Person_ID
)", path)


dt <- dbGetQuery(con, SQL)
setDT(dt)
setorder(dt,Bp_ID,Period_ID)

write_parquet(dt,paste0("../../data/pos_aggm_sql.parquet"))

## =============================================================================
## 02b_contacts.R -- the contact log (data/contacts.parquet)
##
## This replaces the advice measure used before. Until this file arrived, advice
## was only visible when a TRADE followed a contact within five days, so 117'139
## client-months with an investment or performance contact and no trade were
## coded as untreated -- 44% of all contact-months, and the mismeasurement was
## correlated with the outcome (trading).
##
## Two objects are written:
##   contacts_d.parquet : contact-day level, kept for exact within-episode timing
##                        (the Covid drawdown is 23 trading days -- a monthly
##                        indicator cannot separate a call before the trough from
##                        one after it)
##   contacts_m.parquet : client x month indicators, merged into the panel by 02
##
## Topics kept (per the project's judgement):
##   K_Performancebesprechung -- portfolio review meeting
##   K_Anlegen                -- investment topic
## Initiator: init_by_advisor / init_by_client (K_Aufnahme).
## Channel  : K_meeting / K_phone / K_mail. Mail is excluded from every treatment
##            definition (bulk mailings cannot be told apart from personal ones).
## =============================================================================

source("00_setup.R")
log_init("02b_contacts")

CONTACT_FILE <- file.path(DATA, "contacts.parquet")
if (!file.exists(CONTACT_FILE)) stop("not found: ", CONTACT_FILE, call. = FALSE)

NEED <- c("Bp_ID", "MDate", "ContactDate", "K_Anlegen", "K_Performancebesprechung",
          "init_by_advisor", "init_by_client", "K_phone", "K_meeting", "K_mail")
ct <- setDT(read_parquet(CONTACT_FILE, col_select = all_of(NEED)))
require_cols(ct, NEED, "02b_contacts")
log_step("raw contacts", ct)

## MDate in the source is the reporting period (Period_ID); for a small number of
## rows it disagrees with the day the contact actually happened. The economically
## relevant month is the month of the contact.
ct <- ct[!is.na(ContactDate)]
ct[, MDate := eom(ContactDate)]
ct <- ct[MDate >= P$smp_start & MDate <= P$smp_end]
log_step(sprintf("window %s..%s", P$smp_start, P$smp_end), ct)

## the topic flags are only populated from 2011 (see 01_audit); rows before that
## carry a contact but no topic, which would look like "contacted, not about
## investments". P$smp_start already excludes them.
for (v in c("K_Anlegen","K_Performancebesprechung","init_by_advisor","init_by_client",
            "K_phone","K_meeting","K_mail"))
  ct[is.na(get(v)), (v) := 0]

## ---------------------------------------------------------------------------
## DROP THE BULK MAILINGS (P$ev_bulk_mail_months, see 00_setup)
##
## A mass mailing is recorded exactly like outreach: one advisor-initiated
## K_Anlegen contact per client reached. December 2014 is one -- 26080 mail
## contacts in a month that normally carries ~1500 -- and it lands on the
## reference month of the 2015-01 block, where it takes the advisor-initiated
## contact rate to 0.899 against ~0.05 elsewhere. It also inflates contacts_pre
## and adv_a_pre, which are controls in 06 and 07.
##
## Only the MAIL rows in those months go. The meetings and phone calls that
## month are real advisory contact and are kept, so the month is corrected
## rather than deleted.
## ---------------------------------------------------------------------------
if (length(P$ev_bulk_mail_months)) {
  bulk <- ct[eom(ContactDate) %in% P$ev_bulk_mail_months &
               K_mail > 0 & K_meeting == 0 & K_phone == 0, which = TRUE]
  log_step(sprintf("bulk-mail months %s: dropped %s mail-only contacts (kept %s personal)",
                   paste(format(P$ev_bulk_mail_months), collapse = ", "),
                   format(length(bulk), big.mark = "'"),
                   format(ct[eom(ContactDate) %in% P$ev_bulk_mail_months &
                               (K_meeting > 0 | K_phone > 0), .N], big.mark = "'")))
  if (length(bulk)) ct <- ct[-bulk]
  log_step("contacts after removing bulk mail", ct)
}

## ---------------------------------------------------------------------------
## contact-day level
## ---------------------------------------------------------------------------
ct[, `:=`(
  perf     = as.integer(K_Performancebesprechung > 0),
  inv      = as.integer(K_Anlegen > 0),
  init_a   = as.integer(init_by_advisor  > 0),
  init_c   = as.integer(init_by_client   > 0),
  personal = as.integer(K_meeting > 0 | K_phone > 0)     # excludes mail / fax
)]
ct[, `:=`(
  perf_a   = as.integer(perf == 1L & init_a == 1L),
  perf_p   = as.integer(perf == 1L & personal == 1L),
  perf_a_p = as.integer(perf == 1L & init_a == 1L & personal == 1L),
  inv_a    = as.integer(inv  == 1L & init_a == 1L),
  inv_a_p  = as.integer(inv == 1L & init_a==1L & personal == 1L),
  inv_c    = as.integer(inv  == 1L & init_c == 1L),
  inv_c_p   = as.integer(inv  == 1L & init_c == 1L & personal == 1L),
  advice   = as.integer(perf == 1L | inv == 1L),
  perf_inv = as.integer(perf == 1L & inv == 1L),
  perf_inv_a = as.integer(perf == 1L & inv == 1L & init_a == 1L),
  perf_inv_a_p = as.integer(perf == 1L & inv == 1L & init_a == 1L & personal == 1L)
)]
ct[, advice_a := as.integer(advice == 1L & init_a == 1L)]
ct[, advice_c := as.integer(advice == 1L & init_c == 1L)]
ct[, advice_a_p := as.integer(advice_a == 1L & personal == 1L)]
ct[, advice_c_p := as.integer(advice_c == 1L & personal == 1L)]

cd <- ct[, .(Bp_ID, ContactDate, MDate, perf, perf_a, perf_p, perf_a_p,
             inv, inv_a, inv_a_p, inv_c, advice, advice_a, advice_c, advice_a_p, advice_c_p, perf_inv, perf_inv_a, init_a, init_c, perf_inv_a_p,
             personal, K_meeting, K_phone, K_mail)]
save_dt(cd, "contacts_d")
log_step("contact-day file", cd)

## ---------------------------------------------------------------------------
## client x month indicators
## ---------------------------------------------------------------------------
FLAGS <- c("perf","perf_a","perf_p","perf_a_p","inv","inv_a", "inv_a_p","inv_c",
           "advice","advice_a","advice_c", "advice_a_p", "advice_c_p", "perf_inv", "perf_inv_a")

cm <- ct[, c(
  lapply(.SD, function(x) as.integer(any(x == 1L))),
  .(n_contacts   = .N,
    n_perf       = sum(perf),
    n_inv        = sum(inv),
    first_advice = min(ContactDate[advice == 1L], na.rm = TRUE),
    first_inv    = min(ContactDate[inv == 1L], na.rm=T),
    first_inv_a_p= min(ContactDate[inv_a_p == 1L],na.rm=T),
    first_perf   = min(ContactDate[perf   == 1L], na.rm = TRUE),
    first_perf_a_p = min(ContactDate[perf_a_p == 1L], na.rm= T),
    first_inv_c = min(ContactDate[inv_c == 1L]),
    first_inv_c_p = min(ContactDate[inv_c_p == 1L]))
), by = .(Bp_ID, MDate), .SDcols = FLAGS]
setnames(cm, FLAGS, paste0("c_", FLAGS))
for (v in c("first_advice","first_perf"))
  cm[is.infinite(get(v)), (v) := NA]
cm[, `:=`(first_advice = as.Date(first_advice), first_perf = as.Date(first_perf))]

save_dt(cm, "contacts_m")
log_step("client-month contact file", cm)

cat("\n-- contact coverage by year --\n")
print(ct[, .(contacts = .N, clients = uniqueN(Bp_ID),
             perf = sum(perf), perf_advisor_init = sum(perf_a),
             inv = sum(inv), inv_client_init = sum(inv_c)),
         by = .(year = year(ContactDate))][order(year)])

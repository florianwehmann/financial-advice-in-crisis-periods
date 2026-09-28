## =============================================================================
## itt_funs.R -- the habitual-window machinery, shared by 12 and 14
##
## Sourced, never run on its own. Defines functions and nothing else, so the
## window built for the main estimates and the window rebuilt inside the
## robustness grid and the placebo can never drift apart.
## =============================================================================

## A[m, s] = 1 when calendar month m lies in the arc of width w starting at s.
## Months are CIRCULAR: the arc starting in December is Dec-Jan-Feb.
arc_mat <- function(w) {
  A <- matrix(0L, 12L, 12L)
  for (s in 1:12) A[((s - 1L + 0:(w - 1L)) %% 12L) + 1L, s] <- 1L
  A
}

## month-end dates from a to b inclusive.
## NOT seq(a, b, by = "month"): from a month-end that rolls over, because R
## pushes an invalid date forward (2015-08-31 + 1 month = 2015-10-01), which
## silently skips September. madd() snaps to month end and is safe.
mseq <- function(a, b) {
  k <- mdiff(b, a)
  if (is.na(k) || k < 0L) return(as.Date(character(0)))
  madd(a, 0:k)
}

## calendar months touched by the date span [a, b]
months_between <- function(a, b) unique(month(mseq(a, b)))

## Leave-one-episode-out habitual window.
##   Q  : qualifying meetings, columns Bp_ID, ContactDate, mo, yr
##   ep : episode table with ep_id and a start column (default dd_start)
## Only meetings dated strictly before start - leave_out months may enter, and
## that is asserted rather than assumed -- it is the one property the whole
## design rests on.
build_window <- function(Q, ep, arc_w, leave_out, start_col = "dd_start") {
  A <- arc_mat(arc_w)
  rbindlist(lapply(seq_len(nrow(ep)), function(i) {
    st     <- ep[[start_col]][i]
    cutoff <- st %m-% months(leave_out)
    q <- Q[ContactDate < cutoff]
    if (!nrow(q)) return(NULL)

    ## ---- GUARDRAIL ---------------------------------------------------------
    if (max(q$ContactDate) >= cutoff)
      stop("leave-out violated: a meeting at or after the cutoff entered the pool")
    if (max(q$ContactDate) >= st)
      stop("leave-out violated: a meeting inside the episode window entered the pool")

    cnt <- dcast(q[, .N, by = .(Bp_ID, mo)], Bp_ID ~ mo, value.var = "N", fill = 0)
    ids <- cnt$Bp_ID
    M   <- as.matrix(cnt[, -1L, with = FALSE])
    full <- matrix(0L, nrow(M), 12L)
    full[, as.integer(colnames(M))] <- M

    ## share of this client's pre-episode meetings falling in each arc
    sh <- (full %*% A) / rowSums(full)

    ## ties broken toward the arc holding the most recent qualifying meeting
    setorder(q, Bp_ID, ContactDate)
    last_mo <- q[, .(lm = last(mo)), by = Bp_ID][match(ids, Bp_ID), lm]
    best    <- max.col(sh + 1e-9 * A[last_mo, , drop = FALSE], ties.method = "first")

    ny <- q[, .(n_years_pre = uniqueN(yr), last_pre = last(ContactDate)), by = Bp_ID]

    data.table(Bp_ID = ids, ep_id = ep$ep_id[i],
               arc_start = best, arc_w = arc_w,
               regularity  = sh[cbind(seq_along(best), best)],
               n_meet_pre  = as.integer(rowSums(full)),
               n_years_pre = ny[match(ids, Bp_ID), n_years_pre],
               last_pre    = ny[match(ids, Bp_ID), last_pre],
               cutoff      = cutoff)
  }))
}

## Z = 1 when the habitual arc overlaps [from, to] by at least one calendar month
add_Z <- function(W, ep, from = "dd_start", to = "dd_end", name = "Z", arc_w = 3L) {
  A  <- arc_mat(arc_w)
  cm <- lapply(seq_len(nrow(ep)), function(i) months_between(ep[[from]][i], ep[[to]][i]))
  names(cm) <- ep$ep_id
  W[, (name) := as.integer(mapply(function(s, e) any(A[cm[[e]], s] == 1L),
                                  arc_start, ep_id))]
  W
}

## =============================================================================
## 09_export.R -- publish the finished tables and figures to the paper folder
##
## Copies results/tables/*.tex and results/figures/*.pdf into EXPORT_DIR
## (set in 00_setup.R, overridable with CRISIS_EXPORT_DIR), preserving the
## tables/ and figures/ split so the LaTeX paths are stable:
##
##     \input{estimation/tables/06d_cross_section_main.tex}
##     \includegraphics{estimation/figures/06a_es_psell_self.pdf}
##
## Everything in the destination is REPLACED, and files that no longer exist in
## results/ are deleted from the destination, so the paper folder is always an
## exact mirror of the last run -- a table that was renamed cannot linger and be
## silently \input by the paper.
##
## This step NEVER stops the pipeline. If the folder is missing (another
## machine, Dropbox not mounted) or a file is locked, it says so and carries on:
## the analysis is already written to results/ and that is the source of truth.
##
## Output: EXPORT_DIR/{tables,figures}/, EXPORT_DIR/_manifest.txt
## =============================================================================

source("00_setup.R")
log_init("09_export")

sink(file.path(RESULTS, "09_export.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

cat("=============================================================\n")
cat(" EXPORT\n")
cat(" to: ", EXPORT_DIR, "\n", sep = "")
cat("=============================================================\n\n")

if (!dir.exists(EXPORT_DIR)) {
  ok <- dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)
  if (!ok) {
    cat("!! export folder does not exist and could not be created:\n   ",
        EXPORT_DIR, "\n", sep = "")
    cat("   Nothing was copied. results/ is unaffected and is the source of truth.\n")
    cat("   Set CRISIS_EXPORT_DIR to publish somewhere else.\n")
    log_step("export SKIPPED -- destination unavailable")
    sink()
    ## stop the script, not the pipeline
    if (!interactive()) invisible(NULL)
  } else {
    cat("created ", EXPORT_DIR, "\n\n", sep = "")
  }
}

if (dir.exists(EXPORT_DIR)) {

  copy_group <- function(sub, pattern) {
    src <- file.path(RESULTS, sub)
    dst <- file.path(EXPORT_DIR, sub)
    dir.create(dst, recursive = TRUE, showWarnings = FALSE)

    have <- list.files(src, pattern = pattern)
    ## drop anything in the destination that the pipeline no longer produces
    stale <- setdiff(list.files(dst, pattern = pattern), have)
    if (length(stale)) {
      file.remove(file.path(dst, stale))
      cat("  removed ", length(stale), " stale file(s): ",
          paste(stale, collapse = ", "), "\n", sep = "")
    }

    okv <- vapply(have, function(f)
      isTRUE(tryCatch(file.copy(file.path(src, f), file.path(dst, f),
                                overwrite = TRUE, copy.date = TRUE),
                      error = function(e) FALSE, warning = function(e) FALSE)),
      logical(1))

    cat(sprintf("  %-8s %d of %d copied\n", sub, sum(okv), length(okv)))
    if (any(!okv)) {
      cat("  !! FAILED (locked or unreadable): ",
          paste(names(okv)[!okv], collapse = ", "), "\n", sep = "")
    }
    data.table(group = sub, file = have, ok = as.logical(okv),
               bytes = file.size(file.path(src, have)))
  }

  man <- rbindlist(list(copy_group("tables",  "\\.tex$"),
                        copy_group("figures", "\\.pdf$")))

  cat("\n-- manifest --\n")
  print(man[, .(file = paste0(group, "/", file),
                kb = round(bytes / 1024, 1),
                copied = ok)], row.names = FALSE)

  writeLines(c(
    "Tables and figures for 'Uncertainty and Financial Advice'.",
    paste0("Written by R/estimation/09_export.R on ",
           format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "."),
    "",
    "DO NOT EDIT THESE FILES BY HAND -- the next pipeline run overwrites them,",
    "and any file the pipeline stops producing is deleted from this folder.",
    "Regenerate with:  setwd(\"R/estimation\"); source(\"run_all.R\")",
    "",
    sprintf("%-46s %10s", "file", "bytes"),
    sprintf("%-46s %10.0f", paste0(man$group, "/", man$file), man$bytes)
  ), file.path(EXPORT_DIR, "_manifest.txt"))

  cat(sprintf("\n%d file(s) published to %s\n", sum(man$ok), EXPORT_DIR))
  if (any(!man$ok))
    cat("!! ", sum(!man$ok), " file(s) could not be copied -- see above.\n", sep = "")
  log_step(sprintf("exported %d files to %s", sum(man$ok), EXPORT_DIR))
}

sink()

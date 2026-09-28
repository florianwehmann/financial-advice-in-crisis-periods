# =============================================================================
# decomp_fig.R -- shared rendering of the decomposition bar charts, so that the
# crisis figure (07_estim_v3.R) and the placebo figure (07_estim_v3_placebo.R)
# sit on IDENTICAL y-axes.
#
# Each script stores the coefficients of its decomposition under
# results/decomp/<group>__<variant>.rds and then calls decomp_render(group):
# that redraws EVERY stored variant of the group on the common limits, so
# whichever script runs second also fixes the figure of the one that ran first.
#
# group   : "wealth" | "pf"   (one y-axis per group)
# variant : "crisis" | "placebo"
#
# Sourced, never run on its own. Expects data.table and ggplot2 to be loaded.
# =============================================================================
DECOMP_DIR <- "../../results/decomp"
dir.create(DECOMP_DIR, showWarnings = FALSE, recursive = TRUE)

# meta: fig_dir, file, title, subtitle, ylab, fills, xbreaks, ref
decomp_save <- function(group, variant, comp, tot, meta) {
  saveRDS(list(comp = copy(comp), tot = copy(tot), meta = meta, stamp = Sys.time()),
          file.path(DECOMP_DIR, sprintf("%s__%s.rds", group, variant)))
}

# Limits must hold the stacked bars (positive and negative parts separately)
# and the CI of the total, over every stored variant.
decomp_ylim <- function(objs, pad = 0.04) {
  v <- unlist(lapply(objs, function(o) {
    s <- o$comp[, .(lo = sum(pmin(est, 0)), hi = sum(pmax(est, 0))), by = rel_month]
    c(s$lo, s$hi, o$tot$est - 1.96 * o$tot$se, o$tot$est + 1.96 * o$tot$se)
  }))
  r <- range(v, na.rm = TRUE)
  r + c(-1, 1) * pad * diff(r)
}

decomp_plot <- function(o, ylim) {
  ggplot() +
    geom_col(data = o$comp, aes(rel_month, est, fill = outcome), width = 0.75, alpha = 0.85) +
    geom_errorbar(data = o$tot, aes(rel_month, ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                  width = 0.25, linewidth = 0.4) +
    geom_line(data = o$tot, aes(rel_month, est), linewidth = 0.3) +
    geom_point(data = o$tot, aes(rel_month, est), size = 1.8) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_vline(xintercept = o$meta$ref + 0.5, linetype = "dashed", linewidth = 0.4) +
    scale_fill_manual(values = o$meta$fills) +
    scale_x_continuous(breaks = o$meta$xbreaks) +
    coord_cartesian(ylim = ylim) +          # shared scale, no data dropped
    labs(x = "Months relative to pre-period month", y = o$meta$ylab, fill = NULL,
         title = o$meta$title, subtitle = o$meta$subtitle) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}

# Redraws every stored variant of `group`; returns the plot of `current`.
decomp_render <- function(group, current = NULL, width = 8, height = 5, quiet = FALSE) {
  files <- list.files(DECOMP_DIR, pattern = sprintf("^%s__.*\\.rds$", group), full.names = TRUE)
  objs  <- setNames(lapply(files, readRDS), sub("\\.rds$", "", sub(".*__", "", basename(files))))
  yl    <- decomp_ylim(objs)
  yl <- c(NULL,NULL)
  out   <- NULL
  for (nm in names(objs)) {
    p <- decomp_plot(objs[[nm]], yl)
    f <- file.path(objs[[nm]]$meta$fig_dir, objs[[nm]]$meta$file)
    ggsave(f, p, width = width, height = height)
    if (!quiet) cat(sprintf("  %-8s ylim [%.3f, %.3f] -> %s\n", nm, yl[1], yl[2], f))
    if (!is.null(current) && nm == current) out <- p
  }
  invisible(out)
}

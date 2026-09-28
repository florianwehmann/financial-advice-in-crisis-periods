# ==========================================================================
# SMI vs. VIX with annotated stress episodes
# ==========================================================================
library(data.table)
library(ggplot2)

# ---- 1. Data --------------------------------------------------------------
YEARS <- c(2011, 2024)

smi_sub <- as.data.table(smi)[year(Date) %between% YEARS]
vix_sub <- as.data.table(vix)[year(index) %between% YEARS]   # <- was NOT filtered before

# VIX is drawn on the SMI scale:  vix_scaled = VIX * SCALE + SHIFT
SCALE <- 100
SHIFT <- 4000
vix_sub[, vix_scaled := VIX.Close * SCALE + SHIFT]

# ---- 2. Events ------------------------------------------------------------
# The windows, labels and label geometry all come from stress_events.R, which
# 03_episodes.R also reads. Edit the dates THERE and the figure and the
# estimation episodes move together.
#   row   = stacking level of the label (1 = closest to the panel)
#   hjust = 0.5 normally, 0 / 1 for labels near the plot edges
source("stress_events.R")
events <- copy(EVENTS)

events[, mid := start + (end - start) / 2]

# ---- 3. Label geometry ----------------------------------------------------
y_lo <- min(smi_sub$smi, vix_sub$vix_scaled, na.rm = TRUE)
y_hi <- max(smi_sub$smi, vix_sub$vix_scaled, na.rm = TRUE)
pad  <- 0.03 * (y_hi - y_lo)
y_lo <- y_lo - pad
y_hi <- y_hi + pad

STEP <- 0.085 * (y_hi - y_lo)          # vertical distance between label rows
events[, y_lab := y_hi + row * STEP]

# ---- 4. Plot --------------------------------------------------------------
p <- ggplot() +
  # shaded episodes first, so the series are drawn on top
  geom_rect(data = events,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "grey60", alpha = 0.3) +
  # leader lines from the panel top up to the labels
  geom_segment(data = events,
               aes(x = mid, xend = mid, y = y_hi, yend = y_lab),
               linewidth = 0.3, colour = "grey30") +
  geom_text(data = events,
            aes(x = mid, y = y_lab, label = label, hjust = hjust),
            vjust = 0, size = 2.9, lineheight = 0.95) +
  # series
  geom_line(data = vix_sub, aes(index, vix_scaled, colour = "VIX"),
            linewidth = 0.8, alpha = 0.5) +
  geom_line(data = smi_sub, aes(Date, smi, colour = "SMI"),
            linewidth = 0.4) +
  scale_colour_manual(values = c(SMI = "black", VIX = "salmon")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    name     = "SMI",
    sec.axis = sec_axis(~ (. - SHIFT) / SCALE, name = "VIX")
  ) +
  coord_cartesian(ylim = c(y_lo, y_hi), clip = "off") +
  theme_light() +
  labs(x = NULL) +
  guides(colour = guide_legend(title = NULL)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    # top margin must be big enough to hold the label block
    plot.margin = margin(t = 230, r = 15, b = 5, l = 5)
  )

ggsave("smi_vix.png", p, width = 20, height = 11, dpi = 200)
p
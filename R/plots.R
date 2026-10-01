# Chart builders. Each takes a query result and returns a ggplot built from ggiraph's
# interactive geoms; app.R renders it with girafe() so every mark has a hover tooltip.
# The functions are pure, so the tests exercise them without running the app.
#
# Colours are the dataviz reference palette, checked with its validator: categorical
# slots 1-2 for two-series charts (CVD separation 24.7, normal vision 33.6), and a
# five-step single-hue ramp for the ordinal site buckets. Six steps failed the
# adjacent-lightness check, which is why there are five buckets.

library(ggplot2)
library(ggiraph)

INK <- list(
  surface = "#fcfcfb", primary = "#0b0b0b", secondary = "#52514e",
  muted = "#898781", grid = "#e1e0d9", baseline = "#c3c2b7"
)
SERIES <- c("#2a78d6", "#eb6834")
ORDINAL_5 <- c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281")

# Colour follows the entity, so deselecting one area never repaints the other.
AREA_COLOURS <- c(oncology = SERIES[[1]], cardiovascular = SERIES[[2]])
OUTCOME_COLOURS <- c(Completed = SERIES[[1]], Terminated = SERIES[[2]])

# A rate over fewer finished trials than this is too noisy to show as a bar.
MIN_FINAL_FOR_RATE <- 20

theme_tfe <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      plot.background = element_rect(fill = INK$surface, colour = NA),
      panel.background = element_rect(fill = INK$surface, colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = INK$grid, linewidth = 0.3),
      axis.line.y = element_line(colour = INK$baseline, linewidth = 0.3),
      axis.text.x = element_text(colour = INK$muted),
      axis.text.y = element_text(colour = INK$secondary),
      axis.title.x = element_text(colour = INK$secondary, size = rel(0.85), margin = margin(t = 8)),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(colour = INK$secondary, size = rel(0.85)),
      legend.text = element_text(colour = INK$secondary, size = rel(0.85)),
      legend.key.size = unit(9, "pt"),
      plot.margin = margin(4, 14, 4, 4)
    )
}

# Phases top to bottom in PHASE_LEVELS order, keeping only those present.
phase_axis <- function(phase) factor(phase, levels = rev(intersect(PHASE_LEVELS, unique(phase))))

year_breaks <- function(limits) {
  step <- if (diff(limits) > 96) 24 else 12
  seq(0, ceiling(limits[[2]] / step) * step, by = step)
}

plot_enrollment <- function(d) {
  d$phase <- phase_axis(d$phase)
  d$tip <- sprintf(
    "<b>%s</b><br>Median %s participants<br>Middle half of trials: %s–%s<br>%s trials",
    d$phase, fmt_int(d$median), fmt_int(d$p25), fmt_int(d$p75), fmt_int(d$n_trials)
  )
  ggplot(d, aes(y = phase)) +
    geom_segment_interactive(
      aes(x = p25, xend = p75, yend = phase, tooltip = tip, data_id = phase),
      colour = SERIES[[1]], alpha = 0.3, linewidth = 2.4, lineend = "round"
    ) +
    geom_point_interactive(aes(x = median, tooltip = tip, data_id = phase),
                           colour = SERIES[[1]], size = 2.6) +
    geom_text(aes(x = median, label = fmt_int(median)),
              nudge_y = 0.33, size = 2.9, colour = INK$secondary) +
    scale_x_log10(labels = scales::label_comma()) +
    labs(x = "Participants enrolled, log scale (dot: median, bar: middle half of trials)", y = NULL) +
    theme_tfe()
}

plot_sites <- function(d) {
  d$phase <- phase_axis(d$phase)
  d$sites_bucket <- factor(d$sites_bucket, levels = SITE_BUCKETS)
  d$tip <- sprintf(
    "<b>%s</b><br>%s site%s: %s of trials (%s)",
    d$phase, d$sites_bucket, ifelse(d$sites_bucket == "1", "", "s"),
    fmt_pct(d$share_of_phase), fmt_int(d$n_trials)
  )
  # The two lightest steps take dark ink; white clears contrast on the rest.
  d$label_ink <- ifelse(as.integer(d$sites_bucket) <= 2, INK$primary, "#ffffff")
  # Only label a segment wide enough to hold its label.
  d$label <- ifelse(d$share_of_phase >= 0.09, fmt_pct(d$share_of_phase, 0), "")
  stack <- position_stack(reverse = TRUE)
  # group is explicit in every chart that stacks or dodges. Otherwise ggplot derives it
  # from all discrete aesthetics, including ggiraph's tooltip strings, and the segments
  # (and their labels) are ordered alphabetically by tooltip text instead of by bucket.
  ggplot(d, aes(x = share_of_phase, y = phase, fill = sites_bucket, group = sites_bucket)) +
    # A surface-coloured stroke leaves a ~2px gap between adjacent segments.
    geom_col_interactive(aes(tooltip = tip, data_id = paste(phase, sites_bucket)),
                         position = stack, width = 0.68, colour = INK$surface, linewidth = 0.7) +
    geom_text(aes(label = label, colour = label_ink),
              position = position_stack(vjust = 0.5, reverse = TRUE), size = 2.7) +
    scale_colour_identity() +
    scale_fill_manual(values = stats::setNames(ORDINAL_5, SITE_BUCKETS), drop = FALSE,
                      name = "Sites listed") +
    scale_x_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
    labs(x = "Share of the phase's trials", y = NULL) +
    theme_tfe() +
    theme(panel.grid.major.x = element_blank())
}

plot_duration <- function(d) {
  d$phase <- phase_axis(d$phase)
  d$outcome <- factor(d$outcome, levels = names(OUTCOME_COLOURS))
  d$tip <- sprintf(
    "<b>%s, %s</b><br>Median %s<br>Middle half of trials: %s–%s<br>%s trials",
    d$phase, tolower(d$outcome), fmt_months(d$median), fmt_months(d$p25),
    fmt_months(d$p75), fmt_int(d$n_trials)
  )
  dodge <- position_dodge(width = 0.55, orientation = "y", reverse = TRUE)
  ggplot(d, aes(y = phase, colour = outcome, group = outcome)) +
    geom_linerange_interactive(
      aes(xmin = p25, xmax = p75, tooltip = tip, data_id = paste(phase, outcome)),
      position = dodge, linewidth = 2.2, alpha = 0.3
    ) +
    geom_point_interactive(aes(x = median, tooltip = tip, data_id = paste(phase, outcome)),
                           position = dodge, size = 2.4) +
    scale_colour_manual(values = OUTCOME_COLOURS, name = NULL) +
    scale_x_continuous(breaks = year_breaks, limits = c(0, NA), expand = expansion(mult = c(0, 0.04))) +
    labs(x = "Months from start to primary completion (dot: median, bar: middle half)", y = NULL) +
    theme_tfe()
}

# Returns NULL when no phase has enough finished trials to show a rate.
plot_termination <- function(d) {
  d <- d[d$n_final >= MIN_FINAL_FOR_RATE, , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  d$phase <- phase_axis(d$phase)
  d$area <- factor(d$area, levels = names(AREA_COLOURS))
  d$tip <- sprintf(
    "<b>%s, %s</b><br>%s stopped early<br>%s terminated, %s withdrawn<br>of %s trials with a final status",
    label_of(as.character(d$area), AREA_LABELS), d$phase, fmt_pct(d$stopped_early_rate),
    fmt_int(d$n_terminated), fmt_int(d$n_withdrawn), fmt_int(d$n_final)
  )
  dodge <- position_dodge(width = 0.76, preserve = "single", orientation = "y", reverse = TRUE)
  ggplot(d, aes(x = stopped_early_rate, y = phase, fill = area, group = area)) +
    geom_col_interactive(aes(tooltip = tip, data_id = paste(area, phase)),
                         position = dodge, width = 0.72, colour = INK$surface, linewidth = 0.6) +
    scale_fill_manual(values = AREA_COLOURS, labels = AREA_LABELS, name = NULL) +
    scale_x_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Share of trials with a final status that were terminated or withdrawn", y = NULL) +
    theme_tfe()
}

plot_stop_reasons <- function(d) {
  ranked <- d$category[order(-d$n_trials)]
  ordered <- c(setdiff(ranked, STOP_CATEGORY_LAST), intersect(STOP_CATEGORY_LAST, ranked))
  d$category <- factor(d$category, levels = rev(ordered))
  d$tip <- sprintf("<b>%s</b><br>%s of stopped trials (%s)",
                   d$category, fmt_pct(d$share), fmt_int(d$n_trials))
  ggplot(d, aes(x = share, y = category)) +
    geom_col_interactive(aes(tooltip = tip, data_id = category), fill = SERIES[[1]], width = 0.68) +
    geom_text(aes(label = fmt_pct(share)), hjust = -0.15, size = 2.8, colour = INK$secondary) +
    scale_x_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, 0.16))) +
    labs(x = "Share of terminated and withdrawn trials", y = NULL) +
    theme_tfe()
}

plot_countries <- function(d) {
  d$country <- factor(d$country, levels = rev(d$country))  # query returns them ranked
  d$tip <- sprintf("<b>%s</b><br>%s trials with a site here", d$country, fmt_int(d$n_trials))
  ggplot(d, aes(x = n_trials, y = country)) +
    geom_col_interactive(aes(tooltip = tip, data_id = country), fill = SERIES[[1]], width = 0.68) +
    geom_text(aes(label = fmt_int(n_trials)), hjust = -0.15, size = 2.8, colour = INK$secondary) +
    scale_x_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.16))) +
    labs(x = "Trials with at least one site in the country", y = NULL) +
    theme_tfe()
}

TOOLTIP_CSS <- paste(
  "background:#0b0b0b;color:#ffffff;padding:6px 9px;border-radius:4px;",
  "font-family:system-ui,-apple-system,'Segoe UI',sans-serif;font-size:12px;line-height:1.4;"
)

as_girafe <- function(plot, n_rows) {
  girafe(
    ggobj = plot,
    width_svg = 6,
    height_svg = 0.95 + 0.3 * n_rows,
    options = list(
      opts_tooltip(css = TOOLTIP_CSS, opacity = 1, use_fill = FALSE),
      opts_hover(css = "opacity:0.75;"),
      opts_selection(type = "none"),
      opts_toolbar(hidden = c("selection", "zoom", "misc"), saveaspng = FALSE),
      opts_sizing(rescale = TRUE)
    )
  )
}

# ---- Table views ---------------------------------------------------------------------
# Every chart has a table twin, so no value is reachable only by hovering.

order_by_phase <- function(d) d[order(match(d$phase, PHASE_LEVELS)), , drop = FALSE]

table_enrollment <- function(d) {
  d <- order_by_phase(d)
  data.frame(Phase = d$phase, Trials = fmt_int(d$n_trials), `25th percentile` = fmt_int(d$p25),
             Median = fmt_int(d$median), `75th percentile` = fmt_int(d$p75), check.names = FALSE)
}

table_sites <- function(d) {
  wide <- stats::reshape(d[, c("phase", "sites_bucket", "share_of_phase")], idvar = "phase",
                         timevar = "sites_bucket", direction = "wide")
  wide <- order_by_phase(wide)
  out <- data.frame(Phase = wide$phase, check.names = FALSE)
  for (b in SITE_BUCKETS) {
    share <- wide[[paste0("share_of_phase.", b)]]
    if (is.null(share)) share <- rep(0, nrow(wide))
    out[[paste(b, if (b == "1") "site" else "sites")]] <- fmt_pct(ifelse(is.na(share), 0, share), 0)
  }
  out
}

table_duration <- function(d) {
  d <- order_by_phase(d)
  data.frame(Phase = d$phase, Outcome = d$outcome, Trials = fmt_int(d$n_trials),
             `25th percentile` = fmt_months(d$p25), Median = fmt_months(d$median),
             `75th percentile` = fmt_months(d$p75), check.names = FALSE)
}

table_termination <- function(d) {
  d <- order_by_phase(d)
  d <- d[order(match(d$area, names(AREA_LABELS))), , drop = FALSE]
  data.frame(Area = label_of(d$area, AREA_LABELS), Phase = d$phase,
             `Finished trials` = fmt_int(d$n_final), Terminated = fmt_int(d$n_terminated),
             Withdrawn = fmt_int(d$n_withdrawn), `Stopped early` = fmt_pct(d$stopped_early_rate),
             check.names = FALSE)
}

table_stop_reasons <- function(d) {
  data.frame(Reason = d$category, Trials = fmt_int(d$n_trials), Share = fmt_pct(d$share),
             check.names = FALSE)
}

table_countries <- function(d) {
  data.frame(Country = d$country, Trials = fmt_int(d$n_trials), check.names = FALSE)
}

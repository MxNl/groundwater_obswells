# Try-out: the combined Sankey diagram with ggsankey instead of plain ggplot2
# (R/plots.R). Not part of the pipeline; run after targets::tar_make() from
# the project root. ggsankey is only on GitHub:
#   remotes::install_github("davidsjoberg/ggsankey")
#
# Every well is one row with its node at each step: "kept" while it stays,
# the reason at the step where it drops out, NA afterwards (ggsankey ends
# the flow there). At the end the usable wells go to their scenario group
# (first scenario of the list they meet), as in plot_sankey_combined().

library(targets)
library(dplyr)
library(ggplot2)
library(ggsankey)
tar_source("R")

wells <- tar_read(well_dtw)
spec <- tar_read(well_spec)
scenarios <- tar_read(scenarios)
groups <- assign_scenario_groups(tar_read(scenario_wells), scenarios)
font <- setup_plot_font(tar_read(plot_config)$font)

m <- filter(wells, in_meta) |>
  left_join(groups, by = "well_id")
step_ok <- list(
  "Ground elevation" = !is.na(m$elev_gs),
  "Valid levels" = !is.na(m$gwl_mean),
  "Screen depths" = !is.na(m$screen_top) & !is.na(m$screen_bot),
  "Recent data" = m$recent %in% TRUE,
  "Months covered" = coalesce(m$coverage_window, 0) >= spec$min_coverage,
  "Usable wells" = m$usable %in% TRUE
)
reasons <- c(
  "no ground elevation", "no valid levels", "no screen depths",
  "no recent data", "too many gaps", "a season missing"
)

nodes <- tibble(Metadata = rep("Wells", nrow(m)))
alive <- rep(TRUE, nrow(m))
for (i in seq_along(step_ok)) {
  kept <- alive & step_ok[[i]]
  nodes[[names(step_ok)[i]]] <- ifelse(kept, names(step_ok)[i], ifelse(alive, reasons[i], NA))
  alive <- kept
}
names_by_id <- setNames(c(scenarios$name, "No scenario"), c(scenarios$id, "none"))
nodes$Scenario <- ifelse(alive, names_by_id[m$scenario_id], NA)

long <- make_long(nodes, !!!rlang::syms(names(nodes)))
colours <- c(
  setNames(rep("grey45", length(step_ok) + 1), c("Wells", names(step_ok))),
  setNames(rep("grey80", length(reasons)), reasons),
  setNames(c(scenarios$colour, "grey85"), c(scenarios$name, "No scenario"))
)

p <- ggplot(long, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = node)) +
  geom_sankey(flow.alpha = 0.7, node.color = NA, width = 0.12) +
  geom_sankey_text(
    aes(label = node), size = 2.8, family = font, hjust = -0.1, colour = "grey20"
  ) +
  scale_fill_manual(values = colours, guide = "none") +
  labs(
    title = "From the well database to the scenarios (ggsankey)",
    caption = tar_read(plot_config)$caption,
    x = NULL
  ) +
  theme_sankey(base_size = 12, base_family = font)

ggsave(
  "plots/sankey_ggsankey.png", p,
  width = 11, height = 6.5, dpi = 300, bg = "white", device = ragg::agg_png
)

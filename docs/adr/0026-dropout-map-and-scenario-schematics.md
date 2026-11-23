# 26. Drop-out map and scenario schematics

- Status: accepted
- Date: 2026-11-23
- Code: `R/plots_dropout_schematics.R` (`plot_dropout_map()`,
  `plot_scenario_schematics()`); targets `dropout_map_plot`,
  `scenario_schematic_plot`; `plots/dropout_map.png`,
  `plots/scenario_schematics.png`

## Context

The Sankey diagrams show how many wells drop out at each step, but not
where they are. The scenario thresholds were only readable as numbers in
`scenarios_fixed`.

## Decision

- Drop-out map: one map per step of the flow chart (no ground elevation,
  no valid levels, no screen depths, no recent data, a season in too few
  years) plus one for the usable wells. Each well appears once, at its
  first failed step (`stage_flags()`, the same order as the Sankey). All
  FARM wells with coordinates are drawn in light grey behind. The panel
  titles count all wells, also those without coordinates, and say how many
  of them cannot be mapped.
- Wells with coordinates outside Germany are errors in the coordinates and
  are dropped from the map; they would stretch the extent and shrink every
  panel. They stay in the counts.
- Scenario schematics: one 1-D well profile per fixed scenario (ground
  surface, casing, coloured screen, an example mean water table with the
  saturated zone) with the thresholds as arrow spans and labels. Mean water
  table and deepest month are drawn from the ground surface; the screen top,
  screen bottom and screen length limits from the example water table,
  since they are relative to it. The example water table is the shallow
  limit of the mean depth plus 1.5 m (at most half the range); the example
  screen is one that meets the scenario. "Even coverage" is left out: its
  thresholds are fitted, not fixed.

## Consequences

- Both figures follow the settings: a changed threshold or step changes the
  labels and counts without code changes.
- The schematic is not to scale for a real well; the not-falling-dry rule
  (ADR 9) applies to every scenario and is stated in the subtitle rather
  than drawn.

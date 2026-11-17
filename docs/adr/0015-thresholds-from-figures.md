# 15. Preprocessing thresholds chosen from figures of the data

- Status: accepted
- Date: 2026-11-08 (error codes and removed values: 2026-11-16)
- Code: `R/thresholds.R`; settings `threshold_settings` (`history_years` 30,
  `recent_years` 3-20, `spike_mad` 3-10, `spike_min_dev` 0.5-2 m,
  `top_values` 15, `candidate_codes` 0, -1, -9, -99, -999, -9999, 99, 999,
  9999)

## Context

The preprocessing settings (recent window, years per season, years of the
mean, spike rule, plausibility limits) decide how many wells are usable and
how their depths come out. They should be chosen from the data, not set by
habit.

## Decision

- Figures in `plots/thresholds/` show for ranges of each setting the number
  of usable wells and the effect on the results: recent window and years per
  season, years of the mean, spike factor and minimum deviation, the implied
  depths of all measurements with the plausibility limits, the candidate
  error codes and the values most often removed.
- The settings in `well_config` were chosen from these figures: 10 years, 5
  years per season, 5 years for the mean, spike factor 5 and 1 m, valid
  depths -2 to 150 m.

## Consequences

- Frequent values around 30 m turned out to be real levels, not codes; codes
  are caught by the range check (ADR 3).

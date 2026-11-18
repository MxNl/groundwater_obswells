# 23. The level series is chosen by usability, then by priority

- Status: accepted (amends ADR 18)
- Date: 2026-11-18
- Code: `R/levels.R` (`level_candidates()`, `candidate_monthly()`,
  `choose_level_sources()`); targets `level_candidate_series`,
  `gwl_monthly_candidates`, `gwl_stats_candidates`

## Context

ADR 18 took the first source with at least one valid level. With the state
series added, more wells passed the ground elevation step but dropped out at
the levels. Some state series end years ago (NI 2010-2020, MV cut off at the
Excel row limit), and the first source with data then won over a gw_base
series with recent data, so the well dropped out later for lack of recent
data.

## Decision

- Every well gets all its candidate series: the state series, the linked
  gw_base well, the nearest other gw_base well with levels within 10 m.
- The window statistics are computed for every candidate (same checks,
  windows and spike rule).
- The first candidate in the order of priority that is usable over time
  (data in the recent window, every season in at least
  `min_years_per_season` years) is used; if none is, the first with at least
  one valid level. `level_choice` records when a series of higher priority
  was passed over.

## Consequences

- More wells are usable; a state series is still preferred whenever it is
  usable.
- The server reads the gw_base series of all candidates, also of wells that
  end up using state data.
- The flow chart counts wells without any usable or valid series at "no
  valid groundwater levels in any source"; that step follows the ground
  elevation step, so wells gaining a ground elevation can move there.

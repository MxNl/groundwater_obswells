# 4. A recent window and a mean window of the most recent years

- Status: accepted
- Date: 2026-11-04 (reference month as a target: 2026-11-16)
- Code: `R/wells.R` (`gwl_window_stats_sql()`, `current_reference_month()`);
  settings `recent_years` (10), `window_years` (5), `reference_date` (NULL:
  the current month)

## Context

The scenarios need the current state of the groundwater, not the long-term
history: levels change with climate, abstraction and land use. Series end at
different times and have gaps.

## Decision

- Recent window: the last `recent_years` years up to the reference month (the
  current month, or `reference_date`).
- Mean window: the `window_years` most recent calendar years with data in the
  recent window, not necessarily consecutive.
- Mean level: the mean of the monthly means in the mean window. A seasonal
  mean (each calendar month averaged first) is given where all 12 months
  have data.
- The reference month is a target checked on every run; it changes only
  when a new month starts, so the window moves on once a month.

## Consequences

- Wells whose data end more than `recent_years` years ago drop out ("no data
  in the last 10 years").
- The mean focuses on the latest five years with data even if some years are
  missing.
- Results change once a month as the window moves, and when levels are
  added.

## Alternatives considered

- A fixed period (e.g. 2015-2024): comparable between wells, but loses wells
  with gaps in it and ages.
- All data of a well: mixes old and current states.

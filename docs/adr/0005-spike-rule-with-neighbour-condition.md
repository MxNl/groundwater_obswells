# 5. Spikes: far from the median and from both neighbouring months

- Status: accepted
- Date: 2026-11-16
- Code: `R/wells.R` (`gwl_window_stats_sql()`), `R/diagnostics.R`
  (`mark_spikes()`); settings `spike_mad` (5), `spike_min_dev` (1 m),
  `spike_neighbours` (TRUE)

## Context

Single wrong values (typing errors, logger faults) shift the mean and the
maximum depth. A rule based only on the distance from the median also
removed real events lasting several months, such as wet winters or
droughts.

## Decision

- In the recent window, a monthly mean is a spike if it lies further than
  `max(spike_mad * MAD, spike_min_dev)` from the well's median (MAD scaled
  by 1.4826; only for wells with at least 3 months).
- With `spike_neighbours`, it must also lie that far from both neighbouring
  months with data (a missing neighbour does not count).
- Spikes are removed before all statistics and counted (`n_months_spike`).

## Consequences

- Single outliers are removed, events lasting several months stay.
- Two neighbouring wrong months are not caught.
- The minimum deviation of 1 m keeps very calm wells (small MAD) from losing
  normal variation.

## Alternatives considered

- Median distance only: removed real events (see the diagnostic sheets).
- Fixed limits in metres: do not fit wells with very different amplitudes.

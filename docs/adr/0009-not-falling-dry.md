# 9. "Not falling dry": the screen bottom below the average annual low

- Status: accepted
- Date: 2026-11-04 (as a criterion of its own)
- Code: `R/scenarios.R` (`scenario_checks()`), `R/wells.R`
  (`gwl_annual_low`)

## Context

A well whose screen lies above the water table in the dry season cannot be
sampled then.

## Decision

- A well passes if its screen bottom is at least as deep as the typical
  annual low (`screen_bot >= dtw_annual_low`, ADR 7).
- It applies to all scenarios.

## Consequences

- The average annual cycle is the reference: in a year drier than average
  the water table can fall below the screen bottom.
- No margin is required; a screen bottom exactly at the annual low passes.

## Alternatives considered

- The deepest month of the mean window: stricter, but driven by single dry
  years.
- A margin in metres: not needed for the purpose (decided 2026-11-17).

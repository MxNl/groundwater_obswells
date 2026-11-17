# Architecture decision records: methods

Decisions about the data and the analysis methods of this project, one file
per decision. Decisions about the look of the plots (colours, fonts,
layouts) are not recorded here.

The records were written after the fact, from the code, the commit history
and the discussions while the project was built; "Alternatives considered"
lists options that were discussed or are the obvious alternatives. Settings
named in the records are in `_targets.R` (`well_config`, `farm_config`,
`scenarios_fixed`, `coverage_settings`, `threshold_settings`,
`check_settings`), with the values in effect when the record was written.

## Wells and metadata

- [1. FARM master data as the wells analysed, filled from gw_base](0001-farm-master-data-as-well-base.md)
- [2. Linking wells across sources by state and normalised ID](0002-linking-wells-by-state-and-id.md)

## Groundwater levels and depth

- [3. Checks of single levels and monthly means on SQL Server](0003-level-checks-and-monthly-means-on-the-server.md)
- [4. A recent window and a mean window of the most recent years](0004-recent-window-and-mean-window.md)
- [5. Spikes: far from the median and from both neighbouring months](0005-spike-rule-with-neighbour-condition.md)
- [6. Usable wells: complete metadata and data in every season](0006-usable-wells.md)
- [7. Depth to groundwater: mean, maximum and annual low](0007-depth-measures.md)

## Scenarios

- [8. Scenarios: five criteria applied one after the other](0008-scenario-criteria.md)
- [9. "Not falling dry": the screen bottom below the average annual low](0009-not-falling-dry.md)
- [10. Fixed scenarios from guidelines and expert choices](0010-fixed-scenarios.md)
- [11. "Even coverage": a scenario fitted to cover Germany evenly](0011-even-coverage-scenario.md)
- [12. Each well counted once, in the most selective scenario it meets](0012-scenario-groups.md)

## Workflow

- [13. A targets pipeline on SQL Server via ODBC, DuckDB locally](0013-targets-pipeline-sql-server-duckdb.md)
- [14. When the database is read again](0014-when-queries-rerun.md)
- [15. Preprocessing thresholds chosen from figures of the data](0015-thresholds-from-figures.md)

## Source check (temporary)

- [16. A temporary check of the metadata against other sources](0016-source-check.md)

## Format

Each record has a status (accepted, superseded, …), a date, the code and
settings it concerns, and the sections Context, Decision, Consequences and,
where useful, Alternatives considered. A new decision gets the next number;
a changed decision gets a new record that supersedes the old one, which
stays.

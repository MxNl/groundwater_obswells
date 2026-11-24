# Architecture decision records: methods

Decisions about the data and the analysis methods of this project, one file
per decision. Decisions about the look of the plots (colours, fonts,
layouts) are not recorded here.

The records were written after the fact, from the code, the commit history
and the discussions while the project was built; "Alternatives considered"
lists options that were discussed or are the obvious alternatives. Settings
named in the records are in `_targets.R` (`well_config`, `farm_config`,
`level_config`, `scenarios_fixed`, `coverage_settings`, `threshold_settings`,
`check_settings`) and `config/id_mapping.csv`, with the values in effect
when the record was written. Within a topic, records are listed in the order
of the method, so their numbers are not always in order.

## Wells and metadata

- [1. FARM master data as the wells analysed, filled from gw_base](0001-farm-master-data-as-well-base.md)
- [2. Linking wells across sources by state and normalised ID](0002-linking-wells-by-state-and-id.md)
- [17. ID columns per source and state from a mapping table](0017-id-mapping-table.md)
- [20. Missing ground elevations estimated from the measuring point](0020-ground-elevation-from-measuring-point.md)

## Groundwater levels and depth

- [18. Groundwater levels from three sources by priority](0018-levels-from-three-sources.md)
- [21. The state level series are read in the pipeline](0021-state-series-read-in-the-pipeline.md)
- [22. State series without MENTOR levels, only for FARM wells](0022-state-series-without-mentor-farm-wells-only.md)
- [24. The BY "Nachlieferung" is read despite not using MENTOR](0024-by-nachlieferung.md)
- [27. NI: the FARM records replaced by the NI master data](0027-ni-master-data-replace-farm-records.md)
- [23. The level series is chosen by usability, then by priority](0023-level-series-chosen-by-usability.md)
- [19. The state level series come from a separate script](0019-state-series-from-the-delivery-script.md) (superseded by 21)
- [3. Checks of single levels and monthly means on SQL Server](0003-level-checks-and-monthly-means-on-the-server.md)
- [4. A recent window and a mean window of the most recent years](0004-recent-window-and-mean-window.md)
- [5. Spikes: far from the median and from both neighbouring months](0005-spike-rule-with-neighbour-condition.md)
- [6. Usable wells: complete metadata and data in every season](0006-usable-wells.md)
- [7. Depth to groundwater: mean, maximum and annual low](0007-depth-measures.md)

## Scenarios

- [8. Scenarios: five criteria applied one after the other](0008-scenario-criteria.md)
- [9. "Not falling dry": the screen bottom below the average annual low](0009-not-falling-dry.md)
- [10. Fixed scenarios from guidelines and expert choices](0010-fixed-scenarios.md)
- [25. National (2020): screens up to 4 m long](0025-national-2020-screens-up-to-4-m.md)
- [11. "Even coverage": a scenario fitted to cover Germany evenly](0011-even-coverage-scenario.md)
- [12. Each well counted once, in the most selective scenario it meets](0012-scenario-groups.md)
- [26. Drop-out map and scenario schematics](0026-dropout-map-and-scenario-schematics.md)

## Workflow

- [13. A targets pipeline on SQL Server via ODBC, DuckDB locally](0013-targets-pipeline-sql-server-duckdb.md)
- [14. When the database is read again](0014-when-queries-rerun.md)
- [15. Preprocessing thresholds chosen from figures of the data](0015-thresholds-from-figures.md)

## Source check (temporary)

- [16. A temporary check of the metadata against other sources](0016-source-check.md)

## Method diagrams

Flow charts of the methods are in [`../diagrams/`](../diagrams/README.md) as
Mermaid files (`.mmd`), rendered as SVG next to them. In Quarto they can be
used directly:

````markdown
```{mermaid}
%%| file: diagrams/wells_and_levels.mmd
```
````

## Format

Each record has a status (accepted, superseded, …), a date, the code and
settings it concerns, and the sections Context, Decision, Consequences and,
where useful, Alternatives considered. A new decision gets the next number;
a changed decision gets a new record that supersedes the old one, which
stays.

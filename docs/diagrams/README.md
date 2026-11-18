# Method diagrams

Flow charts of the methods, as Mermaid source (`.mmd`) with a rendered SVG
next to each. The numbers in the group titles refer to the decision records
in [`../adr/`](../adr/README.md).

| Diagram | Topic |
|---|---|
| [`wells_and_levels.mmd`](wells_and_levels.mmd) | 1 · FARM wells, ID mapping, filling from gw_base, ground elevation from the measuring point, level series by priority |
| [`depth_to_groundwater.mmd`](depth_to_groundwater.mmd) | 2 · checks of single levels, monthly means, windows, spikes, depths, usable wells |
| [`scenarios.mmd`](scenarios.mmd) | 3 · the five criteria, fixed and fitted scenarios, counting each well once |
| [`pipeline.mmd`](pipeline.mmd) | 4 · the main steps of the targets pipeline and their inputs |

Shapes: cylinders are data, boxes method steps, hexagons criteria (filters
and thresholds), rounded boxes results (plots and tables).

## Datasets

Every data box carries an ID, so a dataset can be followed across the charts
and to where it comes from.

| ID | Dataset | Origin |
|---|---|---|
| D1 | FARM master data (`stammdaten_geo.csv`) | source data (ADR 1) |
| D2 | ID mapping per source and state (`config/id_mapping.csv`) | project table (ADR 17) |
| D3 | gw_base metadata (`tbl_meta_well_import`) | source data (ADR 1) |
| D4 | gw_base levels (`tbl_gwl_raw`) | source data (ADR 3) |
| D5 | State level series: state deliveries and MENTOR (`state_levels_raw`) | built in the pipeline from the delivery folders (ADR 21) |
| D6 | Analysis table `well_meta`, with the origin of every value | created in chart 1 from D1–D3 (ADR 1, 17, 20) |
| D7 | Level source of every well, `level_sources` | created in chart 1 from D3–D6 (ADR 18) |
| D8 | Monthly means under the well's own ID, `gwl_monthly` | created in chart 2 from D4, D5, D7 (ADR 3, 18) |
| D9 | One row per well, `well_dtw` | created in chart 2 from D6, D8 (ADR 4–7) |
| D10 | Wells per scenario and criterion, `scenario_wells` | created in chart 3 from D9 (ADR 8–12) |

## Use in Quarto

Quarto renders Mermaid natively; the file can be included as is (the colours
and layout come from the `config` block at the top of each file):

````markdown
```{mermaid}
%%| file: diagrams/wells_and_levels.mmd
%%| fig-cap: "Wells, metadata and level series"
```
````

The SVGs contain HTML labels, which browsers and Quarto show, but some
graphics programs do not; use the Mermaid source or a PNG export there.

## Rendering

With the Mermaid CLI (`npm install @mermaid-js/mermaid-cli`):

```sh
mmdc -i wells_and_levels.mmd -o wells_and_levels.svg -b white
```

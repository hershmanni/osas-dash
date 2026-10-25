# OSAS Explorer

Interactive Shiny dashboard for publicly released Oregon state assessment group results.

## Run locally

From the repository root:

```r
shiny::runApp(".")
```

The app loads the newest `data/osas_dashboard-YYYYMMDD.rds` artifact.

## Rebuild prepared data

When a new dated source RDS is available, run:

```sh
Rscript scripts/build_dashboard_data.R ode_pagr_all_files-YYYYMMDD.rds
```

This creates a slim canonical artifact and validation CSV in `data/`. The build fills missing source URLs from `file_inventory.csv` without replacing URLs already present in the source, collapses duplicate rows, preserves file/URL provenance, and reports field-level disagreements.

## Run checks

```sh
Rscript tests/testthat.R
```

`testthat` is a development-only dependency and is not bundled in the deployment manifest.

## Interpretation notes

- Spring 2020 and 2021 have no records in the prepared dashboard source. Trend lines break across those years.
- Cohort trajectories connect aggregate grade-level results; they do not follow individual students.
- Rankings may combine state, district, and school results. These populations overlap, so combined ranks are descriptive. The dashboard also reports within-type ranks.
- Participant or participation-rate ranges indicate disagreement between otherwise duplicated source files. Agreed proficiency results remain available and retain their source links.

#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

source(file.path("R", "dashboard_helpers.R"), local = FALSE)

args <- commandArgs(trailingOnly = TRUE)
source_candidates <- sort(Sys.glob("ode_pagr_all_files-*.rds"), decreasing = TRUE)

source_path <- if (length(args) >= 1) args[[1]] else source_candidates[[1]]
if (is.null(source_path) || !file.exists(source_path)) {
  stop("Provide a source RDS path or place a dated ode_pagr_all_files-*.rds file in the app root")
}

source_date <- sub("^.*-([0-9]{8})\\.rds$", "\\1", basename(source_path))
if (!grepl("^[0-9]{8}$", source_date)) {
  stop("Could not derive an eight-digit source date from ", basename(source_path))
}

output_path <- if (length(args) >= 2) {
  args[[2]]
} else {
  file.path("data", paste0("osas_dashboard-", source_date, ".rds"))
}

validation_path <- if (length(args) >= 3) {
  args[[3]]
} else {
  file.path("data", paste0("osas_dashboard-validation-", source_date, ".csv"))
}

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(validation_path), recursive = TRUE, showWarnings = FALSE)

message("Reading ", source_path)
raw_data <- readRDS(source_path)
inventory_path <- "file_inventory.csv"
if (file.exists(inventory_path)) {
  inventory <- read.csv(inventory_path, stringsAsFactors = FALSE, strip.white = TRUE)
  missing_before <- sum(is.na(raw_data$source_url) | !nzchar(trimws(as.character(raw_data$source_url))))
  raw_data <- backfill_source_urls(raw_data, inventory)
  missing_after <- sum(is.na(raw_data$source_url) | !nzchar(trimws(as.character(raw_data$source_url))))
  message(
    "Backfilled ", format(missing_before - missing_after, big.mark = ","),
    " source URLs from ", inventory_path
  )
}
message("Canonicalizing ", format(nrow(raw_data), big.mark = ","), " rows")
artifact <- canonicalize_dashboard_data(raw_data, basename(source_path))

parent_conflicts <- artifact$organization_year |>
  filter(organization_type == "School") |>
  distinct(organization_key, spring_year, district_key) |>
  count(organization_key, spring_year, name = "parent_districts") |>
  filter(parent_districts > 1)

if (nrow(parent_conflicts) > 0) {
  stop(
    "Canonical build found ", nrow(parent_conflicts),
    " school-year keys assigned to more than one parent district"
  )
}

conflict_summary <- artifact$conflicts |>
  count(field, name = "value") |>
  transmute(section = "field_conflicts", metric = field, value)

validation_summary <- bind_rows(
  tibble(
    section = "rows",
    metric = c("raw", "canonical", "duplicates_removed", "source_pairs", "missing_source_urls"),
    value = c(
      artifact$metadata$raw_rows,
      artifact$metadata$canonical_rows,
      artifact$metadata$duplicate_rows_removed,
      artifact$metadata$source_pairs,
      artifact$metadata$missing_source_urls
    )
  ),
  conflict_summary,
  artifact$organizations |>
    count(organization_type, name = "value") |>
    transmute(section = "organizations", metric = organization_type, value),
  tibble(
    section = "validation",
    metric = "school_year_parent_conflicts",
    value = nrow(parent_conflicts)
  )
)

artifact$validation <- validation_summary

message("Writing ", output_path)
saveRDS(artifact, output_path, compress = "gzip")
write.csv(validation_summary, validation_path, row.names = FALSE, na = "")

message(
  "Built ", format(artifact$metadata$canonical_rows, big.mark = ","),
  " canonical observations; removed ",
  format(artifact$metadata$duplicate_rows_removed, big.mark = ","),
  " duplicate rows; recorded ",
  format(artifact$metadata$conflict_rows, big.mark = ","),
  " field conflicts"
)
message("Artifact size: ", round(file.info(output_path)$size / 1024^2, 1), " MB")

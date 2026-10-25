suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinyjqui)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
})

options(dplyr.summarise.inform = FALSE)

dashboard_version <- "v.2026.10.25"

source(file.path("R", "dashboard_helpers.R"), local = FALSE)

dashboard_files <- sort(Sys.glob(file.path("data", "osas_dashboard-[0-9]*.rds")), decreasing = TRUE)
if (length(dashboard_files) == 0) {
  stop(
    "No prepared dashboard artifact found. Run scripts/build_dashboard_data.R before starting the app."
  )
}

dashboard_data_path <- dashboard_files[[1]]
osas <- readRDS(dashboard_data_path)
osas_records <- osas$records
osas_sources <- osas$sources
osas_organizations <- add_organization_choice_labels(osas$organizations)
osas_organization_year <- osas$organization_year
osas_coverage <- osas$coverage
osas_conflicts <- osas$conflicts
osas_metadata <- osas$metadata
osas_validation <- osas$validation

dashboard_subjects <- sort(unique(as.character(osas_records$subject)))
dashboard_student_groups <- sort(unique(as.character(osas_records$student_group)))

grade_lookup <- osas_records |>
  distinct(grade_level, grade_num) |>
  mutate(
    grade_sort = if_else(is.na(grade_num), 99L, as.integer(grade_num)),
    grade_level = as.character(grade_level)
  ) |>
  arrange(grade_sort, grade_level)
dashboard_grades <- grade_lookup$grade_level
dashboard_tested_grades <- grade_lookup |>
  filter(!is.na(grade_num)) |>
  pull(grade_level)

dashboard_spring_years <- sort(unique(osas_records$spring_year))
dashboard_cohorts <- osas_records |>
  filter(!is.na(grade_num)) |>
  transmute(class_of = spring_year + (12L - grade_num)) |>
  distinct(class_of) |>
  arrange(class_of) |>
  pull(class_of)

organization_choice_rows <- osas_organizations |>
  mutate(type_order = match(as.character(organization_type), dashboard_org_types)) |>
  arrange(type_order, current_name)
organization_choices <- stats::setNames(
  organization_choice_rows$organization_key,
  organization_choice_rows$choice_label
)

organization_type_choices <- stats::setNames(dashboard_org_types, dashboard_org_types)

dashboard_metric_choices <- dashboard_metric_labels[
  names(dashboard_metric_labels) %in% c(dashboard_outcome_metrics, "participation_rate")
]

dashboard_palette <- c(
  "#0072B2", "#D55E00", "#009E73", "#CC79A7",
  "#E69F00", "#56B4E9", "#F0E442", "#000000"
)
population_group_colors <- stats::setNames(
  grDevices::hcl.colors(length(dashboard_student_groups), palette = "Dark 3"),
  dashboard_student_groups
)
population_group_shapes <- stats::setNames(
  rep(c(16, 17, 15, 18, 3, 7, 8, 4), length.out = length(dashboard_student_groups)),
  dashboard_student_groups
)

organization_type_shapes <- c(State = 18, District = 17, School = 16)
organization_type_colors <- c(State = "#5A5A5A", District = "#0072B2", School = "#009E73")

dashboard_participants_min <- 10
dashboard_participants_max <- ceiling(max(osas_records$participants_max, na.rm = TRUE))
dashboard_default_cohorts <- intersect(2022:2026, dashboard_cohorts)
if (length(dashboard_default_cohorts) == 0) {
  dashboard_default_cohorts <- tail(dashboard_cohorts, 5)
}

dashboard_default_subject <- if ("Mathematics" %in% dashboard_subjects) {
  "Mathematics"
} else {
  dashboard_subjects[[1]]
}

dashboard_default_group <- if ("Total Population (All Students)" %in% dashboard_student_groups) {
  "Total Population (All Students)"
} else {
  dashboard_student_groups[[1]]
}
dashboard_default_population_groups <- dashboard_default_group

dashboard_default_grades <- intersect(
  c("Grade 3", "Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8", "Grade HS (11)"),
  dashboard_tested_grades
)

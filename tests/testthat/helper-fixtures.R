suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

project_root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
source(file.path(project_root, "R", "dashboard_helpers.R"), local = FALSE)

dashboard_fixture <- function() {
  tibble::tibble(
    year_fall = c(2021, 2021, 2021, 2021, 2021, 2021, 2022),
    district_id = c(1, 1, 1, 1, NA, 1, 1),
    district = c("Example District", "Example District", "Example District", "Example District", "Oregon (All Districts)", "Example District", "Example District"),
    subject = "Mathematics",
    student_group = "Total Population (All Students)",
    grade_level = "Grade 5",
    number_of_participants = c(20, 22, 30, 30, 1000, 100, 24),
    participation_rate = c(90, 92, 95, 95, 97, 96, 93),
    percent_proficient = c(55, 55, 50, 51, 60, 58, 57),
    percent_level_1 = c(10, 10, 12, 12, 9, 11, 10),
    percent_level_2 = c(35, 35, 38, 37, 31, 31, 33),
    percent_level_3 = c(35, 35, 30, 30, 35, 35, 34),
    percent_level_4 = c(20, 20, 20, 20, 25, 23, 23),
    percent_level_5 = NA_real_,
    school_id = c(101, 101, 102, 102, NA, NA, 101),
    organization_type = c("School", "School", "School", "School", "State", "District", "School"),
    organization = c("Old School Name", "Old School Name", "Second School", "Second School", "Oregon (All Districts)", "Example District", "New School Name"),
    source_file = paste0("source-", seq_len(7), ".xlsx"),
    source_dl_date = as.Date("2026-09-29"),
    grade_num = 5,
    class_of = c(rep(2029, 6), 2030),
    source_url = paste0("https://example.org/source-", seq_len(7), ".xlsx")
  )
}

ranking_fixture <- function(include_state = TRUE) {
  data <- tibble::tibble(
    observation_id = 1:4,
    spring_year = 2026L,
    subject = factor("Mathematics"),
    grade_level = factor("Grade 5"),
    grade_num = 5L,
    student_group = factor("Total Population (All Students)"),
    organization_key = factor(c("state:oregon", "district:1", "school:1", "school:2")),
    organization_type = factor(c("State", "District", "School", "School")),
    participants_min = c(1000, 200, 40, 35),
    participants_max = c(1000, 200, 40, 35),
    participation_min = c(95, 95, 95, 95),
    participation_max = c(95, 95, 95, 95),
    percent_proficient = c(60, 80, 80, 40),
    percent_level_1 = NA_real_,
    percent_level_2 = NA_real_,
    percent_level_3 = NA_real_,
    percent_level_4 = NA_real_,
    percent_level_5 = NA_real_
  )
  if (!include_state) data <- data |> filter(as.character(organization_type) != "State")
  data
}

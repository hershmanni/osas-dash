test_that("typed organization IDs are stable and type-specific", {
  expect_equal(
    make_organization_key(
      c("State", "District", "School"),
      c(NA, 22, 22),
      c(NA, NA, 333)
    ),
    c("state:oregon", "district:22", "school:333")
  )
})

test_that("canonicalization collapses duplicates and retains field conflicts", {
  artifact <- canonicalize_dashboard_data(dashboard_fixture(), "fixture.rds")

  school_one <- artifact$records |>
    filter(as.character(organization_key) == "school:101", spring_year == 2022)
  expect_equal(nrow(school_one), 1)
  expect_equal(school_one$percent_proficient, 55)
  expect_equal(school_one$participants_min, 20)
  expect_equal(school_one$participants_max, 22)
  expect_equal(school_one$participation_min, 90)
  expect_equal(school_one$participation_max, 92)

  conflicts <- artifact$conflicts |>
    filter(organization_key == "school:101", spring_year == 2022)
  expect_setequal(conflicts$field, c("participants", "participation"))

  school_two <- artifact$records |>
    filter(as.character(organization_key) == "school:102")
  expect_true(is.na(school_two$percent_proficient))
  expect_true(any(
    artifact$conflicts$organization_key == "school:102" &
      artifact$conflicts$field == "percent_proficient"
  ))

  sources <- artifact$sources |>
    filter(observation_id == school_one$observation_id)
  expect_equal(nrow(sources), 2)
})

test_that("latest names display while historical names remain available", {
  artifact <- canonicalize_dashboard_data(dashboard_fixture(), "fixture.rds")
  school <- artifact$organizations |>
    filter(organization_key == "school:101")
  expect_equal(school$current_name, "New School Name")
  expect_match(school$historical_names, "Old School Name")
  expect_match(school$historical_names, "New School Name")
})

test_that("picker labels omit redundant type text and disambiguate duplicate names", {
  raw <- dashboard_fixture()
  duplicate_rows <- raw[6, ] |>
    slice(rep(1, 2)) |>
    mutate(
      organization_type = "School",
      school_id = c(201, 202),
      organization = "Shared School",
      source_file = c("shared-201.xlsx", "shared-202.xlsx"),
      source_url = c("https://example.org/shared-201", "https://example.org/shared-202")
    )
  artifact <- canonicalize_dashboard_data(bind_rows(raw, duplicate_rows), "fixture.rds")
  shared <- artifact$organizations |>
    filter(current_name == "Shared School")
  unique_school <- artifact$organizations |>
    filter(organization_key == "school:102")

  expect_equal(nrow(shared), 2)
  expect_true(all(grepl("^Shared School — Example District \\[school:[0-9]+\\]$", shared$choice_label)))
  expect_equal(n_distinct(shared$choice_label), 2)
  expect_equal(unique_school$choice_label, "Second School")
  expect_equal(
    artifact$organizations$choice_label[artifact$organizations$organization_key == "state:oregon"],
    "Oregon (All Districts)"
  )
})

test_that("mixed rankings provide overall and within-type ranks", {
  ranked <- compute_rankings(
    ranking_fixture(), 2026, "Mathematics", "Grade 5",
    "Total Population (All Students)", dashboard_org_types, 0, Inf
  )

  district <- ranked |> filter(as.character(organization_key) == "district:1")
  top_school <- ranked |> filter(as.character(organization_key) == "school:1")
  state <- ranked |> filter(as.character(organization_key) == "state:oregon")
  low_school <- ranked |> filter(as.character(organization_key) == "school:2")

  expect_equal(district$overall_rank, 1)
  expect_equal(top_school$overall_rank, 1)
  expect_equal(state$overall_rank, 3)
  expect_equal(low_school$overall_rank, 4)
  expect_equal(district$type_rank, 1)
  expect_equal(top_school$type_rank, 1)
  expect_equal(low_school$type_rank, 2)
  expect_equal(top_school$type_n, 2)
  expect_equal(unique(ranked$overall_n), 4)
})

test_that("participant bounds are inclusive and Oregon absence stays missing", {
  bounded <- compute_rankings(
    ranking_fixture(), 2026, "Mathematics", "Grade 5",
    "Total Population (All Students)", c("School"), 35, 40
  )
  expect_equal(nrow(bounded), 2)

  no_state <- compute_rankings(
    ranking_fixture(include_state = FALSE), 2026, "Mathematics", "Grade 5",
    "Total Population (All Students)", c("School"), 0, Inf
  )
  expect_true(all(is.na(no_state$difference_from_oregon)))
})

test_that("midrank percentiles are tie-aware", {
  expect_equal(midrank_percentile(c(80, 80, 60, 40)), c(250 / 3, 250 / 3, 100 / 3, 0))
  expect_equal(midrank_percentile(50), 100)
})

test_that("spring gaps prevent a direct 2019 to 2022 connection", {
  data <- tibble::tibble(
    organization_key = "school:1",
    subject = "Mathematics",
    grade_level = "Grade 5",
    student_group = c("Group A", "Group A", "Group B", "Group B"),
    spring_year = c(2019, 2022, 2019, 2022),
    value = c(50, 55, 60, 65)
  )
  completed <- add_spring_year_gaps(data, 2019:2022)
  expect_equal(nrow(completed), 8)
  expect_true(all(is.na(completed$value[completed$spring_year %in% 2020:2021])))
})

test_that("published ranges retain disagreements", {
  expect_equal(
    format_value_range(c(10, 10, NA), c(10, 12, 15), digits = 0),
    c("10", "10–12", "Not available")
  )
})

test_that("cohort gaps use derived testing years", {
  data <- tibble::tibble(
    organization_key = "school:1",
    class_of = 2030L,
    subject = "Mathematics",
    grade_num = c(3L, 4L),
    percent_proficient = c(50, 55)
  )
  completed <- add_cohort_covid_gaps(data, valid_grades = 3:4)
  expect_equal(completed$derived_spring_year, c(2021L, 2022L))
  expect_true(is.na(completed$percent_proficient[completed$grade_num == 3]))
  expect_equal(completed$percent_proficient[completed$grade_num == 4], 55)
})

test_that("source links preserve each file and URL pair", {
  sources <- tibble::tibble(
    observation_id = c(1L, 1L),
    source_file = factor(c("one.xlsx", "two.xlsx")),
    source_url = factor(c("https://example.org/one", "https://example.org/two"))
  )
  links <- source_links_for(sources, 1L)$source_links
  expect_match(links, "one.xlsx")
  expect_match(links, "two.xlsx")
  expect_match(links, "https://example.org/one", fixed = TRUE)
  expect_match(links, "https://example.org/two", fixed = TRUE)
})

test_that("missing source URLs never create broken anchors", {
  sources <- tibble::tibble(
    observation_id = 1L,
    source_file = factor("missing-url.xlsx"),
    source_url = factor(NA_character_)
  )
  links <- source_links_for(sources, 1L)$source_links
  expect_match(links, "missing-url.xlsx", fixed = TRUE)
  expect_match(links, "URL unavailable", fixed = TRUE)
  expect_false(grepl("href=", links, fixed = TRUE))
  expect_false(grepl("href=\"NA\"", links, fixed = TRUE))
})

test_that("source inventory fills only missing URLs and rejects ambiguity", {
  data <- tibble::tibble(
    source_file = c("one.xlsx", "two.xlsx", "three.xlsx"),
    source_url = c(NA_character_, "https://existing.example/two", "")
  )
  inventory <- tibble::tibble(
    file_name = c("one.xlsx", "two.xlsx"),
    url = c("https://inventory.example/one", "https://inventory.example/two")
  )

  filled <- backfill_source_urls(data, inventory)
  expect_equal(filled$source_url, c(
    "https://inventory.example/one", "https://existing.example/two", NA_character_
  ))

  conflicting <- bind_rows(
    inventory,
    tibble(file_name = "one.xlsx", url = "https://other.example/one")
  )
  expect_error(backfill_source_urls(data, conflicting), "conflicting URLs")
})

dashboard_outcome_metrics <- c(
  "percent_proficient",
  "percent_level_1",
  "percent_level_2",
  "percent_level_3",
  "percent_level_4",
  "percent_level_5"
)

dashboard_metric_labels <- c(
  percent_proficient = "Percent proficient",
  percent_level_1 = "Percent at Level 1",
  percent_level_2 = "Percent at Level 2",
  percent_level_3 = "Percent at Level 3",
  percent_level_4 = "Percent at Level 4",
  percent_level_5 = "Percent at Level 5",
  participation_rate = "Participation rate"
)

dashboard_org_types <- c("State", "District", "School")

first_non_missing <- function(x) {
  present <- which(!is.na(x))
  if (length(present) == 0) {
    return(x[NA_integer_][1])
  }
  x[present[1]]
}

numeric_min_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}

numeric_max_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

numeric_agreed_value <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  values <- unique(x[!is.na(x)])
  if (length(values) == 1) values[[1]] else NA_real_
}

numeric_has_conflict <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  length(unique(x[!is.na(x)])) > 1
}

format_ode_id <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), NA_character_, format(trunc(x), scientific = FALSE, trim = TRUE))
}

make_organization_key <- function(organization_type, district_id, school_id) {
  organization_type <- as.character(organization_type)
  district_id <- format_ode_id(district_id)
  school_id <- format_ode_id(school_id)

  key <- rep(NA_character_, length(organization_type))
  key[organization_type == "State"] <- "state:oregon"
  district_rows <- organization_type == "District" & !is.na(district_id)
  school_rows <- organization_type == "School" & !is.na(school_id)
  key[district_rows] <- paste0("district:", district_id[district_rows])
  key[school_rows] <- paste0("school:", school_id[school_rows])
  key
}

make_district_key <- function(district_id) {
  district_id <- format_ode_id(district_id)
  ifelse(is.na(district_id), NA_character_, paste0("district:", district_id))
}

assert_dashboard_columns <- function(data) {
  required <- c(
    "year_fall", "district_id", "district", "subject", "student_group",
    "grade_level", "number_of_participants", "participation_rate",
    dashboard_outcome_metrics, "school_id", "organization_type",
    "organization", "source_file", "source_dl_date", "grade_num",
    "class_of", "source_url"
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("Dashboard source is missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

backfill_source_urls <- function(data, inventory) {
  required_inventory_columns <- c("file_name", "url")
  missing_inventory_columns <- setdiff(required_inventory_columns, names(inventory))
  if (length(missing_inventory_columns) > 0) {
    stop(
      "Source inventory is missing required columns: ",
      paste(missing_inventory_columns, collapse = ", ")
    )
  }

  lookup <- inventory |>
    dplyr::transmute(
      source_file = trimws(as.character(file_name)),
      inventory_url = trimws(as.character(url))
    ) |>
    dplyr::filter(nzchar(source_file), nzchar(inventory_url))

  conflicting_urls <- lookup |>
    dplyr::distinct(source_file, inventory_url) |>
    dplyr::count(source_file, name = "url_count") |>
    dplyr::filter(url_count > 1)
  if (nrow(conflicting_urls) > 0) {
    stop(
      "Source inventory has conflicting URLs for: ",
      paste(conflicting_urls$source_file, collapse = ", ")
    )
  }

  lookup <- lookup |>
    dplyr::distinct(source_file, .keep_all = TRUE)

  data |>
    dplyr::mutate(
      source_file = as.character(source_file),
      source_url = as.character(source_url)
    ) |>
    dplyr::left_join(lookup, by = "source_file") |>
    dplyr::mutate(
      source_url = dplyr::if_else(
        is.na(source_url) | !nzchar(trimws(source_url)),
        inventory_url,
        source_url
      )
    ) |>
    dplyr::select(-inventory_url)
}

canonicalize_dashboard_data <- function(raw_data, source_file_name = NA_character_) {
  assert_dashboard_columns(raw_data)

  selected <- raw_data |>
    dplyr::transmute(
      spring_year = as.integer(year_fall + 1),
      subject = as.character(subject),
      grade_level = as.character(grade_level),
      grade_num = suppressWarnings(as.integer(grade_num)),
      class_of = suppressWarnings(as.integer(class_of)),
      student_group = as.character(student_group),
      district_id = suppressWarnings(as.integer(district_id)),
      district_name = as.character(district),
      school_id = suppressWarnings(as.integer(school_id)),
      organization_type = as.character(organization_type),
      organization_name = as.character(organization),
      number_of_participants = suppressWarnings(as.numeric(number_of_participants)),
      participation_rate = suppressWarnings(as.numeric(participation_rate)),
      percent_proficient = suppressWarnings(as.numeric(percent_proficient)),
      percent_level_1 = suppressWarnings(as.numeric(percent_level_1)),
      percent_level_2 = suppressWarnings(as.numeric(percent_level_2)),
      percent_level_3 = suppressWarnings(as.numeric(percent_level_3)),
      percent_level_4 = suppressWarnings(as.numeric(percent_level_4)),
      percent_level_5 = suppressWarnings(as.numeric(percent_level_5)),
      source_file = as.character(source_file),
      source_url = as.character(source_url),
      source_dl_date = as.Date(source_dl_date)
    ) |>
    dplyr::mutate(
      organization_key = make_organization_key(organization_type, district_id, school_id),
      district_key = make_district_key(district_id)
    )

  if (anyNA(selected$organization_key)) {
    stop(sum(is.na(selected$organization_key)), " source rows do not have a usable organization ID")
  }

  key_columns <- c(
    "spring_year", "subject", "grade_level", "student_group", "organization_key"
  )

  keyed <- selected |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_columns))) |>
    dplyr::mutate(observation_id = dplyr::cur_group_id()) |>
    dplyr::ungroup()

  sources <- keyed |>
    dplyr::distinct(observation_id, source_file, source_url, source_dl_date) |>
    dplyr::arrange(observation_id, source_file) |>
    dplyr::mutate(
      observation_id = as.integer(observation_id),
      source_file = factor(source_file),
      source_url = factor(source_url)
    )

  records <- keyed |>
    dplyr::group_by(observation_id) |>
    dplyr::summarise(
      spring_year = dplyr::first(spring_year),
      subject = dplyr::first(subject),
      grade_level = dplyr::first(grade_level),
      grade_num = numeric_agreed_value(grade_num),
      class_of = numeric_agreed_value(class_of),
      student_group = dplyr::first(student_group),
      organization_key = dplyr::first(organization_key),
      organization_type = dplyr::first(organization_type),
      organization_name = paste(sort(unique(organization_name)), collapse = " / "),
      district_key = first_non_missing(district_key),
      district_name = paste(sort(unique(district_name[!is.na(district_name)])), collapse = " / "),
      participants_min = numeric_min_or_na(number_of_participants),
      participants_max = numeric_max_or_na(number_of_participants),
      participants_conflict = numeric_has_conflict(number_of_participants),
      participation_min = numeric_min_or_na(participation_rate),
      participation_max = numeric_max_or_na(participation_rate),
      participation_conflict = numeric_has_conflict(participation_rate),
      participation_rate = numeric_agreed_value(participation_rate),
      dplyr::across(
        dplyr::all_of(dashboard_outcome_metrics),
        numeric_has_conflict,
        .names = "{.col}_conflict"
      ),
      dplyr::across(
        dplyr::all_of(dashboard_outcome_metrics),
        numeric_agreed_value
      ),
      source_count = dplyr::n_distinct(paste(source_file, source_url)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      observation_id = as.integer(observation_id),
      number_of_participants = participants_min,
      grade_num = as.integer(grade_num),
      class_of = as.integer(class_of)
    )

  conflict_columns <- c(
    "participants_conflict", "participation_conflict",
    paste0(dashboard_outcome_metrics, "_conflict")
  )

  conflicts <- records |>
    dplyr::select(
      observation_id, spring_year, subject, grade_level, student_group,
      organization_key, organization_type, participants_min, participants_max,
      participation_min, participation_max, dplyr::all_of(conflict_columns)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(conflict_columns),
      names_to = "field",
      values_to = "has_conflict"
    ) |>
    dplyr::filter(has_conflict) |>
    dplyr::mutate(field = sub("_conflict$", "", field)) |>
    dplyr::mutate(
      value_min = dplyr::case_when(
        field == "participants" ~ participants_min,
        field == "participation" ~ participation_min,
        TRUE ~ NA_real_
      ),
      value_max = dplyr::case_when(
        field == "participants" ~ participants_max,
        field == "participation" ~ participation_max,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::select(-has_conflict)

  conflicting_outcomes <- intersect(unique(conflicts$field), dashboard_outcome_metrics)
  if (length(conflicting_outcomes) > 0) {
    for (field_name in conflicting_outcomes) {
      affected_ids <- conflicts |>
        dplyr::filter(field == field_name) |>
        dplyr::pull(observation_id)
      outcome_ranges <- keyed |>
        dplyr::filter(observation_id %in% affected_ids) |>
        dplyr::group_by(observation_id) |>
        dplyr::summarise(
          value_min = numeric_min_or_na(.data[[field_name]]),
          value_max = numeric_max_or_na(.data[[field_name]]),
          .groups = "drop"
        )
      conflicts <- conflicts |>
        dplyr::left_join(
          outcome_ranges |>
            dplyr::rename(range_min = value_min, range_max = value_max),
          by = "observation_id"
        ) |>
        dplyr::mutate(
          value_min = dplyr::if_else(field == field_name, range_min, value_min),
          value_max = dplyr::if_else(field == field_name, range_max, value_max)
        ) |>
        dplyr::select(-range_min, -range_max)
    }
  }

  organization_year <- records |>
    dplyr::distinct(
      organization_key, spring_year, organization_type, organization_name,
      district_key, district_name
    ) |>
    dplyr::arrange(organization_key, spring_year)

  organization_lookup <- organization_year |>
    dplyr::group_by(organization_key) |>
    dplyr::arrange(dplyr::desc(spring_year), .by_group = TRUE) |>
    dplyr::summarise(
      organization_type = dplyr::first(organization_type),
      current_name = dplyr::first(organization_name),
      current_district_key = first_non_missing(district_key),
      current_district_name = first_non_missing(district_name),
      historical_names = paste(sort(unique(organization_name)), collapse = " | "),
      first_spring_year = min(spring_year, na.rm = TRUE),
      latest_spring_year = max(spring_year, na.rm = TRUE),
      .groups = "drop"
    ) |>
    add_organization_choice_labels() |>
    dplyr::arrange(factor(organization_type, levels = dashboard_org_types), current_name)

  coverage <- records |>
    dplyr::group_by(student_group, spring_year, subject, organization_type) |>
    dplyr::summarise(
      organization_count = dplyr::n_distinct(organization_key),
      .groups = "drop"
    )

  records <- records |>
    dplyr::select(
      observation_id, spring_year, subject, grade_level, grade_num,
      student_group, organization_key, organization_type,
      participants_min, participants_max,
      participation_min, participation_max,
      dplyr::all_of(dashboard_outcome_metrics)
    )

  factor_columns <- c(
    "subject", "grade_level", "student_group", "organization_key",
    "organization_type"
  )
  records <- records |>
    dplyr::mutate(dplyr::across(dplyr::all_of(factor_columns), factor))

  metadata <- list(
    source_file = source_file_name,
    source_download_date = if (all(is.na(selected$source_dl_date))) {
      as.Date(NA)
    } else {
      max(selected$source_dl_date, na.rm = TRUE)
    },
    built_at = Sys.time(),
    raw_rows = nrow(raw_data),
    canonical_rows = nrow(records),
    duplicate_rows_removed = nrow(raw_data) - nrow(records),
    source_pairs = nrow(sources),
    missing_source_urls = sum(
      is.na(as.character(sources$source_url)) |
        !nzchar(trimws(as.character(sources$source_url)))
    ),
    conflict_rows = nrow(conflicts),
    conflict_counts = as.data.frame(table(conflicts$field), stringsAsFactors = FALSE)
  )

  list(
    records = records,
    sources = sources,
    organizations = organization_lookup,
    organization_year = organization_year,
    coverage = coverage,
    conflicts = conflicts,
    metadata = metadata
  )
}

add_organization_choice_labels <- function(organizations) {
  organizations |>
    dplyr::mutate(choice_label = current_name) |>
    dplyr::group_by(organization_type, current_name) |>
    dplyr::mutate(
      choice_label = dplyr::if_else(
        dplyr::n() > 1 & organization_type == "School" &
          !is.na(current_district_name) & nzchar(current_district_name),
        paste0(current_name, " — ", current_district_name),
        current_name
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(organization_type, choice_label) |>
    dplyr::mutate(
      choice_label = if (dplyr::n() > 1) {
        paste0(choice_label, " [", organization_key, "]")
      } else {
        choice_label
      }
    ) |>
    dplyr::ungroup()
}

validate_metric <- function(metric) {
  allowed <- names(dashboard_metric_labels)
  if (length(metric) != 1 || is.na(metric) || !metric %in% allowed) {
    stop("Unsupported dashboard metric: ", paste(metric, collapse = ", "))
  }
  metric
}

midrank_percentile <- function(x) {
  result <- rep(NA_real_, length(x))
  present <- !is.na(x)
  n <- sum(present)
  if (n == 0) return(result)
  if (n == 1) {
    result[present] <- 100
    return(result)
  }
  ascending_rank <- rank(x[present], ties.method = "average")
  result[present] <- 100 * (ascending_rank - 1) / (n - 1)
  result
}

compute_rankings <- function(
  records,
  spring_year,
  subject,
  grade_level,
  student_group,
  organization_types = dashboard_org_types,
  participants_min = -Inf,
  participants_max = Inf
) {
  organization_types <- intersect(as.character(organization_types), dashboard_org_types)
  if (length(organization_types) == 0) return(records[0, , drop = FALSE])

  state_reference <- records |>
    dplyr::filter(
      .data$spring_year == !!as.integer(spring_year),
      as.character(.data$subject) == !!subject,
      as.character(.data$grade_level) == !!grade_level,
      as.character(.data$student_group) == !!student_group,
      as.character(.data$organization_type) == "State",
      !is.na(.data$percent_proficient)
    ) |>
    dplyr::summarise(oregon_proficiency = first_non_missing(.data$percent_proficient)) |>
    dplyr::pull(.data$oregon_proficiency)

  if (length(state_reference) == 0) state_reference <- NA_real_

  ranked <- records |>
    dplyr::filter(
      .data$spring_year == !!as.integer(spring_year),
      as.character(.data$subject) == !!subject,
      as.character(.data$grade_level) == !!grade_level,
      as.character(.data$student_group) == !!student_group,
      as.character(.data$organization_type) %in% !!organization_types,
      !is.na(.data$percent_proficient),
      !is.na(.data$participants_min),
      .data$participants_min >= !!participants_min,
      .data$participants_min <= !!participants_max
    ) |>
    dplyr::mutate(
      overall_rank = dplyr::min_rank(dplyr::desc(.data$percent_proficient)),
      overall_percentile = midrank_percentile(.data$percent_proficient),
      overall_n = dplyr::n(),
      oregon_proficiency = state_reference,
      difference_from_oregon = dplyr::if_else(
        is.na(oregon_proficiency), NA_real_, .data$percent_proficient - oregon_proficiency
      )
    ) |>
    dplyr::group_by(.data$organization_type) |>
    dplyr::mutate(
      type_rank = dplyr::min_rank(dplyr::desc(.data$percent_proficient)),
      type_percentile = midrank_percentile(.data$percent_proficient),
      type_n = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$overall_rank, .data$organization_type, .data$organization_key)

  ranked
}

add_spring_year_gaps <- function(data, year_range = NULL) {
  if (nrow(data) == 0) return(data)
  if (is.null(year_range)) {
    year_range <- seq(min(data$spring_year, na.rm = TRUE), max(data$spring_year, na.rm = TRUE))
  }
  series_columns <- intersect(
    c("organization_key", "subject", "grade_level"), names(data)
  )
  tidyr::complete(
    data,
    tidyr::nesting(!!!rlang::syms(series_columns)),
    spring_year = year_range
  )
}

derive_cohort_spring_year <- function(class_of, grade_num) {
  as.integer(class_of - (12 - grade_num))
}

add_cohort_covid_gaps <- function(data, valid_grades) {
  if (nrow(data) == 0) return(data)
  series_columns <- intersect(c("organization_key", "class_of", "subject"), names(data))
  completed <- tidyr::complete(
    data,
    tidyr::nesting(!!!rlang::syms(series_columns)),
    grade_num = valid_grades
  ) |>
    dplyr::mutate(
      derived_spring_year = derive_cohort_spring_year(.data$class_of, .data$grade_num)
    )

  metric_columns <- intersect(c(names(dashboard_metric_labels), "value"), names(completed))
  covid_rows <- completed$derived_spring_year %in% c(2020L, 2021L)
  completed[covid_rows, metric_columns] <- NA_real_
  completed
}

format_value_range <- function(minimum, maximum, digits = 1, suffix = "") {
  same <- is.na(minimum) | is.na(maximum) | minimum == maximum
  single <- ifelse(is.na(minimum), "Not available", paste0(round(minimum, digits), suffix))
  range <- paste0(round(minimum, digits), "–", round(maximum, digits), suffix)
  ifelse(same, single, range)
}

source_links_for <- function(sources, observation_ids) {
  sources |>
    dplyr::filter(.data$observation_id %in% !!observation_ids) |>
    dplyr::mutate(
      source_file = as.character(.data$source_file),
      source_url = as.character(.data$source_url),
      url_available = !is.na(.data$source_url) & nzchar(trimws(.data$source_url)),
      escaped_file = htmltools::htmlEscape(.data$source_file),
      source_link = ifelse(
        url_available,
        paste0(
          "<a href=\"", htmltools::htmlEscape(.data$source_url, attribute = TRUE),
          "\" target=\"_blank\" rel=\"noopener noreferrer\">",
          escaped_file, "</a>"
        ),
        paste0(escaped_file, " <span class=\"text-muted\">(URL unavailable)</span>")
      )
    ) |>
    dplyr::group_by(.data$observation_id) |>
    dplyr::summarise(source_links = paste(.data$source_link, collapse = "<br>"), .groups = "drop")
}

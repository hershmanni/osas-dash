source(file.path(project_root, "global.r"), local = FALSE, chdir = TRUE)
source(file.path(project_root, "server.r"), local = FALSE, chdir = TRUE)

test_that("dashboard displays the release version", {
  expect_equal(dashboard_version, "v.2026.10.25")
})

test_that("population view warns about the spring 2022 definition break", {
  ui_source <- paste(readLines(file.path(project_root, "ui.r"), warn = FALSE), collapse = "\n")
  expect_match(ui_source, "Important for spring 2022", fixed = TRUE)
  expect_match(ui_source, "reporting-definition break", fixed = TRUE)
})

test_that("organization selector uses clean flat labels while retaining every type", {
  expect_false(is.list(organization_choices))
  expect_equal(
    names(organization_choices)[organization_choices == "state:oregon"],
    "Oregon (All Districts)"
  )
  expect_setequal(unique(as.character(organization_choice_rows$organization_type)), dashboard_org_types)
  expect_equal(osas_metadata$missing_source_urls, 0)
  expect_false(any(
    is.na(as.character(osas_sources$source_url)) |
      !nzchar(trimws(as.character(osas_sources$source_url)))
  ))
})

test_that("trend state updates automatically after selector changes", {
  shiny::testServer(server, {
    session$setInputs(
      student_group = dashboard_default_group,
      subject = dashboard_default_subject,
      organizations = c("state:oregon", "district:2243", "school:693"),
      trend_metric = "percent_proficient",
      trend_grades = c("Grade 5"),
      trend_labels = FALSE
    )
    session$elapse(300)
    session$flushReact()
    current <- trend_state()
    expect_equal(current$organizations, c("state:oregon", "district:2243", "school:693"))
    expect_equal(current$subject, dashboard_default_subject)
    expect_equal(current$grades, "Grade 5")

    session$setInputs(trend_grades = c("Grade 5", "Grade 8"))
    session$elapse(300)
    session$flushReact()
    expect_equal(trend_state()$grades, c("Grade 5", "Grade 8"))
  })
})

test_that("adding All Grades preserves populated multi-row trend facets", {
  selected_grades <- c("Grade 6", "Grade 7", "Grade 8", "All Grades")

  shiny::testServer(server, {
    session$setInputs(
      student_group = dashboard_default_group,
      subject = "English Language Arts",
      organizations = c("state:oregon", "district:2243", "district:2180"),
      trend_metric = "percent_proficient",
      trend_grades = selected_grades,
      trend_labels = FALSE
    )
    session$elapse(300)
    session$flushReact()

    observed <- trend_data() |>
      filter(!is.na(observation_id), !is.na(value))
    expect_setequal(unique(observed$grade_level), selected_grades)

    plot <- jsonlite::fromJSON(as.character(output$trend_plot), simplifyVector = FALSE)
    result_traces <- Filter(
      function(trace) identical(trace$mode %||% "", "lines+markers") &&
        nzchar(trace$name %||% ""),
      plot$x$data
    )
    axis_pairs <- vapply(
      result_traces,
      function(trace) paste(trace$xaxis %||% "x", trace$yaxis %||% "y", sep = ":"),
      character(1)
    )
    paired_axes <- vapply(
      result_traces,
      function(trace) {
        identical(
          sub("^x", "", trace$xaxis %||% "x"),
          sub("^y", "", trace$yaxis %||% "y")
        )
      },
      logical(1)
    )

    expect_true(all(paired_axes))
    expect_length(unique(axis_pairs), length(selected_grades))

    participation_plot <- jsonlite::fromJSON(
      as.character(output$participation_plot),
      simplifyVector = FALSE
    )
    participation_traces <- Filter(
      function(trace) identical(trace$mode %||% "", "lines+markers") &&
        nzchar(trace$name %||% ""),
      participation_plot$x$data
    )
    participation_axes_are_paired <- vapply(
      participation_traces,
      function(trace) {
        identical(
          sub("^x", "", trace$xaxis %||% "x"),
          sub("^y", "", trace$yaxis %||% "y")
        )
      },
      logical(1)
    )

    expect_true(all(participation_axes_are_paired))
  })
})

test_that("default trend plot is compact and vertically resizable", {
  shiny::testServer(server, {
    session$setInputs(trend_grades = dashboard_default_grades)
    session$flushReact()
    rendered <- output$trend_plot_ui$html
    expect_match(rendered, "resizable-plot", fixed = TRUE)
    expect_match(rendered, "height:500px", fixed = TRUE)
    expect_match(rendered, "&quot;handles&quot;:&quot;s&quot;", fixed = TRUE)
    expect_match(rendered, "Drag to resize plot height", fixed = TRUE)
    expect_match(rendered, "aria-valuenow", fixed = TRUE)
    expect_match(rendered, "ArrowUp", fixed = TRUE)
  })
})

test_that("population view compares multiple published All Grades counts", {
  comparison_groups <- intersect(
    c("Total Population (All Students)", "White"),
    dashboard_student_groups
  )
  expect_length(comparison_groups, 2)

  shiny::testServer(server, {
    session$setInputs(
      subject = dashboard_default_subject,
      organizations = "state:oregon",
      population_groups = comparison_groups
    )
    session$elapse(300)
    session$flushReact()

    observed <- population_data() |>
      filter(!is.na(observation_id), !is.na(value))
    expect_setequal(unique(observed$student_group), comparison_groups)
    expect_equal(unique(observed$organization_key), "state:oregon")
    expect_true(all(observed$grade_level == "All Grades"))
    expect_equal(observed$value, observed$participants_min)

    plot <- jsonlite::fromJSON(as.character(output$population_plot), simplifyVector = FALSE)
    plot_keys <- suppressWarnings(as.integer(unlist(lapply(plot$x$data, `[[`, "key"))))
    clicked_id <- observed$observation_id[[1]]
    clicked_group <- observed$student_group[[1]]
    marker_keys <- suppressWarnings(
      Filter(
        function(trace) grepl("markers", trace$mode %||% "", fixed = TRUE),
        plot$x$data
      ) |>
        lapply(`[[`, "key") |>
        unlist() |>
        as.integer()
    )
    expect_equal(plot$x$source, "population")
    expect_true(clicked_id %in% plot_keys)
    expect_true(clicked_id %in% marker_keys)

    click_json <- jsonlite::toJSON(list(key = clicked_id), auto_unbox = TRUE)
    do.call(session$setInputs, stats::setNames(list(click_json), "plotly_click-population"))
    session$flushReact()
    expect_match(output$population_source_details$html, "Students tested", fixed = TRUE)
    expect_match(output$population_source_details$html, "href=", fixed = TRUE)

    session$setInputs(clear_source_population = 1)
    session$flushReact()
    expect_match(
      output$population_source_details$html,
      "Click a reported data point",
      fixed = TRUE
    )

    do.call(session$setInputs, stats::setNames(list(click_json), "plotly_click-population"))
    session$flushReact()
    expect_match(output$population_source_details$html, "href=", fixed = TRUE)

    session$setInputs(population_groups = setdiff(comparison_groups, clicked_group))
    session$elapse(300)
    session$flushReact()
    expect_match(
      output$population_source_details$html,
      "Click a reported data point",
      fixed = TRUE
    )
    expect_false(grepl("href=", output$population_source_details$html, fixed = TRUE))
  })
})

test_that("benchmark action restores benchmark styling after reselection", {
  school <- osas_organizations |>
    filter(organization_type == "School", !is.na(current_district_key)) |>
    slice(1)
  school_key <- as.character(school$organization_key)
  district_key <- as.character(school$current_district_key)

  shiny::testServer(server, {
    session$setInputs(organizations = c("state:oregon", district_key, school_key))
    session$flushReact()
    benchmark_keys(c("state:oregon", district_key))

    session$setInputs(organizations = school_key)
    session$flushReact()
    expect_true("state:oregon" %in% benchmark_keys())
    expect_false(district_key %in% benchmark_keys())

    session$setInputs(organizations = c("state:oregon", district_key, school_key))
    session$setInputs(add_benchmarks = 1)
    session$flushReact()
    expect_true(all(c("state:oregon", district_key) %in% benchmark_keys()))
  })
})

test_that("trend legend uses clean organization names without type suffixes", {
  school_key <- osas_organizations |>
    filter(current_name == "Ainsworth Elementary School") |>
    slice(1) |>
    pull(organization_key) |>
    as.character()

  shiny::testServer(server, {
    session$setInputs(
      student_group = dashboard_default_group,
      subject = dashboard_default_subject,
      organizations = c(school_key, "state:oregon"),
      trend_metric = "percent_proficient",
      trend_grades = "Grade 5",
      trend_labels = FALSE
    )
    session$elapse(300)
    session$flushReact()

    serialized_plot <- as.character(output$trend_plot)
    plot <- jsonlite::fromJSON(serialized_plot, simplifyVector = FALSE)
    trace_names <- unique(vapply(
      plot$x$data,
      function(trace) if (is.null(trace$name)) "" else trace$name,
      character(1)
    ))
    trace_names <- trace_names[nzchar(trace_names)]

    expect_setequal(trace_names, c("Ainsworth Elementary School", "Oregon (All Districts)"))
    expect_false(any(grepl(",(State|District|School)$", trace_names)))
    expect_match(serialized_plot, ".xlsx", fixed = TRUE)
    expect_match(serialized_plot, "Click point for source links", fixed = TRUE)
    expect_false(grepl("; ", serialized_plot, fixed = TRUE))

    clicked_id <- trend_data() |>
      filter(!is.na(value), organization_key == school_key) |>
      slice(1) |>
      pull(observation_id)
    trend_keys <- suppressWarnings(as.integer(unlist(lapply(plot$x$data, `[[`, "key"))))
    expect_equal(plot$x$source, "trend")
    expect_true(clicked_id %in% trend_keys)
    click_json <- jsonlite::toJSON(list(key = clicked_id), auto_unbox = TRUE)
    do.call(session$setInputs, stats::setNames(list(click_json), "plotly_click-trend"))
    session$flushReact()
    source_panel <- output$trend_source_details$html
    expect_match(source_panel, "Source files", fixed = TRUE)
    expect_match(source_panel, "href=", fixed = TRUE)
    expect_match(source_panel, "Percent proficient", fixed = TRUE)
    expect_match(source_panel, "clear_source_trend", fixed = TRUE)

    session$setInputs(clear_source_trend = 1)
    session$flushReact()
    expect_match(output$trend_source_details$html, "Click a reported data point", fixed = TRUE)
    expect_false(grepl("href=", output$trend_source_details$html, fixed = TRUE))

    other_point <- trend_data() |>
      filter(!is.na(value), !is.na(observation_id), observation_id != clicked_id) |>
      slice(1)
    other_click <- jsonlite::toJSON(list(key = other_point$observation_id[[1]]), auto_unbox = TRUE)
    do.call(session$setInputs, stats::setNames(list(other_click), "plotly_click-trend"))
    session$flushReact()
    expect_match(
      output$trend_source_details$html,
      other_point$choice_label[[1]],
      fixed = TRUE
    )

    do.call(session$setInputs, stats::setNames(list(other_click), "plotly_click-trend"))
    session$flushReact()
    expect_match(output$trend_source_details$html, "Click a reported data point", fixed = TRUE)
    expect_false(grepl("href=", output$trend_source_details$html, fixed = TRUE))
  })
})

test_that("source tooltip labels name a file without becoming unbounded", {
  expect_equal(format_source_hover(c(NA, "zero.xlsx", "one.xlsx", "many.xlsx"), c(NA, 0, 1, 6)), c(
    "Sources: unavailable",
    "Sources: unavailable",
    "Source: one.xlsx",
    "First source: many.xlsx<br>+5 more files"
  ))
  expect_match(
    format_source_hover("one.xlsx", 1, click_hint = TRUE),
    "Click point for source links",
    fixed = TRUE
  )
})

test_that("coverage labels switch to white on the darkest cells", {
  expect_equal(
    contrast_text_color(c(1, 180, 600, 900, 1200)),
    c("#111111", "#111111", "#111111", "#FFFFFF", "#FFFFFF")
  )
  expect_equal(contrast_text_color(rep(1, 3)), rep("#111111", 3))
})

test_that("ranking points and heatmap cells reveal persistent source links", {
  shiny::testServer(server, {
    session$setInputs(
      student_group = dashboard_default_group,
      subject = dashboard_default_subject,
      organizations = "state:oregon",
      rank_year = max(dashboard_spring_years),
      rank_types = dashboard_org_types,
      rank_grade = if ("Grade 5" %in% dashboard_grades) "Grade 5" else dashboard_grades[[1]],
      rank_top_n = 10,
      rank_min = dashboard_participants_min,
      rank_max = dashboard_participants_max
    )
    session$elapse(300)
    session$flushReact()

    observation_id <- ranking_context()$top |>
      filter(!is.na(observation_id)) |>
      slice(1) |>
      pull(observation_id)
    click_json <- jsonlite::toJSON(
      list(key = observation_id, customdata = "Stale client tooltip must be ignored"),
      auto_unbox = TRUE
    )

    ranking_plot <- jsonlite::fromJSON(as.character(output$ranking_plot), simplifyVector = FALSE)
    ranking_keys <- suppressWarnings(as.integer(unlist(lapply(ranking_plot$x$data, `[[`, "key"))))
    ranking_marker_keys <- Filter(
      function(trace) grepl("markers", trace$mode %||% "", fixed = TRUE),
      ranking_plot$x$data
    ) |>
      lapply(`[[`, "key") |>
      unlist() |>
      as.integer()
    expect_equal(ranking_plot$x$source, "ranking")
    expect_true(observation_id %in% ranking_keys)
    expect_true(observation_id %in% ranking_marker_keys)
    do.call(session$setInputs, stats::setNames(list(click_json), "plotly_click-ranking"))
    session$flushReact()
    expect_match(output$ranking_source_details$html, "href=", fixed = TRUE)
    expect_match(output$ranking_source_details$html, "Overall rank", fixed = TRUE)
    expect_false(grepl("Stale client tooltip", output$ranking_source_details$html, fixed = TRUE))

    heatmap <- jsonlite::fromJSON(as.character(output$grade_heatmap), simplifyVector = FALSE)
    heatmap_keys <- suppressWarnings(as.integer(unlist(lapply(heatmap$x$data, `[[`, "key"))))
    heatmap_trace <- Filter(
      function(trace) identical(trace$type, "heatmap"),
      heatmap$x$data
    )[[1]]
    expect_equal(heatmap$x$source, "rank_heatmap")
    expect_true(observation_id %in% heatmap_keys)
    expect_true(isTRUE(heatmap_trace$`_isNestedKey`))
    expect_equal(length(heatmap_trace$key), length(heatmap_trace$y))
    expect_true(all(vapply(
      heatmap_trace$key,
      length,
      integer(1)
    ) == length(heatmap_trace$x)))
    do.call(session$setInputs, stats::setNames(list(click_json), "plotly_click-rank_heatmap"))
    session$flushReact()
    expect_match(output$heatmap_source_details$html, "href=", fixed = TRUE)

    stale_id <- osas_records |>
      filter(spring_year != max(dashboard_spring_years)) |>
      slice(1) |>
      pull(observation_id)
    stale_click <- jsonlite::toJSON(list(key = stale_id), auto_unbox = TRUE)
    do.call(session$setInputs, stats::setNames(list(stale_click), "plotly_click-ranking"))
    session$flushReact()
    expect_match(output$ranking_source_details$html, "Click a reported data point", fixed = TRUE)
    expect_false(grepl("href=", output$ranking_source_details$html, fixed = TRUE))
  })
})

test_that("cohort points reveal persistent source links", {
  shiny::testServer(server, {
    session$setInputs(
      student_group = dashboard_default_group,
      subject = dashboard_default_subject,
      organizations = "state:oregon",
      cohort_metric = "percent_proficient",
      cohort_classes = dashboard_default_cohorts,
      cohort_grades = dashboard_tested_grades,
      cohort_labels = FALSE
    )
    session$elapse(300)
    session$flushReact()

    observation_id <- cohort_data() |>
      filter(!is.na(value), !is.na(observation_id)) |>
      slice(1) |>
      pull(observation_id)
    click_json <- jsonlite::toJSON(list(key = observation_id), auto_unbox = TRUE)

    cohort_plot <- jsonlite::fromJSON(as.character(output$cohort_plot), simplifyVector = FALSE)
    cohort_keys <- suppressWarnings(as.integer(unlist(lapply(cohort_plot$x$data, `[[`, "key"))))
    expect_equal(cohort_plot$x$source, "cohort")
    expect_true(observation_id %in% cohort_keys)
    do.call(session$setInputs, stats::setNames(list(click_json), "plotly_click-cohort"))
    session$flushReact()
    expect_match(output$cohort_source_details$html, "href=", fixed = TRUE)
    expect_match(output$cohort_source_details$html, "Class of", fixed = TRUE)
  })
})

test_that("source panel clears when a metric change makes the clicked point unplottable", {
  conflicting <- osas_records |>
    filter(
      !is.na(percent_proficient),
      !is.na(participation_min),
      !is.na(participation_max),
      participation_min != participation_max
    ) |>
    slice(1)

  shiny::testServer(server, {
    session$setInputs(
      student_group = as.character(conflicting$student_group[[1]]),
      subject = as.character(conflicting$subject[[1]]),
      organizations = as.character(conflicting$organization_key[[1]]),
      trend_metric = "percent_proficient",
      trend_grades = as.character(conflicting$grade_level[[1]]),
      trend_labels = FALSE
    )
    session$elapse(300)
    session$flushReact()

    click_json <- jsonlite::toJSON(
      list(key = conflicting$observation_id[[1]]),
      auto_unbox = TRUE
    )
    do.call(session$setInputs, stats::setNames(list(click_json), "plotly_click-trend"))
    session$flushReact()
    expect_match(output$trend_source_details$html, "href=", fixed = TRUE)

    session$setInputs(trend_metric = "participation_rate")
    session$elapse(300)
    session$flushReact()
    expect_match(output$trend_source_details$html, "Click a reported data point", fixed = TRUE)
    expect_false(grepl("href=", output$trend_source_details$html, fixed = TRUE))
  })
})

test_that("mixed ranking defaults can include all organization types", {
  ranked <- compute_rankings(
    osas_records,
    max(dashboard_spring_years),
    dashboard_default_subject,
    if ("Grade 5" %in% dashboard_grades) "Grade 5" else dashboard_grades[[1]],
    dashboard_default_group,
    dashboard_org_types,
    dashboard_participants_min,
    dashboard_participants_max
  )
  expect_true(all(dashboard_org_types %in% unique(as.character(ranked$organization_type))))
  expect_true(all(c("overall_rank", "type_rank") %in% names(ranked)))
})

test_that("cohort state updates automatically after selector changes", {
  shiny::testServer(server, {
    session$setInputs(
      student_group = dashboard_default_group,
      subject = dashboard_default_subject,
      organizations = "state:oregon",
      cohort_metric = "percent_proficient",
      cohort_classes = c(2024, 2025),
      cohort_grades = c("Grade 5", "Grade 8"),
      cohort_labels = FALSE
    )
    session$elapse(300)
    session$flushReact()
    expect_equal(cohort_state()$cohorts, c(2024L, 2025L))

    session$setInputs(cohort_classes = c(2025, 2026))
    session$elapse(300)
    session$flushReact()
    expect_equal(cohort_state()$cohorts, c(2025L, 2026L))
  })
})

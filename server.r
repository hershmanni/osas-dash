decorate_records <- function(data) {
  data |>
    mutate(
      organization_key = as.character(organization_key),
      organization_type = as.character(organization_type)
    ) |>
    left_join(
      osas_organization_year |>
        select(
          organization_key, spring_year, historical_name = organization_name,
          district_key, district_name
        ),
      by = c("organization_key", "spring_year")
    ) |>
    left_join(
      osas_organizations |>
        select(
          organization_key, current_name, choice_label, historical_names,
          current_district_key, current_district_name
        ),
      by = "organization_key"
    )
}

metric_card <- function(label, value, detail = NULL) {
  div(
    class = "metric-card",
    div(class = "metric-label", label),
    div(class = "metric-value", value),
    if (!is.null(detail)) div(class = "metric-detail", detail)
  )
}

dashboard_panel_theme <- function() {
  theme(
    panel.border = element_rect(color = "#8f9ca8", fill = NA, linewidth = 0.7),
    strip.background = element_rect(color = "#8f9ca8", fill = "#eef2f5", linewidth = 0.7),
    strip.text = element_text(color = "#26323d", face = "bold", margin = margin(6, 6, 6, 6)),
    strip.placement = "inside",
    panel.spacing = grid::unit(12, "pt"),
    plot.background = element_rect(color = "#b7c2cc", fill = "white", linewidth = 0.7),
    plot.margin = margin(10, 10, 10, 10)
  )
}

safe_bounds <- function(minimum, maximum) {
  minimum <- suppressWarnings(as.numeric(minimum))
  maximum <- suppressWarnings(as.numeric(maximum))
  if (length(minimum) == 0 || is.na(minimum)) minimum <- -Inf
  if (length(maximum) == 0 || is.na(maximum)) maximum <- Inf
  if (minimum > maximum) c(maximum, minimum) else c(minimum, maximum)
}

source_conflict_label <- function(participants_min, participants_max, participation_min, participation_max) {
  participant_conflict <- !is.na(participants_min) & !is.na(participants_max) & participants_min != participants_max
  participation_conflict <- !is.na(participation_min) & !is.na(participation_max) & participation_min != participation_max
  dplyr::case_when(
    participant_conflict & participation_conflict ~ "Participant count and participation rate differ across sources",
    participant_conflict ~ "Participant count differs across sources",
    participation_conflict ~ "Participation rate differs across sources",
    TRUE ~ "None"
  )
}

metric_values <- function(data, metric) {
  metric <- validate_metric(metric)
  if (metric == "participation_rate") {
    return(ifelse(
      data$participation_min == data$participation_max,
      data$participation_min,
      NA_real_
    ))
  }
  data[[metric]]
}

organization_color_values <- function(keys, registry) {
  keys <- unique(as.character(keys))
  known <- registry()
  new_keys <- setdiff(keys, names(known))
  if (length(new_keys) > 0) {
    start <- length(known)
    new_colors <- dashboard_palette[((seq_along(new_keys) + start - 1) %% length(dashboard_palette)) + 1]
    known <- c(known, stats::setNames(new_colors, new_keys))
    registry(known)
  }
  known[keys]
}

source_names_for <- function(observation_ids) {
  ids <- unique(observation_ids[!is.na(observation_ids)])
  if (length(ids) == 0) {
    return(tibble::tibble(
      observation_id = integer(), source_count = integer(), source_preview = character()
    ))
  }
  osas_sources |>
    filter(observation_id %in% ids) |>
    mutate(source_file = as.character(source_file)) |>
    group_by(observation_id) |>
    summarise(
      source_count = n_distinct(source_file),
      source_preview = sort(unique(source_file))[[1]],
      .groups = "drop"
    )
}

format_source_hover <- function(source_preview, source_count, click_hint = FALSE) {
  unavailable <- is.na(source_count) | source_count < 1 | is.na(source_preview) | !nzchar(source_preview)
  result <- ifelse(
    unavailable,
    "Sources: unavailable",
    paste0(
      ifelse(source_count == 1, "Source: ", "First source: "),
      source_preview,
      ifelse(source_count > 1, paste0("<br>+", source_count - 1, " more file", ifelse(source_count == 2, "", "s")), "")
    )
  )
  if (isTRUE(click_hint)) {
    result <- ifelse(unavailable, result, paste0(result, "<br><b>Click point for source links</b>"))
  }
  result
}

contrast_text_color <- function(values, dark_fill_threshold = 0.65) {
  values <- suppressWarnings(as.numeric(values))
  present <- is.finite(values)
  colors <- rep("#111111", length(values))
  if (!any(present)) return(colors)

  bounds <- range(values[present])
  if (diff(bounds) == 0) return(colors)
  fill_position <- (values - bounds[[1]]) / diff(bounds)
  colors[present & fill_position >= dark_fill_threshold] <- "#FFFFFF"
  colors
}

plotly_click_payload <- function(input, source) {
  click_json <- input[[paste0("plotly_click-", source)]]
  if (is.null(click_json)) return(NULL)
  tryCatch(
    jsonlite::parse_json(click_json, simplifyVector = TRUE),
    error = function(error) NULL
  )
}

source_panel_placeholder <- function() {
  div(
    class = "plot-source-details plot-source-placeholder",
    icon("circle-info"),
    " Click a reported data point to view details and source files; click it again to clear."
  )
}

source_details_panel <- function(observation_id, tooltip_html = NULL, source = NULL) {
  if (length(observation_id) != 1 || is.na(observation_id)) return(source_panel_placeholder())

  record <- osas_records |>
    filter(.data$observation_id == .env$observation_id) |>
    decorate_records() |>
    slice(1)
  if (nrow(record) == 0) return(source_panel_placeholder())

  sources <- osas_sources |>
    filter(.data$observation_id == .env$observation_id) |>
    transmute(
      source_file = as.character(source_file),
      source_url = as.character(source_url)
    ) |>
    distinct() |>
    arrange(source_file)
  if (nrow(sources) == 0) return(source_panel_placeholder())

  source_items <- lapply(seq_len(nrow(sources)), function(index) {
    file_name <- sources$source_file[[index]]
    file_url <- sources$source_url[[index]]
    tags$li(
      if (!is.na(file_url) && nzchar(trimws(file_url))) {
        tags$a(
          href = file_url,
          target = "_blank",
          rel = "noopener noreferrer",
          icon("arrow-up-right-from-square"), " ", file_name
        )
      } else {
        tagList(file_name, tags$span(class = "text-muted", " (URL unavailable)"))
      }
    )
  })

  tooltip_ui <- NULL
  if (!is.null(tooltip_html) && length(tooltip_html) > 0 && !is.na(tooltip_html[[1]])) {
    tooltip_text <- as.character(tooltip_html[[1]])
    tooltip_text <- gsub("<br\\s*/?>", "\n", tooltip_text, ignore.case = TRUE)
    tooltip_text <- gsub("<[^>]+>", "", tooltip_text)
    tooltip_lines <- trimws(strsplit(tooltip_text, "\n", fixed = TRUE)[[1]])
    tooltip_lines <- tooltip_lines[nzchar(tooltip_lines)]
    if (length(tooltip_lines) > 0) {
      tooltip_ui <- div(
        class = "plot-source-tooltip",
        lapply(tooltip_lines, function(line) div(line))
      )
    }
  }

  div(
    class = "plot-source-details",
    div(
      class = "plot-source-details-header",
      h5(icon("circle-info"), " Selected result"),
      if (!is.null(source)) {
        tags$button(
          type = "button",
          class = "plot-source-close",
          title = "Close selected result",
          `aria-label` = "Close selected result",
          onclick = sprintf(
            paste0(
              "if (window.clearDashboardSourceSelection) {",
              "window.clearDashboardSourceSelection('%s');",
              "} else {Shiny.setInputValue('clear_source_%s', Date.now(), {priority: 'event'});}"
            ),
            source, source
          ),
          icon("xmark", `aria-hidden` = "true")
        )
      }
    ),
    div(
      class = "plot-source-toggle-hint",
      "Click the selected point again, empty plot space, or × to clear this panel."
    ),
    tooltip_ui,
    if (is.null(tooltip_ui)) {
      p(
        class = "plot-source-context",
        paste0(
          record$current_name[[1]], " · ", as.character(record$grade_level[[1]]),
          " · Spring ", record$spring_year[[1]]
        )
      )
    },
    h5(class = "plot-source-files-title", icon("file-lines"), " Source files"),
    tags$ul(source_items)
  )
}

plottable_observation_ids <- function(current_data) {
  if (nrow(current_data) == 0 || !"observation_id" %in% names(current_data)) {
    return(integer())
  }
  if ("value" %in% names(current_data)) {
    current_data <- current_data |>
      filter(!is.na(.data$value))
  }
  unique(current_data$observation_id[!is.na(current_data$observation_id)])
}

source_details_for_observation <- function(observation_id, current_data, source = NULL) {
  if (is.na(observation_id) || nrow(current_data) == 0 ||
      !"observation_id" %in% names(current_data)) {
    return(source_panel_placeholder())
  }
  if ("value" %in% names(current_data)) {
    current_data <- current_data |>
      filter(!is.na(.data$value))
  }
  clicked <- current_data |>
    filter(.data$observation_id == .env$observation_id) |>
    slice(1)
  if (nrow(clicked) == 0) return(source_panel_placeholder())
  tooltip_html <- if ("hover_text" %in% names(clicked)) clicked$hover_text[[1]] else NULL
  source_details_panel(observation_id, tooltip_html, source)
}

add_unique_chart_labels <- function(data, label_column = "choice_label") {
  label_map <- data |>
    transmute(
      organization_key = as.character(organization_key),
      base_label = as.character(.data[[label_column]])
    ) |>
    distinct()
  duplicated_labels <- label_map |>
    count(base_label, name = "key_count") |>
    filter(key_count > 1) |>
    pull(base_label)
  label_map <- label_map |>
    mutate(
      chart_label = if_else(
        base_label %in% duplicated_labels,
        paste0(base_label, " [", organization_key, "]"),
        base_label
      )
    ) |>
    select(organization_key, chart_label)
  data |>
    mutate(organization_key = as.character(organization_key)) |>
    left_join(label_map, by = "organization_key")
}

server <- function(input, output, session) {
  color_registry <- reactiveVal(stats::setNames(character(), character()))
  benchmark_keys <- reactiveVal("state:oregon")

  make_source_selection <- function(source) {
    selection <- reactiveVal(NA_integer_)
    observeEvent(input[[paste0("plotly_click-", source)]], {
      click <- plotly_click_payload(input, source)
      if (is.null(click) || is.null(click$key) || length(click$key) == 0) return()
      observation_id <- suppressWarnings(as.integer(unlist(click$key)[[1]]))
      if (is.na(observation_id)) return()
      current <- isolate(selection())
      selection(if (!is.na(current) && identical(current, observation_id)) NA_integer_ else observation_id)
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("clear_source_", source)]], {
      selection(NA_integer_)
    }, ignoreInit = TRUE)
    selection
  }

  clear_invalid_source_selection <- function(selection, data_reactive) {
    observe({
      observation_id <- selection()
      if (is.na(observation_id)) return()
      if (!observation_id %in% plottable_observation_ids(data_reactive())) {
        selection(NA_integer_)
      }
    })
  }

  trend_source_selection <- make_source_selection("trend")
  population_source_selection <- make_source_selection("population")
  ranking_source_selection <- make_source_selection("ranking")
  heatmap_source_selection <- make_source_selection("rank_heatmap")
  cohort_source_selection <- make_source_selection("cohort")

  trend_state <- reactive({
    req(input$student_group, input$subject, input$trend_metric)
    list(
      student_group = input$student_group,
      subject = input$subject,
      organizations = input$organizations,
      metric = validate_metric(input$trend_metric),
      grades = input$trend_grades,
      labels = isTRUE(input$trend_labels)
    )
  }) |>
    debounce(250)
  population_state <- reactive({
    req(input$subject)
    list(
      subject = input$subject,
      organizations = input$organizations,
      student_groups = input$population_groups
    )
  }) |>
    debounce(250)
  cohort_state <- reactive({
    req(input$student_group, input$subject, input$cohort_metric)
    list(
      student_group = input$student_group,
      subject = input$subject,
      organizations = input$organizations,
      metric = validate_metric(input$cohort_metric),
      cohorts = suppressWarnings(as.integer(input$cohort_classes)),
      grades = input$cohort_grades,
      labels = isTRUE(input$cohort_labels)
    )
  }) |>
    debounce(250)

  observeEvent(input$organizations, {
    selected <- input$organizations
    if (length(selected) > 8) {
      keep <- selected[seq_len(8)]
      updateVirtualSelect("organizations", selected = keep, session = session)
      showNotification(
        "Dashboard comparisons are limited to eight total organizations, including benchmarks.",
        type = "warning",
        duration = 6
      )
    }
    benchmark_keys(unique(c("state:oregon", intersect(benchmark_keys(), selected))))
  }, ignoreNULL = TRUE)

  observeEvent(input$add_benchmarks, {
    selected <- input$organizations
    if (is.null(selected)) selected <- character()

    selected_schools <- osas_organizations |>
      filter(organization_key %in% selected, organization_type == "School")
    additions <- unique(c(
      "state:oregon",
      selected_schools$current_district_key[!is.na(selected_schools$current_district_key)]
    ))
    proposed <- unique(c(selected, additions))

    if (length(proposed) > 8) {
      showNotification(
        paste0(
          "Adding Oregon and all current parent districts would create ", length(proposed),
          " organizations. Remove selections until the total is eight or fewer."
        ),
        type = "warning",
        duration = 7
      )
      return()
    }

    benchmark_keys(unique(c(benchmark_keys(), additions)))

    if (setequal(proposed, selected)) {
      showNotification(
        if (nrow(selected_schools) == 0) {
          "Oregon is already selected. Select a school to add its current parent district."
        } else {
          "Oregon and the selected schools' current parent districts are already selected."
        },
        type = "message",
        duration = 5
      )
      return()
    }

    updateVirtualSelect("organizations", selected = proposed, session = session)
  })

  observeEvent(input$reset_filters, {
    updatePickerInput(session, "student_group", selected = dashboard_default_group)
    updateVirtualSelect(
      "population_groups",
      selected = dashboard_default_population_groups,
      session = session
    )
    updatePickerInput(session, "subject", selected = dashboard_default_subject)
    updateVirtualSelect("organizations", selected = "state:oregon", session = session)
    updatePickerInput(session, "trend_metric", selected = "percent_proficient")
    updateVirtualSelect("trend_grades", selected = dashboard_default_grades, session = session)
    updateMaterialSwitch(session, "trend_labels", value = FALSE)
    updateMaterialSwitch(session, "show_participation", value = FALSE)
    updatePickerInput(session, "rank_year", selected = max(dashboard_spring_years))
    updateCheckboxGroupButtons(session, "rank_types", selected = dashboard_org_types)
    updatePickerInput(
      session,
      "rank_grade",
      selected = if ("Grade 5" %in% dashboard_grades) "Grade 5" else dashboard_grades[[1]]
    )
    updateSliderInput(session, "rank_top_n", value = 10)
    updateNumericInput(session, "rank_min", value = dashboard_participants_min)
    updateNumericInput(session, "rank_max", value = dashboard_participants_max)
    updateVirtualSelect("cohort_classes", selected = dashboard_default_cohorts, session = session)
    updatePickerInput(session, "cohort_metric", selected = "percent_proficient")
    updateVirtualSelect("cohort_grades", selected = dashboard_tested_grades, session = session)
    updateMaterialSwitch(session, "cohort_labels", value = FALSE)
    updateCheckboxGroupButtons(session, "coverage_types", selected = dashboard_org_types)
    benchmark_keys("state:oregon")
    showNotification("Dashboard filters reset.", type = "message", duration = 3)
  })

  output$trend_selection_summary <- renderUI({
    state <- trend_state()
    div(
      class = "selection-summary",
      paste(
        dashboard_metric_labels[[state$metric]], "for", state$student_group, "in",
        state$subject, "—", length(state$organizations), "organization(s),",
        length(state$grades), "grade panel(s)."
      )
    )
  })

  trend_data <- reactive({
    state <- trend_state()
    data <- osas_records |>
      filter(
        as.character(student_group) == state$student_group,
        as.character(subject) == state$subject,
        as.character(organization_key) %in% state$organizations,
        as.character(grade_level) %in% state$grades
      ) |>
      mutate(
        organization_key = as.character(organization_key),
        grade_level = as.character(grade_level),
        value = metric_values(pick(everything()), state$metric),
        participation_display = format_value_range(participation_min, participation_max, 1, "%"),
        participants_display = format_value_range(participants_min, participants_max, 0),
        data_flags = source_conflict_label(
          participants_min, participants_max, participation_min, participation_max
        )
      )

    if (nrow(data) == 0) return(data)

    data <- data |>
      left_join(source_names_for(data$observation_id), by = "observation_id") |>
      decorate_records() |>
      select(
        observation_id, spring_year, organization_key, grade_level, value,
        participants_display, participation_display, source_count, source_preview,
        historical_name, district_name, data_flags
      )

    data |>
      add_spring_year_gaps(year_range = seq(min(dashboard_spring_years), max(dashboard_spring_years))) |>
      left_join(
        osas_organizations |>
          select(organization_key, current_name, choice_label, organization_type),
        by = "organization_key"
      ) |>
      add_unique_chart_labels() |>
      mutate(
        organization_type = as.character(organization_type),
        hover_text = if_else(
          is.na(value),
          paste0("<b>", choice_label, "</b><br>Spring ", spring_year, "<br>No reported value"),
          paste0(
            "<b>", choice_label, "</b><br>",
            state$subject, " · ", grade_level, "<br>",
            "Spring ", spring_year, "<br>",
            dashboard_metric_labels[[state$metric]], ": ", round(value, 1), "%<br>",
            "Students tested: ", participants_display, "<br>",
            "Participation: ", participation_display, "<br>",
            if_else(is.na(district_name), "", paste0("District: ", district_name, "<br>")),
            if_else(
              is.na(historical_name) | historical_name == current_name,
              "",
              paste0("Reported name: ", historical_name, "<br>")
            ),
            "Source conflict: ", data_flags, "<br>",
            format_source_hover(source_preview, source_count, click_hint = TRUE)
          )
        )
      )
  })

  clear_invalid_source_selection(trend_source_selection, trend_data)

  output$trend_plot_ui <- renderUI({
    panels <- max(1, length(input$trend_grades))
    columns <- if (panels <= 2) panels else if (panels <= 6) 3 else 4
    initial_height <- max(420, ceiling(panels / columns) * 250)

    tagList(
      jqui_resizable(
        div(
          class = "resizable-plot",
          style = paste0("height:", initial_height, "px;"),
          plotlyOutput("trend_plot", height = "100%")
        ),
        options = list(
          handles = "s",
          minHeight = 420,
          maxHeight = 1600,
          create = JS(
            "function(event, ui) {",
            "  var $element = $(event.target);",
            "  var $handle = $element.find('.ui-resizable-s');",
            "  $handle",
            "    .attr('title', 'Drag to resize plot height')",
            "    .attr('aria-label', 'Resize plot height')",
            "    .attr('aria-orientation', 'vertical')",
            "    .attr('aria-valuemin', '420')",
            "    .attr('aria-valuemax', '1600')",
            "    .attr('aria-valuenow', Math.round($element.height()))",
            "    .attr('role', 'slider')",
            "    .attr('tabindex', '0')",
            "    .on('keydown', function(e) {",
            "      if (!['ArrowUp', 'ArrowDown', '+', '-'].includes(e.key)) return;",
            "      e.preventDefault();",
            "      var increase = e.key === 'ArrowUp' || e.key === '+';",
            "      var height = Math.max(420, Math.min(1600, $element.height() + (increase ? 50 : -50)));",
            "      $element.height(height);",
            "      $handle.attr('aria-valuenow', Math.round(height));",
            "      var plot = $element.find('.plotly')[0];",
            "      if (plot && window.Plotly) Plotly.Plots.resize(plot);",
            "    });",
            "}"
          ),
          resize = JS(
            "function(event, ui) {",
            "  ui.element.find('.ui-resizable-s').attr('aria-valuenow', Math.round(ui.size.height));",
            "  var plot = ui.element.find('.plotly')[0];",
            "  if (plot && window.Plotly) Plotly.Plots.resize(plot);",
            "}"
          ),
          stop = JS(
            "function(event, ui) {",
            "  var plot = ui.element.find('.plotly')[0];",
            "  if (plot && window.Plotly) Plotly.Plots.resize(plot);",
            "}"
          )
        )
      )
    )
  })

  output$trend_plot <- renderPlotly({
    state <- trend_state()
    data <- trend_data()
    validate(need(nrow(data) > 0 && any(!is.na(data$value)), "No outcome data match the current selections."))

    keys <- unique(data$organization_key)
    key_colors <- organization_color_values(keys, color_registry)
    label_colors <- stats::setNames(key_colors[data$organization_key[match(keys, data$organization_key)]], data$chart_label[match(keys, data$organization_key)])
    label_colors <- label_colors[!is.na(names(label_colors))]

    applied_benchmarks <- intersect(benchmark_keys(), keys)
    benchmark_labels <- data$chart_label[match(applied_benchmarks, data$organization_key)]
    if (length(benchmark_labels) > 0) {
      neutral_colors <- grDevices::gray.colors(length(benchmark_labels), start = 0.28, end = 0.58)
      label_colors[benchmark_labels] <- neutral_colors
    }
    line_types <- stats::setNames(
      ifelse(keys %in% applied_benchmarks, "22", "solid"),
      data$chart_label[match(keys, data$organization_key)]
    )
    label_shapes <- stats::setNames(
      unname(organization_type_shapes[data$organization_type[match(keys, data$organization_key)]]),
      data$chart_label[match(keys, data$organization_key)]
    )

    plot_width <- suppressWarnings(as.numeric(session$clientData$output_trend_plot_width))
    narrow_plot <- length(plot_width) == 1 && is.finite(plot_width) && plot_width < 720
    facet_columns <- if (narrow_plot) {
      min(2, length(state$grades))
    } else if (length(state$grades) <= 2) {
      length(state$grades)
    } else if (length(state$grades) <= 6) {
      3
    } else {
      4
    }
    facet_columns <- max(1, facet_columns)
    year_step <- if (facet_columns >= 4) 3 else if (facet_columns == 3) 2 else 1
    year_breaks <- sort(unique(c(
      seq(min(dashboard_spring_years), max(dashboard_spring_years), by = year_step),
      max(dashboard_spring_years)
    )))

    graph <- ggplot(
      data,
      aes(
        x = spring_year,
        y = value,
        color = chart_label,
        linetype = chart_label,
        shape = chart_label,
        group = organization_key,
        key = observation_id,
        text = hover_text
      )
    ) +
      annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf, fill = "#777777", alpha = 0.12) +
      annotate("text", x = 2020.5, y = 4, label = "No source records\nspring 2020–2021", size = 2.5, color = "#555555") +
      geom_line(linewidth = 0.75, na.rm = TRUE) +
      geom_point(size = 1.8, na.rm = TRUE) +
      # Give every facet its own Plotly axis pair. ggplotly can otherwise
      # combine axes across rows (for example x2 with y), leaving panels blank
      # when a grade selection changes the layout from one row to two.
      facet_wrap(vars(grade_level), ncol = facet_columns, scales = "free") +
      scale_color_manual(values = label_colors, drop = FALSE) +
      scale_linetype_manual(values = line_types, drop = FALSE) +
      scale_shape_manual(values = label_shapes, drop = FALSE) +
      scale_x_continuous(breaks = year_breaks) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 100, 20)) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(
        x = "Spring testing year",
        y = dashboard_metric_labels[[state$metric]],
        color = NULL,
        linetype = NULL,
        shape = NULL,
        subtitle = paste(state$student_group, "·", state$subject)
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 10))
      ) +
      dashboard_panel_theme()

    if (isTRUE(state$labels)) {
      endpoints <- data |>
        filter(!is.na(value)) |>
        group_by(organization_key, grade_level) |>
        slice_max(spring_year, n = 1, with_ties = FALSE) |>
        ungroup()
      graph <- graph + geom_text(
        data = endpoints,
        aes(label = paste0(round(value, 1), "%")),
        nudge_y = 3,
        size = 2.7,
        show.legend = FALSE,
        check_overlap = TRUE
      )
    }

    ggplotly(graph, tooltip = "text", dynamicTicks = FALSE, source = "trend") |>
      event_register("plotly_click") |>
      layout(legend = list(orientation = "h", x = 0, y = 1.08), margin = list(t = 70)) |>
      config(displaylogo = FALSE, responsive = TRUE)
  })

  output$trend_source_details <- renderUI({
    source_details_for_observation(trend_source_selection(), trend_data(), "trend")
  })

  output$population_selection_summary <- renderUI({
    state <- population_state()
    available <- population_data() |>
      filter(!is.na(observation_id), !is.na(value))
    div(
      class = "selection-summary",
      paste(
        "Published All Grades counts available for",
        length(unique(available$student_group)), "of", length(state$student_groups),
        "selected student population(s) in", state$subject, "across",
        length(unique(available$organization_key)), "organization(s)."
      )
    )
  })

  population_data <- reactive({
    state <- population_state()
    validate(need(length(state$student_groups) > 0, "Select at least one student population."))
    validate(need(length(state$organizations) > 0, "Select at least one organization."))

    data <- osas_records |>
      filter(
        as.character(subject) == state$subject,
        as.character(organization_key) %in% state$organizations,
        as.character(student_group) %in% state$student_groups,
        as.character(grade_level) == "All Grades"
      ) |>
      mutate(
        organization_key = as.character(organization_key),
        subject = as.character(subject),
        grade_level = as.character(grade_level),
        student_group = as.character(student_group),
        value = participants_min,
        participants_display = format_value_range(participants_min, participants_max, 0),
        participation_display = format_value_range(participation_min, participation_max, 1, "%"),
        data_flags = source_conflict_label(
          participants_min, participants_max, participation_min, participation_max
        )
      )

    if (nrow(data) == 0) return(data)

    data <- data |>
      left_join(source_names_for(data$observation_id), by = "observation_id") |>
      decorate_records() |>
      select(
        observation_id, spring_year, organization_key, subject, grade_level,
        student_group, value, participants_min, participants_max,
        participants_display, participation_display, source_count, source_preview,
        historical_name, district_name, data_flags
      )

    data |>
      add_spring_year_gaps(year_range = seq(min(dashboard_spring_years), max(dashboard_spring_years))) |>
      left_join(
        osas_organizations |>
          select(organization_key, current_name, choice_label, organization_type),
        by = "organization_key"
      ) |>
      add_unique_chart_labels() |>
      mutate(
        organization_type = as.character(organization_type),
        hover_text = if_else(
          is.na(value),
          paste0(
            "<b>", choice_label, "</b><br>", student_group,
            "<br>Spring ", spring_year, "<br>No published All Grades count"
          ),
          paste0(
            "<b>", choice_label, "</b><br>",
            student_group, " · ", state$subject, "<br>",
            "Spring ", spring_year, "<br>",
            "Students tested: ", participants_display, "<br>",
            "Participation: ", participation_display, "<br>",
            if_else(is.na(district_name), "", paste0("District: ", district_name, "<br>")),
            if_else(
              is.na(historical_name) | historical_name == current_name,
              "",
              paste0("Reported name: ", historical_name, "<br>")
            ),
            "Source conflict: ", data_flags, "<br>",
            format_source_hover(source_preview, source_count, click_hint = TRUE)
          )
        )
      )
  })

  clear_invalid_source_selection(population_source_selection, population_data)

  output$population_plot_ui <- renderUI({
    organization_count <- population_data() |>
      filter(!is.na(observation_id), !is.na(value)) |>
      distinct(organization_key) |>
      nrow() |>
      max(1)
    plotlyOutput(
      "population_plot",
      height = paste0(max(500, ceiling(organization_count / 2) * 300), "px")
    )
  })

  output$population_plot <- renderPlotly({
    state <- population_state()
    data <- population_data()
    validate(need(nrow(data) > 0 && any(!is.na(data$value)), "No published All Grades counts match the current selections."))

    group_levels <- state$student_groups[state$student_groups %in% unique(data$student_group)]
    group_colors <- population_group_colors[group_levels]
    group_shapes <- population_group_shapes[group_levels]
    organization_count <- length(unique(data$organization_key))
    facet_columns <- if (organization_count <= 2) organization_count else 2

    graph <- ggplot(
      data,
      aes(
        x = spring_year,
        y = value,
        color = student_group,
        shape = student_group,
        group = interaction(organization_key, student_group),
        key = observation_id,
        text = hover_text
      )
    ) +
      annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf, fill = "#777777", alpha = 0.12) +
      annotate(
        "text", x = 2020.5, y = Inf, label = "No source records\n2020–2021",
        vjust = 1.25, size = 2.6, color = "#555555"
      ) +
      geom_line(linewidth = 0.8, na.rm = TRUE) +
      geom_point(size = 2, na.rm = TRUE) +
      facet_wrap(vars(chart_label), ncol = max(1, facet_columns), scales = "free_y") +
      scale_color_manual(values = group_colors, drop = FALSE) +
      scale_shape_manual(values = group_shapes, drop = FALSE) +
      scale_x_continuous(breaks = seq(min(dashboard_spring_years), max(dashboard_spring_years))) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = "Spring testing year",
        y = "Students tested",
        color = "Student population",
        shape = "Student population",
        subtitle = paste(state$subject, "· Published All Grades records")
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 10))
      ) +
      dashboard_panel_theme()

    ggplotly(graph, tooltip = "text", dynamicTicks = FALSE, source = "population") |>
      event_register("plotly_click") |>
      layout(legend = list(orientation = "h", x = 0, y = 1.08), margin = list(t = 70)) |>
      config(displaylogo = FALSE, responsive = TRUE)
  })

  output$population_source_details <- renderUI({
    source_details_for_observation(
      population_source_selection(),
      population_data(),
      "population"
    )
  })

  output$population_table <- renderDT({
    display <- population_data() |>
      filter(!is.na(observation_id), !is.na(value)) |>
      transmute(
        Organization = chart_label,
        `Student Population` = student_group,
        `Spring Year` = spring_year,
        `Students Tested` = participants_display
      ) |>
      arrange(Organization, `Student Population`, `Spring Year`) |>
      pivot_wider(names_from = `Spring Year`, values_from = `Students Tested`)

    validate(need(nrow(display) > 0, "No published All Grades counts match the current selections."))
    datatable(
      display,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  participation_data <- reactive({
    state <- trend_state()
    data <- osas_records |>
      filter(
        as.character(student_group) == state$student_group,
        as.character(subject) == state$subject,
        as.character(organization_key) %in% state$organizations,
        as.character(grade_level) %in% state$grades
      ) |>
      mutate(
        organization_key = as.character(organization_key),
        grade_level = as.character(grade_level),
        value = if_else(participation_min == participation_max, participation_min, NA_real_),
        rate_display = format_value_range(participation_min, participation_max, 1, "%")
      ) |>
      select(spring_year, organization_key, grade_level, value, rate_display)
    if (nrow(data) == 0) return(data)
    data |>
      add_spring_year_gaps(year_range = seq(min(dashboard_spring_years), max(dashboard_spring_years))) |>
      left_join(
        osas_organizations |>
          select(organization_key, choice_label),
        by = "organization_key"
      ) |>
      add_unique_chart_labels() |>
      mutate(
        hover_text = paste0(
          "<b>", choice_label, "</b><br>", grade_level, " · Spring ", spring_year,
          "<br>Participation: ", if_else(is.na(rate_display), "Not available", rate_display)
        )
      )
  })

  output$participation_plot_ui <- renderUI({
    panels <- max(1, length(trend_state()$grades))
    plotlyOutput("participation_plot", height = paste0(max(380, ceiling(panels / 2) * 220), "px"))
  })

  output$participation_plot <- renderPlotly({
    data <- participation_data()
    validate(need(nrow(data) > 0 && any(!is.na(data$value)), "No participation-rate data match the current selections."))
    keys <- unique(data$organization_key)
    colors <- organization_color_values(keys, color_registry)
    label_colors <- stats::setNames(colors, data$chart_label[match(keys, data$organization_key)])

    graph <- ggplot(
      data,
      aes(
        spring_year, value, color = chart_label, group = organization_key,
        text = hover_text
      )
    ) +
      annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf, fill = "#777777", alpha = 0.12) +
      geom_line(linewidth = 0.7, na.rm = TRUE) +
      geom_point(size = 1.5, na.rm = TRUE) +
      facet_wrap(vars(grade_level), ncol = 2, scales = "free") +
      scale_color_manual(values = label_colors) +
      scale_x_continuous(breaks = seq(min(dashboard_spring_years), max(dashboard_spring_years), by = 1)) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 100, 20)) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(x = "Spring testing year", y = "Participation rate", color = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top", panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      dashboard_panel_theme()

    ggplotly(graph, tooltip = "text") |>
      config(displaylogo = FALSE, responsive = TRUE)
  })

  ranking_context <- reactive({
    req(input$rank_year, input$rank_grade, input$student_group, input$subject)
    validate(need(length(input$rank_types) > 0, "Select at least one organization type."))
    bounds <- safe_bounds(input$rank_min, input$rank_max)

    ranked <- compute_rankings(
      osas_records,
      spring_year = as.integer(input$rank_year),
      subject = input$subject,
      grade_level = input$rank_grade,
      student_group = input$student_group,
      organization_types = input$rank_types,
      participants_min = bounds[[1]],
      participants_max = bounds[[2]]
    ) |>
      decorate_records()

    top_n <- as.integer(input$rank_top_n)
    if (nrow(ranked) == 0) {
      top <- ranked
    } else {
      cutoff_row <- min(top_n, nrow(ranked))
      cutoff_rank <- ranked$overall_rank[[cutoff_row]]
      top <- ranked |>
        filter(overall_rank <= cutoff_rank)
    }

    list(ranked = ranked, top = top, bounds = bounds)
  }) |>
    debounce(250)

  output$rank_eligible_card <- renderUI({
    context <- ranking_context()
    metric_card(
      "Eligible organizations",
      format(nrow(context$ranked), big.mark = ","),
      paste(input$rank_types, collapse = " · ")
    )
  })

  output$rank_top_card <- renderUI({
    context <- ranking_context()
    if (nrow(context$ranked) == 0) {
      return(metric_card("Highest result", "No data", "Adjust ranking filters"))
    }
    top <- context$ranked[1, ]
    metric_card(
      "Highest result",
      paste0(round(top$percent_proficient, 1), "%"),
      paste(top$current_name, "·", top$organization_type)
    )
  })

  output$rank_oregon_card <- renderUI({
    context <- ranking_context()
    value <- unique(context$ranked$oregon_proficiency)
    value <- value[!is.na(value)]
    metric_card(
      "Oregon comparison",
      if (length(value) == 0) "Unavailable" else paste0(round(value[[1]], 1), "%"),
      paste("Spring", input$rank_year, "·", input$rank_grade)
    )
  })

  ranking_plot_data <- reactive({
    top <- ranking_context()$top
    if (nrow(top) == 0) return(top)

    top |>
      left_join(source_names_for(top$observation_id), by = "observation_id") |>
      add_unique_chart_labels() |>
      mutate(
        chart_label = factor(chart_label, levels = rev(unique(chart_label))),
        data_flags = source_conflict_label(
          participants_min, participants_max, participation_min, participation_max
        ),
        hover_text = paste0(
          "<b>", choice_label, "</b><br>",
          "Overall rank ", overall_rank, " of ", overall_n, "<br>",
          organization_type, " rank ", type_rank, " of ", type_n, "<br>",
          "Proficient: ", round(percent_proficient, 1), "%<br>",
          "Overall percentile: ", round(overall_percentile, 1), "<br>",
          "Type percentile: ", round(type_percentile, 1), "<br>",
          "Students tested: ", format_value_range(participants_min, participants_max, 0), "<br>",
          "Participation: ", format_value_range(participation_min, participation_max, 1, "%"), "<br>",
          if_else(is.na(current_district_name), "", paste0("District: ", current_district_name, "<br>")),
          "Difference from Oregon: ", if_else(
            is.na(difference_from_oregon), "Comparison unavailable",
            paste0(if_else(difference_from_oregon > 0, "+", ""), round(difference_from_oregon, 1), " points")
          ), "<br>Source conflict: ", data_flags,
          "<br>", format_source_hover(source_preview, source_count, click_hint = TRUE)
        )
      )
  })

  clear_invalid_source_selection(ranking_source_selection, ranking_plot_data)

  output$ranking_plot <- renderPlotly({
    top <- ranking_plot_data()
    validate(need(nrow(top) > 0, "No organizations meet the ranking filters."))

    graph <- ggplot(
      top,
      aes(
        x = percent_proficient,
        y = chart_label,
        color = organization_type,
        shape = organization_type,
        key = observation_id,
        text = hover_text
      )
    ) +
      geom_segment(aes(x = 0, xend = percent_proficient, yend = chart_label), color = "#d9dee3", linewidth = 0.8) +
      geom_point(size = 4) +
      scale_color_manual(values = organization_type_colors) +
      scale_shape_manual(values = organization_type_shapes) +
      scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      labs(x = "Percent proficient", y = NULL, color = "Type", shape = "Type") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank(), legend.position = "top") +
      dashboard_panel_theme()

    ggplotly(graph, tooltip = "text", source = "ranking") |>
      event_register("plotly_click") |>
      layout(margin = list(l = 220)) |>
      config(displaylogo = FALSE, responsive = TRUE)
  })

  output$ranking_source_details <- renderUI({
    source_details_for_observation(ranking_source_selection(), ranking_plot_data(), "ranking")
  })

  output$rank_distribution <- renderPlotly({
    req(input$rank_year, input$rank_grade, input$student_group, input$subject)
    validate(need(length(input$rank_types) > 0, "Select at least one organization type."))
    bounds <- safe_bounds(input$rank_min, input$rank_max)

    data <- osas_records |>
      filter(
        spring_year == as.integer(input$rank_year),
        as.character(subject) == input$subject,
        as.character(grade_level) == input$rank_grade,
        as.character(student_group) == input$student_group,
        as.character(organization_type) %in% input$rank_types,
        !is.na(percent_proficient),
        !is.na(participants_min),
        participants_min > 0
      ) |>
      mutate(in_range = participants_min >= bounds[[1]] & participants_min <= bounds[[2]])

    validate(need(nrow(data) > 0, "No student-count distribution is available."))
    use_log <- max(data$participants_min, na.rm = TRUE) / min(data$participants_min, na.rm = TRUE) > 10

    graph <- ggplot(data, aes(x = participants_min, fill = in_range)) +
      geom_histogram(bins = 28, color = "white", linewidth = 0.25) +
      scale_fill_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "#c8cdd2"), guide = "none") +
      labs(x = if (use_log) "Students tested (log scale)" else "Students tested", y = "Organizations") +
      theme_minimal(base_size = 11) +
      dashboard_panel_theme()
    if (use_log) graph <- graph + scale_x_log10(labels = scales::label_number(big.mark = ","))

    ggplotly(graph) |>
      config(displaylogo = FALSE, responsive = TRUE)
  })

  pinned_rows <- reactive({
    context <- ranking_context()
    pins <- input$organizations
    if (length(pins) == 0) return(context$ranked[0, , drop = FALSE])

    outside_top <- context$ranked |>
      filter(organization_key %in% pins, !organization_key %in% context$top$organization_key) |>
      mutate(result_section = "Eligible pinned comparison")

    base_pins <- osas_records |>
      filter(
        spring_year == as.integer(input$rank_year),
        as.character(subject) == input$subject,
        as.character(grade_level) == input$rank_grade,
        as.character(student_group) == input$student_group,
        as.character(organization_key) %in% pins,
        !is.na(percent_proficient)
      ) |>
      decorate_records()

    missing_keys <- setdiff(pins, base_pins$organization_key)
    missing_rows <- osas_organizations |>
      filter(organization_key %in% missing_keys) |>
      transmute(
        observation_id = NA_integer_,
        organization_key,
        organization_type,
        current_name,
        current_district_name,
        overall_rank = NA_integer_,
        overall_percentile = NA_real_,
        overall_n = nrow(context$ranked),
        type_rank = NA_integer_,
        type_percentile = NA_real_,
        type_n = NA_integer_,
        percent_proficient = NA_real_,
        oregon_proficiency = NA_real_,
        difference_from_oregon = NA_real_,
        participants_min = NA_real_,
        participants_max = NA_real_,
        participation_min = NA_real_,
        participation_max = NA_real_,
        result_section = "No matching result"
      )

    ineligible <- base_pins |>
      filter(!organization_key %in% context$ranked$organization_key) |>
      mutate(
        overall_rank = NA_integer_,
        overall_percentile = NA_real_,
        overall_n = nrow(context$ranked),
        type_rank = NA_integer_,
        type_percentile = NA_real_,
        type_n = NA_integer_,
        oregon_proficiency = NA_real_,
        difference_from_oregon = NA_real_,
        result_section = case_when(
          !organization_type %in% input$rank_types ~ "Type excluded",
          participants_min < context$bounds[[1]] | participants_min > context$bounds[[2]] ~ "Not rank-eligible",
          TRUE ~ "Not rank-eligible"
        )
      )

    bind_rows(outside_top, ineligible, missing_rows) |>
      distinct(organization_key, .keep_all = TRUE)
  })

  rank_table_data <- reactive({
    top <- ranking_context()$top |>
      mutate(result_section = "Top result")
    combined <- bind_rows(top, pinned_rows()) |>
      distinct(organization_key, .keep_all = TRUE)
    if (nrow(combined) == 0) return(combined)

    source_links <- source_links_for(
      osas_sources,
      combined$observation_id[!is.na(combined$observation_id)]
    )

    combined |>
      left_join(source_links, by = "observation_id") |>
      mutate(
        data_flags = if_else(
          result_section == "No matching result",
          "Not applicable",
          source_conflict_label(
            participants_min, participants_max, participation_min, participation_max
          )
        )
      ) |>
      transmute(
        Result = result_section,
        Type = organization_type,
        Organization = current_name,
        District = current_district_name,
        `Overall Rank` = overall_rank,
        `Type Rank` = type_rank,
        `Overall Percentile` = overall_percentile,
        `Type Percentile` = type_percentile,
        `Percent Proficient` = percent_proficient,
        `Oregon Difference` = difference_from_oregon,
        `Students Tested` = format_value_range(participants_min, participants_max, 0),
        Participation = format_value_range(participation_min, participation_max, 1, "%"),
        `Data Flags` = data_flags,
        Sources = source_links
      )
  })

  output$ranking_table <- renderDT({
    data <- rank_table_data()
    validate(need(nrow(data) > 0, "No ranking results or pinned comparisons match these selections."))

    datatable(
      data,
      rownames = FALSE,
      escape = setdiff(names(data), "Sources"),
      class = "compact stripe hover",
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE,
        order = list(list(4, "asc")),
        columnDefs = list(list(className = "dt-nowrap", targets = c(0, 1, 2, 10, 11, 12)))
      )
    ) |>
      formatRound(c("Overall Percentile", "Type Percentile", "Percent Proficient", "Oregon Difference"), digits = 1) |>
      formatStyle(
        "Result",
        target = "row",
        backgroundColor = styleEqual(
          c("Eligible pinned comparison", "Not rank-eligible", "Type excluded", "No matching result"),
          c("#fff6d6", "#fbe9e7", "#eeeeee", "#fbe9e7")
        )
      )
  }, server = FALSE)

  grade_heatmap_data <- reactive({
    context <- ranking_context()
    row_keys <- unique(c(context$top$organization_key, input$organizations))
    if (length(row_keys) == 0) return(osas_records[0, , drop = FALSE])

    data <- osas_records |>
      filter(
        spring_year == as.integer(input$rank_year),
        as.character(subject) == input$subject,
        as.character(student_group) == input$student_group,
        as.character(organization_key) %in% row_keys,
        !is.na(grade_num),
        !is.na(percent_proficient)
      ) |>
      decorate_records()

    data <- data |>
      left_join(source_names_for(data$observation_id), by = "observation_id") |>
      add_unique_chart_labels()

    state_values <- osas_records |>
      filter(
        spring_year == as.integer(input$rank_year),
        as.character(subject) == input$subject,
        as.character(student_group) == input$student_group,
        as.character(organization_type) == "State",
        !is.na(grade_num),
        !is.na(percent_proficient)
      ) |>
      transmute(grade_level = as.character(grade_level), oregon_proficiency = percent_proficient) |>
      distinct(grade_level, .keep_all = TRUE)

    data <- data |>
      mutate(grade_level = as.character(grade_level)) |>
      left_join(state_values, by = "grade_level") |>
      mutate(
        difference = percent_proficient - oregon_proficiency,
        row_label = chart_label,
        data_flags = source_conflict_label(
          participants_min, participants_max, participation_min, participation_max
        ),
        hover_text = paste0(
          "<b>", choice_label, "</b><br>", grade_level, " · Spring ", spring_year, "<br>",
          "Proficient: ", round(percent_proficient, 1), "%<br>",
          "Oregon difference: ", if_else(
            is.na(difference), "Comparison unavailable",
            paste0(if_else(difference > 0, "+", ""), round(difference, 1), " points")
          ), "<br>",
          "Students tested: ", format_value_range(participants_min, participants_max, 0), "<br>",
          "Participation: ", format_value_range(participation_min, participation_max, 1, "%"), "<br>",
          if_else(is.na(current_district_name), "", paste0("District: ", current_district_name, "<br>")),
          "Source conflict: ", data_flags, "<br>",
          format_source_hover(source_preview, source_count, click_hint = TRUE)
        )
      )

    ranked_key_order <- context$top$organization_key
    order_labels <- data |>
      distinct(organization_key, chart_label) |>
      mutate(
        row_order = match(organization_key, ranked_key_order),
        row_order = if_else(is.na(row_order), length(ranked_key_order) + row_number(), row_order)
      ) |>
      arrange(row_order) |>
      pull(chart_label)
    data$row_label <- factor(data$row_label, levels = rev(order_labels))
    data$grade_level <- factor(data$grade_level, levels = dashboard_tested_grades)

    data
  })

  clear_invalid_source_selection(heatmap_source_selection, grade_heatmap_data)

  output$grade_heatmap <- renderPlotly({
    data <- grade_heatmap_data()
    validate(need(nrow(data) > 0, "No grade-specific data are available for these organizations."))

    graph <- ggplot(
      data,
      aes(
        x = grade_level, y = row_label, fill = difference,
        key = observation_id, text = hover_text
      )
    ) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient2(
        low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0,
        na.value = "#d9d9d9", name = "Points vs.\nOregon"
      ) +
      labs(x = "Tested grade", y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)) +
      dashboard_panel_theme()

    heatmap <- ggplotly(graph, tooltip = "text", source = "rank_heatmap") |>
      event_register("plotly_click") |>
      layout(margin = list(l = 240)) |>
      config(displaylogo = FALSE, responsive = TRUE) |>
      plotly_build()
    heatmap_trace_indexes <- which(vapply(
      heatmap$x$data,
      function(trace) identical(trace$type, "heatmap"),
      logical(1)
    ))
    for (trace_index in heatmap_trace_indexes) {
      trace <- heatmap$x$data[[trace_index]]
      x_indexes <- suppressWarnings(as.integer(trace$x))
      y_indexes <- suppressWarnings(as.integer(trace$y))
      x_values <- if (is.factor(data$grade_level) && all(!is.na(x_indexes))) {
        levels(data$grade_level)[x_indexes]
      } else {
        as.character(trace$x)
      }
      y_values <- if (is.factor(data$row_label) && all(!is.na(y_indexes))) {
        levels(data$row_label)[y_indexes]
      } else {
        as.character(trace$y)
      }
      trace$key <- lapply(y_values, function(y_value) {
        lapply(x_values, function(x_value) {
          matching_key <- data$observation_id[
            as.character(data$grade_level) == x_value &
              as.character(data$row_label) == y_value
          ]
          if (length(matching_key) == 0) NA_integer_ else matching_key[[1]]
        })
      })
      trace$`_isNestedKey` <- TRUE
      heatmap$x$data[[trace_index]] <- trace
    }

    heatmap
  })

  output$heatmap_source_details <- renderUI({
    source_details_for_observation(heatmap_source_selection(), grade_heatmap_data(), "rank_heatmap")
  })

  output$cohort_selection_summary <- renderUI({
    state <- cohort_state()
    div(
      class = "selection-summary",
      paste(
        dashboard_metric_labels[[state$metric]], "for", state$student_group, "in",
        state$subject, "— classes", paste(state$cohorts, collapse = ", "), "."
      )
    )
  })

  cohort_data <- reactive({
    state <- cohort_state()
    data <- osas_records |>
      filter(
        as.character(student_group) == state$student_group,
        as.character(subject) == state$subject,
        as.character(organization_key) %in% state$organizations,
        as.character(grade_level) %in% state$grades,
        !is.na(grade_num)
      ) |>
      mutate(
        organization_key = as.character(organization_key),
        class_of = spring_year + (12L - grade_num),
        value = metric_values(pick(everything()), state$metric),
        participants_display = format_value_range(participants_min, participants_max, 0),
        participation_display = format_value_range(participation_min, participation_max, 1, "%"),
        data_flags = source_conflict_label(
          participants_min, participants_max, participation_min, participation_max
        )
      ) |>
      filter(class_of %in% state$cohorts)

    if (nrow(data) == 0) return(data)

    data <- data |>
      left_join(source_names_for(data$observation_id), by = "observation_id") |>
      decorate_records() |>
      select(
        observation_id, organization_key, class_of, subject, grade_num,
        value, participants_display, participation_display, source_count, source_preview,
        historical_name, district_name, data_flags
      )
    valid_grades <- osas_records |>
      filter(as.character(subject) == state$subject, !is.na(grade_num)) |>
      distinct(grade_num) |>
      arrange(grade_num) |>
      pull(grade_num)

    data |>
      add_cohort_covid_gaps(valid_grades = valid_grades) |>
      left_join(
        osas_organizations |>
          select(organization_key, current_name, choice_label, organization_type),
        by = "organization_key"
      ) |>
      add_unique_chart_labels() |>
      mutate(
        hover_text = if_else(
          is.na(value),
          paste0(
            "<b>", choice_label, "</b><br>Class of ", class_of,
            " · Grade ", grade_num, "<br>No reported value"
          ),
          paste0(
            "<b>", choice_label, "</b><br>Class of ", class_of,
            " · Grade ", grade_num, "<br>",
            "Spring ", derived_spring_year, "<br>",
            dashboard_metric_labels[[state$metric]], ": ", round(value, 1), "%<br>",
            "Students tested: ", participants_display, "<br>",
            "Participation: ", participation_display, "<br>",
            if_else(is.na(district_name), "", paste0("District: ", district_name, "<br>")),
            if_else(
              is.na(historical_name) | historical_name == current_name,
              "",
              paste0("Reported name: ", historical_name, "<br>")
            ),
            "Source conflict: ", data_flags, "<br>",
            format_source_hover(source_preview, source_count, click_hint = TRUE)
          )
        )
      )
  })

  clear_invalid_source_selection(cohort_source_selection, cohort_data)

  output$cohort_plot_ui <- renderUI({
    panels <- max(1, length(cohort_state()$cohorts))
    plotlyOutput("cohort_plot", height = paste0(max(460, ceiling(panels / 2) * 270), "px"))
  })

  output$cohort_plot <- renderPlotly({
    state <- cohort_state()
    data <- cohort_data()
    validate(need(nrow(data) > 0 && any(!is.na(data$value)), "No cohort data match the current selections."))

    keys <- unique(data$organization_key)
    colors <- organization_color_values(keys, color_registry)
    label_colors <- stats::setNames(colors, data$chart_label[match(keys, data$organization_key)])

    graph <- ggplot(
      data,
      aes(
        grade_num, value, color = chart_label,
        group = interaction(organization_key, class_of),
        key = observation_id, text = hover_text
      )
    ) +
      geom_line(linewidth = 0.75, na.rm = TRUE) +
      geom_point(size = 1.8, na.rm = TRUE) +
      facet_wrap(vars(class_of), ncol = 2, labeller = label_both) +
      scale_color_manual(values = label_colors) +
      scale_x_continuous(breaks = sort(unique(data$grade_num))) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 100, 20)) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(
        x = "Grade",
        y = dashboard_metric_labels[[state$metric]],
        color = NULL,
        subtitle = "Paths break where their derived testing year is spring 2020 or 2021."
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top", panel.grid.minor = element_blank(), strip.text = element_text(face = "bold")) +
      dashboard_panel_theme()

    if (isTRUE(state$labels)) {
      endpoints <- data |>
        filter(!is.na(value)) |>
        group_by(organization_key, class_of) |>
        slice_max(grade_num, n = 1, with_ties = FALSE) |>
        ungroup()
      graph <- graph + geom_text(
        data = endpoints,
        aes(label = paste0(round(value, 1), "%")),
        nudge_y = 3,
        size = 2.7,
        show.legend = FALSE,
        check_overlap = TRUE
      )
    }

    ggplotly(graph, tooltip = "text", source = "cohort") |>
      event_register("plotly_click") |>
      layout(legend = list(orientation = "h", x = 0, y = 1.08), margin = list(t = 75)) |>
      config(displaylogo = FALSE, responsive = TRUE)
  })

  output$cohort_source_details <- renderUI({
    source_details_for_observation(cohort_source_selection(), cohort_data(), "cohort")
  })

  coverage_data <- reactive({
    req(input$student_group)
    types <- input$coverage_types
    if (length(types) == 0) return(osas_coverage[0, , drop = FALSE])
    osas_coverage |>
      filter(student_group == input$student_group, organization_type %in% types) |>
      mutate(
        organization_type = factor(organization_type, levels = dashboard_org_types),
        subject = as.character(subject)
      )
  })

  output$coverage_plot <- renderPlotly({
    data <- coverage_data()
    validate(need(nrow(data) > 0, "No coverage data match these selections."))
    data <- data |>
      mutate(
        text_color = contrast_text_color(organization_count),
        hover_text = paste0(
          "<b>", organization_type, "</b><br>", subject, " · Spring ", spring_year,
          "<br>", format(organization_count, big.mark = ","), " distinct organizations"
        )
      )

    graph <- ggplot(
      data,
      aes(spring_year, subject, fill = organization_count, text = hover_text)
    ) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(
        aes(label = scales::comma(organization_count), color = text_color),
        size = 3,
        show.legend = FALSE
      ) +
      facet_wrap(vars(organization_type), ncol = 1, scales = "free_y") +
      scale_fill_gradient(low = "#f1eef6", high = "#54278f", labels = scales::comma) +
      scale_color_identity() +
      scale_x_continuous(breaks = seq(min(dashboard_spring_years), max(dashboard_spring_years), by = 1)) +
      labs(x = "Spring testing year", y = NULL, fill = "Organizations") +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 10))
      ) +
      dashboard_panel_theme()

    ggplotly(graph, tooltip = "text") |>
      config(displaylogo = FALSE, responsive = TRUE)
  })

  output$coverage_table <- renderDT({
    data <- coverage_data() |>
      mutate(spring_year = as.character(spring_year)) |>
      select(Type = organization_type, Subject = subject, spring_year, organization_count) |>
      pivot_wider(names_from = spring_year, values_from = organization_count, values_fill = 0) |>
      arrange(Type, Subject)

    datatable(
      data,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(paging = FALSE, searching = FALSE, info = FALSE, scrollX = TRUE)
    )
  }, server = FALSE)

  output$validation_table <- renderDT({
    display <- osas_validation |>
      mutate(
        section = tools::toTitleCase(gsub("_", " ", section)),
        metric = tools::toTitleCase(gsub("_", " ", metric)),
        value = format(as.numeric(value), big.mark = ",", scientific = FALSE)
      ) |>
      rename(Section = section, Metric = metric, Value = value)
    datatable(
      display,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(paging = FALSE, searching = FALSE, info = FALSE)
    )
  }, server = FALSE)

  conflict_report_data <- reactive({
    types <- input$coverage_types
    if (length(types) == 0) types <- dashboard_org_types
    conflicts <- osas_conflicts |>
      filter(
        student_group == input$student_group,
        organization_type %in% types
      ) |>
      decorate_records()
    if (nrow(conflicts) == 0) return(conflicts)

    source_html <- source_links_for(osas_sources, conflicts$observation_id)
    source_text <- osas_sources |>
      filter(observation_id %in% conflicts$observation_id) |>
      mutate(
        source_file = as.character(source_file),
        source_url = as.character(source_url),
        source_text = if_else(
          is.na(source_url) | !nzchar(trimws(source_url)),
          paste0(source_file, " | URL unavailable"),
          paste0(source_file, " | ", source_url)
        )
      ) |>
      group_by(observation_id) |>
      summarise(source_text = paste(unique(source_text), collapse = "; "), .groups = "drop")

    conflicts |>
      left_join(source_html, by = "observation_id") |>
      left_join(source_text, by = "observation_id") |>
      transmute(
        `Spring Year` = spring_year,
        Subject = subject,
        Grade = grade_level,
        `Student Group` = student_group,
        Type = organization_type,
        Organization = current_name,
        District = current_district_name,
        Field = tools::toTitleCase(gsub("_", " ", field)),
        `Reported Range` = format_value_range(value_min, value_max, 1),
        Sources = source_links,
        source_text
      )
  })

  output$conflict_table <- renderDT({
    data <- conflict_report_data()
    validate(need(nrow(data) > 0, "No field conflicts match the selected student population and organization types."))
    display <- data |>
      select(-source_text)
    datatable(
      display,
      rownames = FALSE,
      escape = setdiff(names(display), "Sources"),
      class = "compact stripe hover",
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  output$download_conflicts <- downloadHandler(
    filename = function() paste0("osas-source-conflicts-", Sys.Date(), ".csv"),
    content = function(file) {
      conflict_report_data() |>
        select(-Sources) |>
        rename(Sources = source_text) |>
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  output$data_metadata <- renderUI({
    built <- format(as.POSIXct(osas_metadata$built_at), "%B %d, %Y at %I:%M %p %Z")
    tags$dl(
      class = "coverage-meta",
      tags$dt("Source artifact"), tags$dd(osas_metadata$source_file),
      tags$dt("Prepared artifact"), tags$dd(basename(dashboard_data_path)),
      tags$dt("Prepared"), tags$dd(built),
      tags$dt("Canonical observations"), tags$dd(format(osas_metadata$canonical_rows, big.mark = ",")),
      tags$dt("Duplicate rows collapsed"), tags$dd(format(osas_metadata$duplicate_rows_removed, big.mark = ",")),
      tags$dt("Field conflicts retained"), tags$dd(format(osas_metadata$conflict_rows, big.mark = ",")),
      tags$dt("Source rows without a URL"), tags$dd(format(osas_metadata$missing_source_urls, big.mark = ",")),
      tags$dt("Source"),
      tags$dd(
        tags$a(
          href = "https://www.oregon.gov/ode/educator-resources/assessment/Pages/Assessment-Group-Reports.aspx",
          target = "_blank", rel = "noopener noreferrer", "ODE Assessment Group Reports"
        )
      )
    )
  })

  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("osas-canonical-", gsub("[^A-Za-z0-9]+", "-", input$student_group), ".csv")
    },
    content = function(file) {
      types <- input$coverage_types
      if (length(types) == 0) types <- dashboard_org_types
      export <- osas_records |>
        filter(
          as.character(student_group) == input$student_group,
          as.character(organization_type) %in% types
        ) |>
        decorate_records()

      source_export <- osas_sources |>
        filter(observation_id %in% export$observation_id) |>
        mutate(
          source_file = as.character(source_file),
          source_url = as.character(source_url),
          source_pair = if_else(
            is.na(source_url) | !nzchar(trimws(source_url)),
            paste0(source_file, " | URL unavailable"),
            paste0(source_file, " | ", source_url)
          )
        ) |>
        group_by(observation_id) |>
        summarise(sources = paste(unique(source_pair), collapse = "; "), .groups = "drop")

      export |>
        left_join(source_export, by = "observation_id") |>
        transmute(
          spring_year, subject = as.character(subject), grade_level = as.character(grade_level),
          student_group = as.character(student_group), organization_key,
          organization_type, organization_name = current_name,
          historical_name, district_name, participants_min, participants_max,
          participation_min, participation_max, percent_proficient,
          percent_level_1, percent_level_2, percent_level_3,
          percent_level_4, percent_level_5,
          participant_count_conflict = participants_min != participants_max,
          participation_rate_conflict = participation_min != participation_max,
          sources
        ) |>
        write.csv(file, row.names = FALSE, na = "")
    }
  )
}

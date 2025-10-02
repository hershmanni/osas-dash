## osas reports

server <- function(input, output, session) {
    output$osas.student.group.chooser <- renderUI ({
        student_groups <- dt.osas %>% distinct(student_group) %>% arrange(student_group) %>% pull(student_group)

        radioGroupButtons(inputId = 'student_groups.focus',
                          label = 'Select student_group focus',
                          choices = student_groups,
                          selected = "Total Population (All Students)"
                          )
    })

    output$osas.outcomes <- renderPlotly({
        my_subjects <- input$osas.subjects.focus
        my_group <- input$student_groups.focus
        my_organizations <- input$osas.organization.focus
        my_grades <- input$osas.grade.focus

        dt.osas %>% filter(subject %in% my_subjects,
                           organization %in% my_organizations,
                           student_group == my_group,
                           grade_level %in% my_grades) %>%
            mutate(subject.fix = case_when(subject == "English Language Arts" ~ "ELA",
                                           subject == "Mathematics" ~ "Math",
                                           subject == "Science" ~ "Science")) %>%            
            ggplot(aes(x = year_spring, y = !!sym(input$osas.value.focus), color = organization, group = grade_level, label = number_of_participants, label2 = participation_rate, label3 = source_file)) +
            theme_bw() +
            theme(legend.position = "top") +
            geom_line() +
            geom_point() +
            geom_text(aes(label = round(!!sym(input$osas.value.focus), 1)), nudge_y = 5, size = 2.5) + # added apr 16th
            # geom_label_repel(aes(label = round(!!sym(input$osas.value.focus), 1)),
            #                  box.padding = 0.3,
            #                  point.padding = 0.2,
            #                  max.overlaps = Inf,
            #                  show.legend = FALSE
            #                  ) + 
            facet_grid(subject.fix ~ grade_level,
                       labeller = labeller(subject = label_wrap_gen(8),
                                           multi_line = TRUE)) +
            ylim(0, 100) +
            scale_x_continuous(breaks = c(2014, 2018, 2022)) +
            labs(title = paste0("OSAS - ",input$osas.value.focus," (",my_group,")")) -> g

        ggplotly(g) %>%
            plotly::layout(legend = list(x = 0.9,
                                         xanchor = "right",
                                         y = -0.1,
                                         yanchor = "top"),
                            margin = list(r = -5)
                           )
    })

    output$osas.cohort.outcomes <- renderPlotly({
        my_subjects <- input$osas.subjects.focus        
        my_group <- input$student_groups.focus
        my_organizations <- input$osas.organization.focus
        my_cohorts <- as.numeric(input$osas.cohorts.select)

        dt.osas %>% filter(subject %in% my_subjects,
                           !is.na(class_of),
                           organization %in% my_organizations,
                           student_group == my_group,
                           class_of %in% my_cohorts) %>%
            ggplot(aes(x = grade_num, y = !!sym(input$osas.value.focus), color = organization, group = class_of,
            label = number_of_participants, label2 = participation_rate, label3 = source_file)) +
            theme_bw() +
            geom_line() +
            geom_point() +
            facet_grid(class_of ~ subject,
                       labeller = labeller(label_wrap_gen(8),
                                           multi_line = TRUE)) +
            ylim(0, 100) +
            scale_x_continuous(breaks = c(3:8,11)) +
            labs(title = paste0("OSAS - ",input$osas.value.focus," (",my_group,")"),
                 x = "Grade") -> g

        ggplotly(g) %>% plotly::layout(legend = list(x = 0.9,
                                                     xanchor = "right",
                                                     y = -.1,
                                                     yanchor = "top"),
                                       margin = list(r = -5))
    })
    
    osas_aggregated <- reactive({
        req(input$osas.focus_year)

        focus_year <- as.numeric(input$osas.focus_year)
        my_subjects <- input$osas.subjects.focus
        my_group <- input$student_groups.focus
        my_grades <- input$osas.grade.focus
        value_type <- input$osas.value.focus
        value_col <- sym(input$osas.value.focus)

        if (is.null(my_group) || length(my_group) == 0) {
            return(tibble::tibble(
                subject = character(),
                grade_level = character(),
                district = character(),
                organization = character(),
                total_students = numeric(),
                focus_value = numeric()
            ))
        }

        if (is.null(my_subjects) || length(my_subjects) == 0) {
            return(tibble::tibble(
                subject = character(),
                grade_level = character(),
                district = character(),
                organization = character(),
                total_students = numeric(),
                focus_value = numeric()
            ))
        }

        if (is.null(my_grades) || length(my_grades) == 0) {
            return(tibble::tibble(
                subject = character(),
                grade_level = character(),
                district = character(),
                organization = character(),
                total_students = numeric(),
                focus_value = numeric()
            ))
        }

        # browser()

        ## TODO verify that participant numbers is correct and matches canonical ODE sources
        ## Verify that reported values match
        dt.osas %>%
            filter(year_spring == focus_year,
                   subject %in% my_subjects,
                   student_group %in% my_group,
                   grade_level %in% my_grades) %>%
            mutate(
                focus_type = value_type,
                focus_value = as.numeric(!!value_col),
                participants_raw = suppressWarnings(as.numeric(number_of_participants))
            ) %>%
            filter(!is.na(focus_value)) %>%
            mutate(participants_raw = if_else(is.na(participants_raw), 0, participants_raw)) %>%
            group_by(subject, grade_level, district, organization, focus_type, focus_value, participants_raw) %>%
            summarise(source_file = paste(unique(source_file), collapse = ", "),
                      source_url = paste(unique(source_url), collapse = ", "),
                      .groups = 'drop') %>%
            rename(total_students = participants_raw)

            # group_by(subject, grade_level, district, organization) %>%
            # summarise(
            #     total_students = sum(as.numeric(participants_raw), na.rm = TRUE),
            #     focus_value = if_else(total_students > 0,
            #                           sum(focus_value * participants_raw, na.rm = TRUE) / total_students,
            #                           mean(focus_value, na.rm = TRUE)),
            #     .groups = 'drop'
            # )
    })

    osas_size_bounds <- reactive({
        min_val <- suppressWarnings(as.numeric(input$osas.size.min))
        max_val <- suppressWarnings(as.numeric(input$osas.size.max))

        if (is.null(min_val) || length(min_val) == 0) {
            min_val <- NA_real_
        }
        if (is.null(max_val) || length(max_val) == 0) {
            max_val <- NA_real_
        }

        if (!is.na(min_val) && !is.na(max_val) && min_val > max_val) {
            tmp <- min_val
            min_val <- max_val
            max_val <- tmp
        }

        list(
            min_raw = min_val,
            max_raw = max_val,
            min = ifelse(is.na(min_val), -Inf, min_val),
            max = ifelse(is.na(max_val), Inf, max_val),
            active = !(is.na(min_val) && is.na(max_val))
        )
    })

    output$osas.size.hist <- renderPlotly({
        aggregated <- osas_aggregated()
        if (nrow(aggregated) == 0) {
            return(plotly::plotly_empty())
        }

        aggregated <- aggregated %>% filter(!grepl("All Districts",organization), as.character(district) != as.character(organization))

        bounds <- osas_size_bounds()
        if (is.null(bounds$active) || !bounds$active) {
            aggregated <- aggregated %>% mutate(in_range = FALSE)
        } else {
            aggregated <- aggregated %>%
                mutate(in_range = total_students >= bounds$min & total_students <= bounds$max)
        }
        
        range_span <- diff(range(aggregated$total_students, na.rm = TRUE))
        binwidth <- ifelse(range_span <= 30, 1, 5)
        if (!is.finite(binwidth) || binwidth <= 0) {
            binwidth <- 1
        }

        # browser()

        g <- ggplot(aggregated, aes(x = total_students, fill = in_range)) +
            geom_histogram(binwidth = binwidth, color = "#ffffff") +
            scale_fill_manual(values = c(`TRUE` = "#3182bd", `FALSE` = "#d3d3d3"), guide = FALSE) +
            xlim(0, min(1000, max(aggregated$total_students))) + 
            labs(x = "Students tested", y = "Organizations") +
            theme_minimal(base_size = 12)

        suppressWarnings(ggplotly(g))
    })

    output$osas.participation_rate <- renderPlotly({
        my_subjects <- input$osas.subjects.focus        
        my_group <- input$student_groups.focus
        my_organizations <- input$osas.organization.focus        
        
        dt.osas %>% filter(subject %in% my_subjects,
                           organization %in% my_organizations,
                           student_group == my_group) %>%
            mutate(subject.fix = case_when(subject == "English Language Arts" ~ "ELA",
                                           subject == "Mathematics" ~ "Math",
                                           subject == "Science" ~ "Science")) %>%
            ggplot(aes(x = year_spring, y = participation_rate, color = organization, group = grade_level)) +
            theme_bw() +
            theme(legend.position = "top") +            
            geom_line() +
            geom_point() +
            facet_grid(subject.fix ~ grade_level,
                       labeller = labeller(subject = label_wrap_gen(8),
                                           multi_line = TRUE)) +
            ylim(0, 100) +            
            scale_x_continuous(breaks = c(2014, 2018, 2022)) +
            labs(title = paste0("OSAS Participation Rate (",my_group,")")) -> g

        ggplotly(g) %>% plotly::layout(legend = list(x = 0.9,
                                                     xanchor = "right",
                                                     y = -.1,
                                                     yanchor = "top"),
                                       margin = list(r = -5))
    })

    output$osas.top.bottom.table <- renderDT({
        aggregated <- osas_aggregated()

        bounds <- osas_size_bounds()

        filtered <- aggregated %>%
            filter(total_students >= bounds$min,
                   total_students <= bounds$max)

        if (nrow(filtered) == 0) {
            empty_tbl <- tibble::tibble(
                `Year (Spring)` = numeric(),
                Subject = character(),
                Grade = character(),
                `Student Group` = character(),
                District = character(),
                Organization = character(),
                Direction = character(),
                Rank = numeric(),
                `Students` = numeric(),
                `Focus` = character(),
                `Focus Value` = numeric(),
                `Source File` = character()
            )
            return(datatable(empty_tbl))
        }

        focus_year <- as.numeric(input$osas.focus_year)
        my_group <- input$student_groups.focus

        filtered <- filtered %>%
            mutate(
                focus_value = round(focus_value, 1),
                total_students = round(total_students)
            )

        top_tbl <- filtered %>%
            group_by(subject, grade_level) %>%
            arrange(desc(focus_value), organization) %>%
            mutate(rank = row_number()) %>%
            slice_head(n = 5) %>%
            mutate(direction = "Highest")

        bottom_tbl <- filtered %>%
            group_by(subject, grade_level) %>%
            arrange(focus_value, organization) %>%
            mutate(rank = row_number()) %>%
            slice_head(n = 5) %>%
            mutate(direction = "Lowest")

        summary_tbl <- bind_rows(top_tbl, bottom_tbl) %>%
            ungroup() %>%
            mutate(
                Grade = as.character(grade_level),
                `Year (Spring)` = focus_year,
                `Student Group` = my_group,
                `Students` = total_students,
                direction_order = if_else(direction == "Highest", 0, 1),
                focus_sort = if_else(direction == "Highest", -focus_value, focus_value)
            ) %>%
            arrange(`Year (Spring)`, subject, Grade, `Student Group`, district, direction_order, rank, focus_sort) %>%
            mutate(file_link = paste0("<a href='", source_url, "' target='_blank'>", source_file, "</a>")) %>%
            select(`Year (Spring)`, subject, Grade, `Student Group`, district, organization, direction, rank, `Students`, focus_type, focus_value, file_link) %>%
            rename(
                Subject = subject,
                District = district,
                Organization = organization,
                Direction = direction,
                Rank = rank,
                `Focus` = focus_type,
                `Focus Value` = focus_value,
                `Source File` = file_link
            )

        datatable(summary_tbl,
                  options = list(
                      pageLength = 10,
                      order = list(list(0, 'asc'), list(1, 'asc'), list(2, 'asc'), list(3, 'asc'), list(7, 'asc'), list(11, 'desc'))
                      
                  ),
                  escape = FALSE
                  )
    }, server = TRUE)
}

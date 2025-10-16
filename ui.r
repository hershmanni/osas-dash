library(shinydashboard)
library(tidyr)

header <- dashboardHeader(title = "OSAS Group Assessment Dashboard v0.1")

sidebar <- dashboardSidebar(width = 80,
    tags$style(HTML(".dropdown-menu>li>a{ color: #444 !important; }")),
    tags$style(HTML('div #sankey { height: 800px !important; }')),
    tags$style(HTML('div #sankey-summary table td:nth-child(1) { text-align: right; padding-right: 10px }')),
    tags$style(HTML('div #sankey-summary table td:nth-child(2) { font-weight: bold; }')),
    sidebarMenu(
        menuItem("OSAS", tabName = "osas")
    )
)

# styling inputs

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "osas",
        tags$style(HTML("
                .osas-size-input {
                    position: relative;
                    display: inline-block;
                    width: 100%;
                }
                .osas-size-input input.form-control {
                    padding-left: 28px;
                }
                .osas-size-input::before {
                    position: absolute;
                    top: 40%;
                    left: 15px;
                    //transform: translateY(-50%);
                    color: #ccc;
                    pointer-events: none;
                    font-size: 12pt;
                }
                .osas-size-input-min::before { content: '≥'; }
                .osas-size-input-max::before { content: '≤'; }
                ")),
                tags$style(HTML("#osas_student_group_summary table.dataTable tbody td { padding: 2px 6px; }")),
                HTML(paste0('<p>Data Source: <b>',dt.osas.fname,'</b> (happy to provide by request)</p>',
                '<p>Raw files provided by ODE at <a href="https://www.oregon.gov/ode/educator-resources/assessment/Pages/Assessment-Group-Reports.aspx">Group Assessments page</a>.</p>',
                '<p>Oregonlive has a <a href="https://schools.oregonlive.com/">test score viewer</a> and <a href="https://www.oregonlive.com/education/2024/10/look-up-test-scores-for-any-oregon-school-see-performance-trends-at-a-glance.html">overview</a>.</p>',
                '<p>Script/Site built by: Nick Hershman. Please let me know if you notice any errors or have suggestions for improvement!</p>',
                '<p>All code available at <a href="https://github.com/hershmanni/osas-dash">github.com/hershmanni/osas-dash</a>.</p>',
                '<p>Last data update: ',dt.osas.date,'</p>')),
                hr(),
                h2('OSAS Group Assessment Results'),
                p('This data can serve as an external indicator of student performance. Because these files are publicly provided they do not include individual student-level data and so cannot be joined with individual student records on the other reports.'),
                checkboxGroupButtons(inputId = 'osas.subjects.focus',
                                  label = 'Select subject(s)',
                                  choices = dt.osas %>% distinct(subject) %>% mutate(subject = as.character(subject)) %>% pull,
                                  selected = dt.osas %>% distinct(subject) %>% mutate(subject = as.character(subject)) %>% pull
                                  ),
                hr(),
                uiOutput('osas.student.group.chooser'),
                p(paste0('Number of schools and districts reporting OSAS data for your selected student group.')),
                DTOutput('osas.student.group.summary'),
                hr(),
                radioGroupButtons(inputId = 'osas.value.focus',
                                    label = 'Select focus value',
                                    choices = c("percent_proficient","percent_level_1", "percent_level_2", "percent_level_3","percent_level_4"),
                                    selected = "percent_proficient"
                                    ),
                materialSwitch(inputId = 'osas.outcomes.show_labels',
                            label = 'Show labels on yearly outcomes',
                            value = FALSE,
                            status = 'primary'
                            ),
                fluidRow(
                    column(width = 6,
                    pickerInput(inputId = "osas.organization.focus",
                            label = "Select Organization(s)",
                            choices = levels(dt.osas$organization),
                            selected = c("Oregon (All Districts)"),
                            multiple = TRUE,
                            options = pickerOptions(
                                actionsBox = TRUE,
                                liveSearch = TRUE,
                                size = 10
                                )
                            )
                    ),
                    column(width = 6,
                           pickerInput(inputId = "osas.grade.focus",
                            label = "Select Grade Level(s)",
                            choices = dt.osas %>% distinct(grade_level) %>% pull,
                            selected = dt.osas %>% distinct(grade_level) %>% pull,
                            multiple = TRUE,
                            options = pickerOptions(
                                actionsBox = TRUE
                                )
                            ),
                    )
                ),
                jqui_resizable(plotlyOutput('osas.outcomes')),
                hr(),
                pickerInput(inputId = "osas.cohorts.select",
                            label = "Select cohorts by class_of",
                            choices = dt.osas %>% distinct(class_of) %>% drop_na() %>% arrange(class_of) %>% pull,
                            selected = 2022:2026,
                            multiple = TRUE,
                            options = pickerOptions(
                                actionsBox = TRUE,
                                )
                            ),
                materialSwitch(inputId = 'osas.cohort.outcomes.show_labels',
                               label = 'Show labels on cohort trajectories',
                               value = FALSE,
                               status = 'primary'
                               ),
                hr(),
                jqui_resizable(plotlyOutput('osas.cohort.outcomes')),
                hr(),
                jqui_resizable(plotlyOutput('osas.participation_rate')),
                hr(),
                h3('Highest Performing Organizations'),
                HTML('<p>Find the highest performing organizations in a given <b>focus year</b> for this student population. Select a year and use the minimum and maximum inputs if you want to only include schools or districts with certain numbers of students.</p>'),
                pickerInput(inputId = "osas.focus_year",
                            label = "Focus Spring Year",
                            choices = dt.osas %>% distinct(year_spring) %>% arrange(desc(year_spring)) %>% pull,
                            selected = dt.osas %>% summarise(max(year_spring, na.rm = TRUE)) %>% pull,
                            multiple = FALSE,
                            options = pickerOptions(
                                actionsBox = FALSE
                                )
                            ),
                plotlyOutput('osas.size.hist', height = 220),
                div(style = 'min-width: 720px; max-width: 100%;',
                    fluidRow(
                        column(
                            width = 2,
                            div(
                                class = "osas-size-input osas-size-input-min",
                                numericInput(
                                    inputId = 'osas.size.min',
                                    label = 'Minimum Students Tested',
                                    value = osas_size_min,
                                    min = 0,
                                    step = 1
                                )
                            )
                        ),
                        column(
                            width = 2,
                            div(
                                class = "osas-size-input osas-size-input-max",
                                numericInput(
                                    inputId = 'osas.size.max',
                                    label = 'Maximum Students Tested',
                                    value = osas_size_max,
                                    min = 0,
                                    step = 1
                                )
                            )
                        )
                    )
                ),
                p('Highlights the five highest organizations for the selected focus value, by grade, in the chosen year.'),
                DTOutput('osas.top.bottom.table'),
                #renderDataTable('osas.top.bottom.table'),
                hr()
    ))
)

ui <- dashboardPage(header, sidebar, body, skin = "purple")

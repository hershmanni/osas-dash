library(shinydashboard)
library(tidyr)
library(dplyr)
library(janitor)
options(dplyr.summarise.inform=FALSE)
library(networkD3)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjqui)
library(DT)
library(ggplot2)
library(plotly)


# load data
dt.osas.date <- '20251002'

cat(dt.osas.date)
dt.osas.fname <- paste0('ode_pagr_all_files-',dt.osas.date,'.rds')
dt.osas <- readRDS(dt.osas.fname) %>%
    mutate(year_spring = year_fall + 1)



osas_size_summary <- dt.osas %>%
    filter(!grepl("All Districts",organization), as.character(district) != as.character(organization)) %>%
    summarise(
        min_participants = suppressWarnings(min(number_of_participants, na.rm = TRUE)),
        max_participants = suppressWarnings(max(number_of_participants, na.rm = TRUE))
    )

osas_size_min <- osas_size_summary$min_participants
if (!is.finite(osas_size_min)) {
    osas_size_min <- 0
}

osas_size_max <- osas_size_summary$max_participants
if (!is.finite(osas_size_max)) {
    osas_size_max <- osas_size_min
}
if (osas_size_max < osas_size_min) {
    osas_size_max <- osas_size_min + 1
}


header <- dashboardHeader(title = "OSAS Group Assessment Dashboard v0.1")

sidebar <- dashboardSidebar(width = 500,
    tags$style(HTML(".dropdown-menu>li>a{ color: #444 !important; }")),
    tags$style(HTML('div #sankey { height: 800px !important; }')),
    tags$style(HTML('div #sankey-summary table td:nth-child(1) { text-align: right; padding-right: 10px }')),
    tags$style(HTML('div #sankey-summary table td:nth-child(2) { font-weight: bold; }')),
    sidebarMenu(
        menuItem("OSAS", tabName = "osas")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "osas",
                HTML(paste0('<p>Data Source: <b>',dt.osas.fname,'</b></p>',
                '<p>Raw files provided by ODE at <a href="https://www.oregon.gov/ode/educator-resources/assessment/Pages/Assessment-Group-Reports.aspx">Group Assessments page</a>.</p>',
                '<p>Script/Site built by: Nick Hershman. Please let me know if you notice any errors or have suggestions for improvement!</p>',
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
                hr(),
                radioGroupButtons(inputId = 'osas.value.focus',
                                  label = 'Select focus value',
                                  choices = c("percent_proficient","percent_level_1", "percent_level_2", "percent_level_3","percent_level_4"),
                                  selected = "percent_proficient"
                                  ),
                pickerInput(inputId = "osas.grade.focus",
                            label = "Select Grade Level(s)",
                            choices = dt.osas %>% distinct(grade_level) %>% pull,
                            selected = dt.osas %>% distinct(grade_level) %>% pull,
                            multiple = TRUE,
                            options = pickerOptions(
                                actionsBox = TRUE
                                )
                            ),
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
                div(style = 'width: 720px; max-width: 100%;',
                    sliderInput('osas.size.range',
                                label = 'Organization Size (students tested)',
                                min = floor(osas_size_min),
                                max = ceiling(osas_size_max),
                                value = c(floor(osas_size_min), ceiling(osas_size_max)),
                                step = 1,
                                sep = ',',
                                width = '100%')
                ),
                pickerInput(inputId = "osas.organization.focus",
                            label = "Select Organization(s)",
                            choices = levels(dt.osas$organization),
                            selected = c("Oregon (All Districts)","Beaverton SD 48J"),
                            multiple = TRUE,
                            options = pickerOptions(
                                actionsBox = TRUE,
                                liveSearch = TRUE,
                                size = 10
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
                hr(),
                jqui_resizable(plotlyOutput('osas.cohort.outcomes')),
                hr(),
                jqui_resizable(plotlyOutput('osas.participation_rate')),
                hr(),
                h3('Top & Bottom Performers'),
                p('Highlights the five highest and five lowest organizations for the selected focus value, by grade, in the chosen year.'),
                DTOutput('osas.top.bottom.table'),
                hr()
    ))
)

ui <- dashboardPage(header, sidebar, body, skin = "purple")

header <- dashboardHeader(
  title = paste("OSAS Explorer", dashboard_version),
  titleWidth = 320
)

sidebar <- dashboardSidebar(
  width = 320,
  sidebarMenu(
    id = "main_tab",
    menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
    menuItem("Population", tabName = "population", icon = icon("users")),
    menuItem("Rankings", tabName = "rankings", icon = icon("ranking-star")),
    menuItem("Cohorts", tabName = "cohorts", icon = icon("people-group")),
    menuItem("Coverage & Data", tabName = "coverage", icon = icon("table-cells"))
  ),
  div(
    class = "sidebar-filter-panel",
    h4("Explore"),
    conditionalPanel(
      condition = "input.main_tab != 'population'",
      pickerInput(
        "student_group",
        "Student population",
        choices = dashboard_student_groups,
        selected = dashboard_default_group,
        options = pickerOptions(liveSearch = TRUE, size = 10)
      )
    ),
    conditionalPanel(
      condition = "input.main_tab == 'population'",
      virtualSelectInput(
        "population_groups",
        "Student populations",
        choices = dashboard_student_groups,
        selected = dashboard_default_population_groups,
        multiple = TRUE,
        search = TRUE,
        showSelectedOptionsFirst = TRUE,
        optionsCount = 10,
        noOfDisplayValues = 2,
        maxValues = 8,
        disableSelectAll = TRUE,
        updateOn = "change"
      )
    ),
    pickerInput(
      "subject",
      "Subject",
      choices = dashboard_subjects,
      selected = dashboard_default_subject,
      options = pickerOptions(size = 6)
    ),
    virtualSelectInput(
      "organizations",
      "Schools, districts, and state",
      choices = organization_choices,
      selected = "state:oregon",
      multiple = TRUE,
      search = TRUE,
      showSelectedOptionsFirst = TRUE,
      optionsCount = 6,
      optionHeight = "34px",
      noOfDisplayValues = 2,
      maxValues = 8,
      disableSelectAll = TRUE,
      disableOptionGroupCheckbox = TRUE,
      position = "bottom left",
      showDropboxAsPopup = FALSE,
      zIndex = 2000,
      updateOn = "change"
    ),
    div(
      class = "sidebar-actions",
      actionButton(
        "add_benchmarks",
        "Add Oregon + districts",
        icon = icon("plus"),
        class = "btn-info btn-sm",
        title = "Add Oregon and the current parent district of each selected school."
      ),
      actionButton("reset_filters", "Reset", icon = icon("rotate-left"), class = "btn-default btn-sm")
    ),
    p(class = "help-block benchmark-help", "Adds Oregon and each selected school's current parent district."),
    conditionalPanel(
      condition = "input.main_tab == 'trends'",
      h4("Trend options"),
      pickerInput(
        "trend_metric",
        "Measure",
        choices = stats::setNames(names(dashboard_metric_choices), dashboard_metric_choices),
        selected = "percent_proficient",
        options = pickerOptions(size = 8)
      ),
      virtualSelectInput(
        "trend_grades",
        "Grades",
        choices = dashboard_grades,
        selected = dashboard_default_grades,
        multiple = TRUE,
        showSelectedOptionsFirst = TRUE,
        optionsCount = 10,
        noOfDisplayValues = 3,
        disableSelectAll = FALSE,
        updateOn = "change"
      ),
      materialSwitch("trend_labels", "Label latest values", value = FALSE, status = "primary"),
      materialSwitch("show_participation", "Show participation trend", value = FALSE, status = "primary")
    ),
    conditionalPanel(
      condition = "input.main_tab == 'rankings'",
      h4("Ranking options"),
      pickerInput(
        "rank_year",
        "Spring year",
        choices = rev(dashboard_spring_years),
        selected = max(dashboard_spring_years),
        options = pickerOptions(size = 10)
      ),
      checkboxGroupButtons(
        "rank_types",
        "Organization types",
        choices = organization_type_choices,
        selected = dashboard_org_types,
        status = "primary",
        justified = TRUE,
        checkIcon = list(yes = icon("check"))
      ),
      pickerInput(
        "rank_grade",
        "Grade",
        choices = dashboard_grades,
        selected = if ("Grade 5" %in% dashboard_grades) "Grade 5" else dashboard_grades[[1]],
        options = pickerOptions(size = 10)
      ),
      sliderInput("rank_top_n", "Top organizations", min = 5, max = 30, value = 10, step = 5),
      numericInput("rank_min", "Minimum students tested", value = dashboard_participants_min, min = 0, step = 1),
      numericInput("rank_max", "Maximum students tested", value = dashboard_participants_max, min = 0, step = 1)
    ),
    conditionalPanel(
      condition = "input.main_tab == 'cohorts'",
      h4("Cohort options"),
      virtualSelectInput(
        "cohort_classes",
        "Graduating classes",
        choices = dashboard_cohorts,
        selected = dashboard_default_cohorts,
        multiple = TRUE,
        search = TRUE,
        showSelectedOptionsFirst = TRUE,
        optionsCount = 10,
        noOfDisplayValues = 3,
        disableSelectAll = FALSE,
        updateOn = "change"
      ),
      pickerInput(
        "cohort_metric",
        "Measure",
        choices = stats::setNames(names(dashboard_metric_choices), dashboard_metric_choices),
        selected = "percent_proficient",
        options = pickerOptions(size = 8)
      ),
      virtualSelectInput(
        "cohort_grades",
        "Grades",
        choices = dashboard_tested_grades,
        selected = dashboard_tested_grades,
        multiple = TRUE,
        showSelectedOptionsFirst = TRUE,
        optionsCount = 10,
        noOfDisplayValues = 3,
        disableSelectAll = FALSE,
        updateOn = "change"
      ),
      materialSwitch("cohort_labels", "Label latest grade", value = FALSE, status = "primary")
    ),
    conditionalPanel(
      condition = "input.main_tab == 'coverage'",
      h4("Coverage options"),
      checkboxGroupButtons(
        "coverage_types",
        "Organization types",
        choices = organization_type_choices,
        selected = dashboard_org_types,
        status = "primary",
        justified = TRUE,
        checkIcon = list(yes = icon("check"))
      )
    )
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css"),
    tags$script(src = "dashboard.js", defer = "defer"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  tabItems(
    tabItem(
      tabName = "trends",
      h2("Spring assessment trends"),
      p(
        class = "dashboard-intro",
        "Compare state, district, and school results over time. The chart uses spring testing years and preserves gaps where no source records are available."
      ),
      uiOutput("trend_selection_summary"),
      box(
        width = 12,
        title = "Assessment outcomes",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        uiOutput("trend_plot_ui"),
        uiOutput("trend_source_details")
      ),
      conditionalPanel(
        condition = "input.show_participation == true",
        box(
          width = 12,
          title = "Participation rate",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput("participation_plot_ui")
        )
      )
    ),
    tabItem(
      tabName = "rankings",
      h2("Highest-performing organizations"),
      div(
        class = "ranking-note",
        tags$b("How to read this view: "),
        "State, district, and school results may be displayed and ranked together. These units represent overlapping populations, so the overall rank is descriptive. Type rank provides the within-type comparison."
      ),
      fluidRow(
        column(4, uiOutput("rank_eligible_card")),
        column(4, uiOutput("rank_top_card")),
        column(4, uiOutput("rank_oregon_card"))
      ),
      box(
        width = 8,
        title = "Top organizations",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("ranking_plot", height = 520),
        uiOutput("ranking_source_details")
      ),
      box(
        width = 4,
        title = "Students tested distribution",
        status = "info",
        solidHeader = TRUE,
        plotlyOutput("rank_distribution", height = 520)
      ),
      box(
        width = 12,
        title = "Top results and pinned comparisons",
        status = "primary",
        solidHeader = TRUE,
        DTOutput("ranking_table")
      ),
      box(
        width = 12,
        title = "Performance across grades relative to Oregon",
        status = "info",
        solidHeader = TRUE,
        plotlyOutput("grade_heatmap", height = 520),
        uiOutput("heatmap_source_details")
      )
    ),
    tabItem(
      tabName = "population",
      h2("Students tested over time"),
      p(
        class = "dashboard-intro",
        "Compare the published All Grades tested count for up to eight student populations over time. Each organization panel uses its own vertical scale."
      ),
      div(
        class = "data-note",
        p(
          "Student populations can overlap and should not be added together. Population definitions and reporting availability may change across years. Where duplicate source files disagree, the chart uses the lower reported count and the tooltip shows the published range."
        ),
        p(
          class = "population-definition-note",
          tags$b("Important for spring 2022: "),
          "ODE reported this group as “Econo. Disadvantaged,” and pandemic-era universal meal rules caused nearly all tested students to be included. The dashboard harmonizes that historical label as “Students Experiencing Poverty,” but the 2022 value should be treated as a reporting-definition break—not as a demographic change."
        )
      ),
      uiOutput("population_selection_summary"),
      box(
        width = 12,
        title = "Students tested by population",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        uiOutput("population_plot_ui"),
        uiOutput("population_source_details")
      ),
      box(
        width = 12,
        title = "Published student counts",
        status = "info",
        solidHeader = TRUE,
        DTOutput("population_table")
      )
    ),
    tabItem(
      tabName = "cohorts",
      h2("Synthetic grade-cohort trajectories"),
      div(
        class = "data-note",
        "These lines connect aggregate results for a graduating class across tested grades. They do not track the same individual students, and student-group composition can change over time."
      ),
      uiOutput("cohort_selection_summary"),
      box(
        width = 12,
        title = "Grade-cohort outcomes",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        uiOutput("cohort_plot_ui"),
        uiOutput("cohort_source_details")
      )
    ),
    tabItem(
      tabName = "coverage",
      h2("Coverage, sources, and downloads"),
      p(
        class = "dashboard-intro",
        "Coverage counts distinct organizations for every subject, regardless of the Subject selected in the sidebar."
      ),
      div(
        class = "dashboard-about",
        tags$b("About: "),
        "Data source: ",
        tags$a(
          href = "https://www.oregon.gov/ode/educator-resources/assessment/Pages/Assessment-Group-Reports.aspx",
          target = "_blank",
          rel = "noopener noreferrer",
          "ODE-released PAGR files"
        ),
        ". Data cleaning and views prepared by Nick Hershman."
      ),
      box(
        width = 12,
        title = "Available organizations by spring year",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("coverage_plot", height = 560)
      ),
      box(
        width = 12,
        title = "Distinct organization counts",
        status = "info",
        solidHeader = TRUE,
        DTOutput("coverage_table")
      ),
      box(
        width = 7,
        title = "Prepared data validation",
        status = "warning",
        solidHeader = TRUE,
        DTOutput("validation_table")
      ),
      box(
        width = 5,
        title = "Data notes and download",
        status = "primary",
        solidHeader = TRUE,
        uiOutput("data_metadata"),
        br(),
        downloadButton("download_filtered", "Download filtered canonical data", class = "btn-primary")
      ),
      box(
        width = 12,
        title = "Source conflicts retained in the prepared data",
        status = "warning",
        solidHeader = TRUE,
        p("These fields differ across duplicate public source files. Agreed outcome fields remain available; student-count eligibility uses the lower reported count."),
        div(class = "conflict-table-region", DTOutput("conflict_table")),
        div(
          class = "conflict-actions",
          downloadButton("download_conflicts", "Download conflict report", class = "btn-warning")
        )
      )
    )
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "purple",
  title = paste("OSAS Explorer", dashboard_version)
)

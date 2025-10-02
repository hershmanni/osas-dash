
rm(list = ls())
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
library(openxlsx)


# load data
dt.osas.date <- '20251002'
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

source('ui.r')
source('server.r')

shinyApp(ui = ui, server = server)
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

dt.osas.fname <- paste0('ode_pagr_all_files-',dt.osas.date,'.rds')
dt.osas <- readRDS(dt.osas.fname) %>%
    mutate(year_spring = year_fall + 1)

dt.osas %>% str()

osas_palette_base <- c(
    "#1f77b4", # blue
    "#ff7f0e", # orange
    "#2ca02c", # green
    "#d62728", # red
    "#9467bd", # purple
    "#8c564b", # brown
    "#17becf", # cyan
    "#bcbd22", # olive
    "#e377c2", # pink
    "#7f7f7f"  # gray
)

# osas_palette_base <- c(
#     "#21bde0", # blue
#     "#ffa70e" # orange
# )

osas_palette <- grDevices::colorRampPalette(osas_palette_base)
osas_org_colors <- setNames(
    osas_palette(length(levels(dt.osas$organization))),
    levels(dt.osas$organization)
)

osas_colors_for <- function(orgs) {
    orgs <- unique(as.character(orgs))
    if (length(orgs) > length(osas_palette_base)) {
        myPal <- setNames(osas_palette(length(orgs)), orgs)
    } else {
        myPal <- setNames(osas_palette_base[0:length(orgs)], orgs)
    }
    return(myPal)
}


osas_size_summary <- dt.osas %>%
    # filter(!grepl("All Districts",organization), as.character(district) != as.character(organization)) %>%
    filter(!grepl("All Districts",organization)) %>%
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

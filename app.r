library(shiny)

source("global.r", local = FALSE)
source("ui.r", local = FALSE)
source("server.r", local = FALSE)

shinyApp(ui = ui, server = server)

library(shiny)
library(shinydashboard)
## library(shinyWidgets)
## library(shinyBS)
## library(shinyTree)
## library(fansi)
## library(DT)
## library(reactable)
## library(leaflet)
library(shinyjs)


## if (options()$browser == "") options(browser = "chromium")
## unloadNamespace("status")
## library(status)

## source("shiny/ui.R")
## source("shiny/server.R")


## setwd("R")
runApp(
  appDir = "shiny",
  port = 3838,
  host = "0.0.0.0",
  launch.browser = FALSE
)


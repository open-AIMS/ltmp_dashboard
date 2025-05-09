source("ui_header.R")
source("ui_sidebar.R")
source("ui_body.R")

ui <- dashboardPage(
        useShinyjs(),              # required in order to use any shinyjs functions
        header = header,
        ## sidebar
        sidebar = sidebar,
        ## body
        body = body
)

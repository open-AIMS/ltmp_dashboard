sidebar <- dashboardSidebar(
  sidebarMenu(
    tags$head(tags$style(".inactiveLink {
                           pointer-events: none;
                           color: grey !important;
                           cursor: default;
                           }
                      .activeLink {
                           pointer-events: auto;
                           color: orange !important;
                           cursor: pointer;
                           }
    ")),

    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    ## menuItem("Landing", tabName = "landing", icon = icon("home")),
    ## menuItem("Data", tabName = "data", icon = icon("file-excel")),
    ## menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("chart-column")),
    ## menuItem("Analysis", tabName = "analysis", icon = icon("calculator")),
    ## menuItem("Manual", tabName = "manual", icon=icon("mortar-board")),
    ## hr(),
    ## actionButton("runLoadCode", "Run Stage 2", icon = icon("play")),
    ## ## actionBttn("runLoadCode", "Run Stage 2", style = "jelly", color =  "primary", icon = icon("play")),
    ## actionButton("runProcessCode", "Run Stage 3", icon = icon("play"), class = "btn-disabled"),
    ## ## actionBttn("runProcessCode", "Run Stage 3", style = "unite", color = "primary", icon = icon("play")),
    ## actionButton("runEDACode", "Run Stage 4", icon = icon("play"), class = "btn-disabled"),
    ## actionButton("runAnalysisCode", "Run Stage 5", icon = icon("play"))
    ## ## actionButton("runAnalysisCode", "Run Stage 5", icon = icon("play"), class = "btn-disabled")
    ## ## actionButton("runTestCode", "Run Test", icon = icon("play"), class = "btn-enabled")
  )
)

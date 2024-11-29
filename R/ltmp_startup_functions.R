#########################################################################
## The following function determines whether the current script is the ##
## parent (directly called from command line etc) or child (sourced    ##
## from another R script.                                              ##
## NOTE.  if we were to run sys.nframe() directly from a parent        ##
## script, it would return a value of 0.  However, since we are        ##
## calling sys.nframe() from a sourced script, then it will be 1       ##
#########################################################################
ltmp_is_parent <- function() {
   ifelse(sys.nframe()==1, TRUE, FALSE) 
}

#########################################################################
## The following function is a wrapper to a series of functions that:  ##
## - parse the command line arguments                                  ##
#########################################################################
ltmp_start_matter <- function(args = commandArgs()) {
    ltmp_initialise_status()                 ## create the status list
    status::status_set_stage(stage = 1, title = "Configure system")
    status::display_status_terminal()        ## display an opening banner
    ltmp_parse_cla(args)                    ## parse command line arguments
    if (status::get_setting("refresh_data")) ltmp_clear_data()
    ltmp_generate_other_settings() 
    ## ltmp_initialise_log()       ## create the log 
    ltmp_config()
    ltmp_check_packages()
    ltmp_analysis_stage()

}

ltmp_initialise_status <- function() {
   status::status_initialize(
    ## pkg = "reefCloudPackage",
    project_name = "LTMP/MMP dashboard",
    box_width = 100
  ) ## create the status list
}

ltmp_parse_cla <- function(args) {
  status::status_try_catch( {
    parent_file <- FALSE
    ## Generate a message outlining the necessary format of the command line arguments
    valid_cla <- paste0("The call must be of the form:\n",
                        "<script>.R --file=\"<PATH>\" --status=<true|false> --sql\n",
                        "\n<script>: \ta script name",
                        "\n<PATH>: \ta valid path to a folder containing the input data",
                        "\n<true|false>:\twhether to provide full status"
                        )
    ## determine whether any of the CLA's include the name of the R script to run
    has_script_argument <- any(grepl("--script=.*", args, perl = TRUE))
    ## determine whether any of the CLA's include a file path
    has_file_argument <- any(grepl("--file=.*", args, perl = TRUE))
    has_path_argument <- any(grepl("--path=.*", args, perl = TRUE))
    has_sql_argument <- any(grepl("--sql", args, perl = TRUE))
    ## if they do then it is likely this is the parent frame
    if (has_file_argument) {                              ## Read data
      ## also from a file (or bucket) only consider it the parentFile
      ## if the number of system frames (previous environments) is
      ## equal to one. if there are more than that, then this function
      ## must have been called from an environment that was itself
      ## called from the parent script
      if (sys.nframe() < 3) parent_file <- TRUE
    }
    ## if this is the parent frame (e.g. first time that this function
    ## has been called, parse the command line
    if (parent_file) {
      ## does not contain iether a "--file=" or a "--sql"
      if (!has_file_argument & !has_sql_argument) {  
        stop(paste0("A bucket (location/path of raw data) needs ",
                    "to be provided as a command line argument,\n\t",
                    "such as: Rscript <script.R> --file=<PATH>\n\n",
                    valid_cla
                    ))
      }
      ## parse out the path and filename
      if (has_path_argument) {
        ## extract the file argument
        arg <- args[grep("--path=.*", args)]
        arg <- gsub("'", "", arg) 
        arg <- gsub("\"", "", arg) 
        if (grepl('.*(\\.zip$|\\.csv$)',arg)) {  #if the commandline argument ends in a zip or csv
          FILENAME <- basename(arg)
          status::add_setting(element = "filename", item = FILENAME, name = "Filename")
          AWS_PATH <- gsub('--path=(.*)/raw$', '\\1/', dirname(arg))
          assign("AWS_PATH", AWS_PATH, envir = .GlobalEnv)
          status::add_setting(element = "aws_path", item = AWS_PATH, name = "AWS path")
          if (grepl("--path=s3.*", arg, perl = TRUE)) {         ## Must be AWS source
            status::add_setting(element = "data_from", item = "AWS", name = "Data source")
            ## parse out the domain unit (e.g. reef, region etc)
            SCALE <- gsub('.*process/[^/]*/[^/]*/[^/]*/([^/]*)/.*', '\\1', AWS_PATH)
            status::add_setting(element = "data_scale", item = SCALE, name = "Data scale")
            DOMAIN_NAME <- gsub('.*process/[^/]*/[^/]*/[^/]*/[^/]*/(.*)/.*', '\\1', AWS_PATH)
            status::add_setting(element = "domain_name", item = DOMAIN_NAME, name = "Domain name")
            DATA_METHOD <- gsub('[^/]*//[^/]*/[^/]*/([^/]*)/.*', '\\1', AWS_PATH)
            status::add_setting(element = "data_method", item = DATA_METHOD, name = "Data type")
          } else  {           ## local copy
            status::add_setting(element = "data_from", item = "local copy", name = "Data source")
          }
        } else {                     #we will have to go to the AIMS database
            status::add_setting(element = "data_from", item = "sql", name = "Data source")
        }
      }
    }
    ## Get all other CLA
    get_params_from_cla(args)
    ## Check that all the necessary settings have been set
    if (is.null(status::get_setting("aws_path")))
      stop(paste0("Source path must be supplied via the command line\n\t",
                  "such as: Rscript <script.R> --path=<path>\n\n",
                  valid_cla
                  ))
    if (is.null(status::get_setting("data_scale")))
      stop(paste0("Analysis scale must be supplied via the command line\n\t",
                  "such as: Rscript <script.R> --scale=<reef|sector|nrm|gbr>\n\n",
                  valid_cla
                  ))
    if (is.null(status::get_setting("domain_name")))
      stop(paste0("Domain name must be supplied via the command line\n\t",
                  "such as: Rscript <script.R> --domain=<name>\n\n",
                  valid_cla
                  ))
    if (is.null(status::get_setting("data_method")))
      stop(paste0("Analysis data type must be supplied via the command line\n\t",
                  "such as: Rscript <script.R> --method=<photo-transect|manta|fish|juveniles>\n\n",
                  valid_cla
                  ))
    if (is.null(status::get_setting("refresh_data")))
      status::add_setting(element = "refresh_data",
                          item = FALSE,
                          name = "Refresh data")
    if (is.null(status::get_setting("display_status")))
      status::add_setting(element = "display_status",
                          item = TRUE,
                          name = "Display status")
    ## reset the logfile directory - so that it is constantly being written back to the bucket
    assign("aws_out_path", paste0(AWS_PATH, "output/"), envir = .GlobalEnv)
    if (!dir.exists(aws_out_path)) dir.create(aws_out_path)
    log_file <- paste0(aws_out_path, gsub("(\\.csv|\\.zip)", ".log", FILENAME))
    if (!file.exists(log_file)) file.create(log_file)
    if (file.exists(log_file)) unlink(log_file)
    assign("log_file", log_file, envir = .GlobalEnv)
  },
  stage_ = 1,
  order_ = 1,
  name_ = "Parse CLA",
  item_ = "parse_cla"
  )
}

get_params_from_cla <- function(args) {
  has_method_argument <- any(grepl("--method=.*", args, perl = TRUE))
  if(has_method_argument) {
    arg <- args[grep("--method=.*", args)]
    status::add_setting(element = "data_method",
                        item = gsub("--method=(.)", "\\1", arg),
                        name = "Data type")
  } 
  has_scale_argument <- any(grepl("--scale=.*", args, perl = TRUE))
  if(has_scale_argument) {
    arg <- args[grep("--scale=.*", args)]
    status::add_setting(element = "data_scale",
                        item = gsub("--scale=(.)", "\\1", arg),
                        name = "Data scale")
  }
  has_domain_argument <- any(grepl("--domain=.*", args, perl = TRUE))
  if(has_domain_argument) {
    arg <- args[grep("--domain=.*", args)]
    arg <- gsub("'", "", arg) 
    status::add_setting(element = "domain_name",
                        item = gsub("--domain=(.)", "\\1", arg),
                        name = "Domain name")
  }
  DEBUG_MODE <- ifelse(any(grepl('--status ?= ?(true|t|TRUE|T)', args, perl = TRUE)), TRUE, FALSE)
  status::add_setting(element = "display_status",
                      item = DEBUG_MODE,
                      name = "Display status")
  REFRESH_DATA <- ifelse(any(grepl('--refresh_data ?= ?(true|t|TRUE|T)', args, perl = TRUE)), TRUE, FALSE)
  status::add_setting(element = "refresh_data",
                      item = REFRESH_DATA,
                      name = "Refresh data")
}

ltmp_clear_data <- function() {
   ## Clear the /data/primary folder
  unlink("../data/primary/*.csv")
  unlink("../data/primary/*.zip")
  unlink("../data/primary/*.RData")
  unlink("../data/primary/*.geojson")
  unlink("../data/primary/*.json")
  unlink("../data/primary/GIS/*.*", recursive = TRUE)

  unlink("../data/processed/*.*", recursive = TRUE)
  unlink("../data/modelled/*.*", recursive = TRUE)
  unlink("../data/summarised/*.*", recursive = TRUE)

  unlink("../output/figures/*.*", recursive = TRUE)

  unlink("../docs/Reports/*.html")
  unlink("../data/model_stage.RData")
  unlink("../data/reefCloudPackage::analysis_stage.RData")
}

ltmp_generate_other_settings <- function() {
  status::status_try_catch(
  {

    ## Location of folder to store R data objects
    assign("DATA_PATH", "../data/", envir = globalenv())
    if (!dir.exists(DATA_PATH)) dir.create(DATA_PATH)
    status::add_setting(element = "data_path", item = DATA_PATH, name = "Data path")

    ## Define the name of the input benthic data
    assign("INPUT_DATA", "reef_data.zip", envir = globalenv())
    status::add_setting(element = "input_data", item = INPUT_DATA, name = "Input data")

    ## Working name for RData version of input data
    assign("RDATA_FILE",  gsub('\\.csv|\\.zip','\\.RData', INPUT_DATA), envir = globalenv())
    status::add_setting(element = "rdata_file", item = RDATA_FILE, name = "R data file")

    ## Working name for csv version of input data
    assign("CSV_FILE",  gsub('\\.csv|\\.zip','\\.csv', INPUT_DATA), envir = globalenv())
    status::add_setting(element = "csv_file", item = CSV_FILE, name = "csv file")
 
  },
  stage_ = 1,
  order_ = 2,
  name_ = "Generate settings",
  item_ = "generate_settings"
  )
}

ltmp_config <- function() {
  status::status_try_catch(
  {
    if (!dir.exists(paste0(DATA_PATH, '/primary'))) dir.create(paste0(DATA_PATH, '/primary'))
    if (!dir.exists(paste0(DATA_PATH, '/processed'))) dir.create(paste0(DATA_PATH, '/processed'))
    if (!dir.exists(paste0(DATA_PATH, '/processed'))) dir.create(paste0(DATA_PATH, '/processed'))
    if (!dir.exists(paste0(DATA_PATH, '/modelled'))) dir.create(paste0(DATA_PATH, '/modelled'))
    if (!dir.exists(paste0(DATA_PATH, '/summarised'))) dir.create(paste0(DATA_PATH, '/summarised'))
    assign("OUTPUT_PATH", "../output/", envir = globalenv())
    status::add_setting(element = "output_path", item = OUTPUT_PATH, name = "Output path")
    if (!dir.exists(OUTPUT_PATH)) dir.create(OUTPUT_PATH)
    assign("FIGURES_PATH", "../output/figures/", envir = globalenv())
    if (!dir.exists(paste0(OUTPUT_PATH, 'figures'))) dir.create(paste0(OUTPUT_PATH, 'figures'))
    if (!dir.exists(paste0(OUTPUT_PATH, 'figures/spatial/'))) dir.create(paste0(OUTPUT_PATH, 'figures/spatial/'))
    if (!dir.exists(paste0(OUTPUT_PATH, 'tables/'))) dir.create(paste0(OUTPUT_PATH, 'tables/'))

    ## DOCS_PATH <<- "../docs/"
  },
  stage_ = 1,
  order_ = 3,
  name_ = "Configure paths",
  item_ = "configure_paths"
  )

}

ltmp_check_packages <- function() {
  status::status_try_catch(
  {
    missing <- ''
    options(tidyverse.quiet = TRUE)
    pkgs <- c(
      "tidyverse", "sf", "ggspatial", "INLA", "knitr", "broom.mixed",
      "DHARMa", "jsonlite", "rlang", "tidybayes", "crayon"
      ## "testthat",
      ## "rnaturalearth", "rnaturalearthdata", "patchwork", "ggnewscale",
      ## "inlabru", "cli", "stars", "geojsonR", "geojsonsf", "s2",
      ## "FRK", "spacetime", "ows4R", "httr"
    )
    for (p in pkgs) {
      ## unforunately we must do this the base r way until rlang is
      ## loaded
      eval(parse(text=paste0("suppressPackageStartupMessages(if(!require(",
                             p,",quietly = TRUE, warn.conflicts = FALSE)) missing <- c(missing, ",
                             p,"))")))
    }
    if (missing != "") {
      stop(paste(
        "The following required package(s) are missing: ",
        paste(missing, collapse = ", ")
      ))
    }
  },
  stage_ = 1,
  order_ = 4,
  name_ = "Load package dependencies",
  item_ = "load_packages"
  )
  
}

ltmp_analysis_stage <- function() {
    ## if (file.exists(paste0(DATA_PATH, "analysis_stage.RData"))) {
    ##     load(paste0(DATA_PATH, "analysis_stage.RData"))
    ## } else {
    ##     ANALYSIS_STAGE <<- list(list(type='component', value='01_start'))
    ##     save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "analysis_stage.RData"))
    ## }
}


## setwd("~/dev/R")
library(DBI)
library(RSQLite)
library(tidyverse)
library(digest)

## print(getwd())
## print(list.files())
print(Sys.info()[["user"]])
message(Sys.info()[["user"]])

## args <- c(
##   "Rscript",
##   "batch.R",
##   "--purpose=sql,post-process,prepare",
##   "--method=manta",
##   "--scale=reef",
##   "--log=../../data/dashboard.log"
##   )
args <- c(
  "Rscript",
  "batch.R",
  "--purpose=sql",
  "--method=fish",
  "--scale=NULL",
  "--log=../../data/dashboard.log"
  )
## args <- c(
##   "Rscript",
##   "batch.R",
##   "--purpose=sql,post-process,prepare",
##   "--method=manta",
##   "--scale=reef",
##   "--domain='Reef 14-133'",
##   "--log=../../data/dashboard.log"
##   )

args <- commandArgs()
print(args)

has_file_argument <- any(grepl("--file=.*", args, perl = TRUE))
if(has_file_argument) {
  arg <- args[grep("--file=.*", args)]
  file <- gsub("--file=(.)", "\\1", arg)
  setwd(dirname(file))
} else stop("No --file= component provided")

## print(getwd())
## print(list.files())
source("backend_functions.R")
## sink(file = "../../data/test.log")
## print(args)

has_method_argument <- any(grepl("--method=.*", args, perl = TRUE))
if(has_method_argument) {
  arg <- args[grep("--method=.*", args)]
  data_type <- gsub("--method=(.)", "\\1", arg)
  sql_file <- case_when(
    data_type == "photo-transect" ~ "pt",
    data_type == "manta" ~ "manta",
    data_type == "juvenile" ~ "juv",
    data_type == "fish" ~ "fish",
    .default = NULL
    )
}
has_scale_argument <- any(grepl("--scale=.*", args, perl = TRUE))
if(has_scale_argument) {
  arg <- args[grep("--scale=.*", args)]
  data_scale <- gsub("--scale=(.)", "\\1", arg)
  scale <- case_when(
    data_scale == "reef" ~ "reefs",
    data_scale == "nrm" ~ "nrm",
    data_scale == "sector" ~ "sectors",
    .default = NULL
    )
} else scale <- NULL

has_domain_argument <- any(grepl("--domain=.*", args, perl = TRUE))
if(has_domain_argument) {
  arg <- args[grep("--domain=.*", args)]
  domain <- gsub("--domain=(.)", "\\1", arg)
} else {
  domain <- NULL
}
## has_fit_argument <- any(grepl("--fit_model=.*", args, perl = TRUE))
## if(has_fit_argument) {
##   arg <- args[grep("--fit_model=.*", args)]
##   fit_model <- as.logical(gsub("--fit_model=(.)", "\\1", arg))
## } else fit_model <- FALSE

has_log_argument <- any(grepl("--log=.*", args, perl = TRUE))
if(has_log_argument) {
  arg <- args[grep("--log=.*", args)]
  out_log_file <- gsub("--log=(.)", "\\1", arg)
}

has_purpose_argument <- any(grepl("--purpose=.*", args, perl = TRUE))
if(has_purpose_argument) {
  arg <- args[grep("--purpose=.*", args)]
  purpose <- gsub("--purpose=(.)", "\\1", arg) |>
    strsplit(",") |>
    ## _[[1]] |>
    pluck(1) |> 
    str_trim()
} else purpose <- NULL


config_ <- list(
  data_path = "../../data/",
  model_path = "../../dev/data/modelled/", 
  ## dashboard_log = "../../data/dashboard.log"
  ## dashboard_log = paste0("../../data/", out_log_file)
  dashboard_log = out_log_file
  )
config_$db_path = paste0(config_$data_path, "dashboard.sqlite")
message(paste0("Current working directory is:", getwd()))
message(config_$dashboard_log)
## unlink(config_$dashboard_log)
## unlink(gsub(".log", ".old", config_$dashboard_log))

## print(purpose)
## print(data_type)
## print(data_scale)
## print(scale)
## print(sql_file)
## print(config_$dashboard_log)
## sink()
if ("sql" %in% purpose) {  ## Extract from Oracle database
  print("Start extraction")
  ## system(paste0("java -jar ../dbExport.jar ../../dashboard/data/",
  ##               sql_file, ".sql ../../data/",
  ##               data_type, ".csv reef reefmon >> ",
  ##               paste0(config_$dashboard_log,"1"), " 2>&1"))
  ##               ## config_$dashboard_log, " 2>&1"))
  ## out <- system2("ls", args = c("-lat"),
  ## system2("ls", args = c("-lat"),
  system2("java",
                 args = c("-jar",
                          "../dbExport.jar",
                          shQuote(paste0("../../dashboard/data/", sql_file, ".sql")),
                          shQuote(paste0("../../data/", data_type, ".csv")),
                          "reef",
                          ## "reefmon"),
                          "reefmon",
                          ## paste0(">> ../../data/error.log 2>&1")),
                          paste0(">> ", config_$dashboard_log, " 2>&1")),
                          ## shQuote(paste0(">> ", "../../data/error.log", " 2>&1"))),
                          ## shQuote(paste0(">> ", config_$dashboard_log, " 2>&1"))),
                 wait = TRUE,
          ## stdout = paste0(">> ", config_$dashboard_log),
          ## stdout = "../../data/error.log",
          ## stdout = sub("dashboard", "dash", config_$dashboard_log)
          stderr = sub("dashboard", "dashboard_error", config_$dashboard_log)
          )
  print("Extraction complete")
  print("Update databases")
  create_db_table_from_extract(config_$db_path,
                               data_type = data_type,
                               paste0("../../data/", data_type, ".csv"))
  create_db_summary_from_extract(config_$db_path,
                                 data_type = data_type,
                                 paste0("../../data/", data_type, ".csv"))
  print("Update databases complete")
  if (data_type == "fish") {
    system2("java",
            args = c("-jar",
                     "../dbExport.jar",
                     shQuote(paste0("../../dashboard/data/fish_codes.sql")),
                     shQuote(paste0("../../data/fish_codes.csv")),
                     "reef",
                     "reefmon",
                     paste0(">> ", config_$dashboard_log, " 2>&1")),
            wait = TRUE,
            stderr = sub("dashboard", "dashboard_error", config_$dashboard_log)
            )
  }
}

if ("post-process" %in% purpose) { ## Run post-processing script
  print("Start post-process")
  system(paste0("Rscript process_db_extract.R --method=", data_type,
                " --purpose=post-process --csv_file=../../data/", data_type, ".csv >> ",
                config_$dashboard_log, " 2>&1"))
  print("Post-process complete")
}

if ("prepare" %in% purpose) {  ## Place data in domain_name specific folders
  print("Start prepare data")
  if (is.null(scale)) {
    scale <- c("reefs", "nrm", "sectors")
    data_scale <- c("reef", "nrm", "sector") 
  }
  for (i in seq_along(scale)) {
    print(paste0("Preparing data for ", scale[i]," analyses"))
    warning(paste0("Preparing data for ", scale[i]," analyses"))
    system(paste0("Rscript process_db_extract.R --method=", data_type,
                  " --purpose=make_", scale[i],
                  " --rds_file=../../data/", data_type, ".rds >> ",
                  config_$dashboard_log, " 2>&1"))
    warning("About to make the db table")
    update_db_summary_tables(method = data_type, scale = data_scale[i], domain = NULL)
  }
  print("Prepare data complete")
}

if ("fit" %in% purpose) {  ## Fit models
  print("Start fit models")
  ## domains <- which_models_to_update(method = data_type, scale = data_scale, all = TRUE) |>
  ##   str_subset("^NA$", negate = TRUE)
  print(domain)
  if (!all(is.null(domain) | domain == "NULL")) {
    domains <- domain
  } else {
    ## print("here")
    ## print(data_type)
    ## print(data_scale)
    domains <- which_models_to_update(method = data_type, scale = data_scale, all = FALSE) |>
      str_subset("^NA$", negate = TRUE)
    print("The domains")
    print(domains)
  }
  print(paste0("Fit models for ", domains))
  message(paste0("Fit models for ", domains))
  if(length(domains)>0) {
    process <- processx::process$new("Rscript", 
                                     args = c("run_models.R",
                                              paste0("--method=", data_type),
                                              paste0("--scale=", data_scale),
                                              paste0("--domain=", domains),
                                              paste0("--log=", config_$dashboard_log)),
                                     stdout = config_$dashboard_log
                                     ## stderr =  config_$dashboard_log
                                     )
    ## process$is_alive()
    process$wait()
    update_db_model_hash(method = data_type, scale = data_scale, domain = domains)
  }
  print("Fit models complete")
}

if (1 == 2) {
  


system(paste0("Rscript process_db_extract.R --method=manta --purpose=post-process --csv_file=../../data/manta.csv >> ", config_$dashboard_log, " 2>&1"))

## Reefs
system(paste0("Rscript process_db_extract.R --method=manta --purpose=make_reefs --rds_file=../../data/manta.rds >> ", config_$dashboard_log, " 2>&1"))
update_db_summary_tables(method = "manta", scale = "reef", domain = NULL)
domains <- which_models_to_update(method = "manta", scale = "reef", all = FALSE) |>
  str_subset("^NA$", negate = TRUE)
if(length(domains)>0) {
  process <- processx::process$new("Rscript", 
                                   args = c("run_models.R",
                                            paste0("--method=manta"),
                                            paste0("--scale=reef"),
                                            paste0("--domain=", domains),
                                            paste0("--log=", config_$dashboard_log)),
                                   stdout = config_$dashboard_log
                                   ## stderr =  config_$dashboard_log
                                   )
  ## process$is_alive()
  process$wait()
  update_db_model_hash(method = "manta", scale = "reef", domain = domains)
}


## Run NRM regions
system(paste0("Rscript process_db_extract.R --method=manta --purpose=make_nrm --rds_file=../../data/manta.rds >> ", config_$dashboard_log, " 2>&1"))
update_db_summary_tables(method = "manta", scale = "nrm", domain = NULL)
domains <- which_models_to_update(method = "manta", scale = "nrm", all = FALSE) |>
  str_subset("^NA$", negate = TRUE)
if(length(domains)>0) {
  process <- processx::process$new("Rscript", 
                                   args = c("run_models.R",
                                            paste0("--method=manta"),
                                            paste0("--scale=nrm"),
                                            paste0("--domain=", domains),
                                            paste0("--log=", config_$dashboard_log)),
                                   stdout = config_$dashboard_log
                                   ## stderr =  config_$dashboard_log
                                   )
  process$wait()
  update_db_model_hash(method = "manta", scale = "nrm", domain = domains)
}


## Run all sectors
system(paste0("Rscript process_db_extract.R --method=manta --purpose=make_sectors --rds_file=../../data/manta.rds >> ", config_$dashboard_log, " 2>&1"))
update_db_summary_tables(method = "manta", scale = "sector", domain = NULL)
domains <- which_models_to_update(method = "manta", scale = "sector", all = FALSE) |>
  str_subset("^NA$", negate = TRUE)
if(length(domains)>0) {
  process <- processx::process$new("Rscript", 
                                   args = c("run_models.R",
                                            paste0("--method=manta"),
                                            paste0("--scale=sectors"),
                                            paste0("--domain=", domains),
                                            paste0("--log=", config_$dashboard_log)),
                                   stdout = config_$dashboard_log
                                   ## stderr =  config_$dashboard_log
                                   )
  process$wait()
  update_db_model_hash(method = "manta", scale = "sector", domain = domains)
}


## con <- dbConnect(RSQLite::SQLite(), config_$db_path)
## db_sum_tbl <- paste0(method, "_", scale, "_sum")
## tb <- tbl(con, db_sum_tbl) |>
##   collect()
## dbDisconnect(con)

## con <- dbConnect(RSQLite::SQLite(), config_$db_path)
## tb <- tbl(con, "models") |>
##   collect()
## dbDisconnect(con)
## tb |> filter(data_type == "manta", data_scale == "nrm") |>
##   as.data.frame()

## tb |> filter(data_type == "manta", data_scale == "sector") |>
##   as.data.frame()

}


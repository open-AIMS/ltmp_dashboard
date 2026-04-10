## setwd("~/dev/R")
library(DBI)
library(RSQLite)
library(tidyverse)
library(digest)


## print(getwd())
## print(list.files())
print(Sys.info()[["user"]])

## The "messages" get directed to ~/dashboard/log/dashboard_error.log
message(Sys.info()[["user"]])
message(paste0("Current working directory is:", getwd()))

## args <- c(
##   "Rscript",
##   "batch.R",
##   "--purpose=sql,post-process,prepare",
##   "--method=manta",
##   "--scale=reef",
##   "--log=../../data/dashboard.log"
##   )
## args <- c(
##   "Rscript",
##   "batch.R",
##   "--purpose=sql",
##   "--method=fish",
##   "--scale=NULL",
##   "--log=../../data/dashboard.log"
##   )
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
cat(paste(capture.output(args), collapse = "\n"))
message(paste(capture.output(args), collapse = "\n"))

has_log_argument <- any(grepl("--log=.*", args, perl = TRUE))
if(has_log_argument) {
  arg <- args[grep("--log=.*", args)]
  out_log_file <- gsub("--log=(.)", "\\1", arg)
}
cat(paste0("Current working directory:", getwd()), "\n", file = out_log_file, append = TRUE)
cat(paste0("Log file working directory:", out_log_file), "\n", file = out_log_file, append = TRUE)

has_file_argument <- any(grepl("--file=.*", args, perl = TRUE))
if(has_file_argument) {
  arg <- args[grep("--file=.*", args)]
  file <- gsub("--file=(.)", "\\1", arg)
  cat(paste0("File:", file), "\n", file = out_log_file, append = TRUE)
  setwd(dirname(file))
} else stop("No --file= component provided")

## The following must remain here (e.g. after working directory has been set)
source("backend_functions.R")

## print(getwd())
## print(list.files())
## sink(file = "../../data/test.log")
## print(args)

has_method_argument <- any(grepl("--method=.*", args, perl = TRUE))
if(has_method_argument) {
  arg <- args[grep("--method=.*", args)]
  data_type <- gsub("--method=(.)", "\\1", arg)
  sql_file <- case_when(
    data_type == "photo-transect" ~ "pt",
    data_type == "manta" ~ "manta",
    data_type == "juveniles" ~ "juv",
    data_type == "fish" ~ "fish",
    .default = NULL
    )
  cat(paste0("Data type:", data_type), "\n", file = out_log_file, append = TRUE)
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
cat(paste0("Data scale:", scale), "\n", file = out_log_file, append = TRUE)

has_domain_argument <- any(grepl("--domain=.*", args, perl = TRUE))
if(has_domain_argument) {
  arg <- args[grep("--domain=.*", args)]
  domain <- gsub("--domain=(.)", "\\1", arg)
} else {
  domain <- NULL
}
cat(paste0("Domain:", domain), "\n", file = out_log_file, append = TRUE)

## has_fit_argument <- any(grepl("--fit_model=.*", args, perl = TRUE))
## if(has_fit_argument) {
##   arg <- args[grep("--fit_model=.*", args)]
##   fit_model <- as.logical(gsub("--fit_model=(.)", "\\1", arg))
## } else fit_model <- FALSE


has_purpose_argument <- any(grepl("--purpose=.*", args, perl = TRUE))
if(has_purpose_argument) {
  arg <- args[grep("--purpose=.*", args)]
  purpose <- gsub("--purpose=(.)", "\\1", arg) |>
    strsplit(",") |>
    ## _[[1]] |>
    pluck(1) |>
    str_trim()
} else purpose <- NULL
cat(paste0("Batch purpose:", purpose), "\n", file = out_log_file, append = TRUE)


has_sql_argument <- any(grepl("--sql_path=.*", args, perl = TRUE))
if(has_sql_argument) {
  arg <- args[grep("--sql_path=.*", args)]
  sql_path <- gsub("--sql_path=(.)", "\\1", arg)
} else {
  sql_path <- NULL
}
cat(paste0("SQL path:", sql_path), "\n", file = out_log_file, append = TRUE)

has_data_argument <- any(grepl("--data_path=.*", args, perl = TRUE))
if(has_data_argument) {
  arg <- args[grep("--data_path=.*", args)]
  data_path <- gsub("--data_path=(.)", "\\1", arg)
} else {
  data_path <- NULL
}
cat(paste0("Data path:", data_path), "\n", file = out_log_file, append = TRUE)

config_ <- list(
  data_path = data_path, #"../../data/",
  data_file = paste0(data_path, data_type, ".csv"),
  sql_path = sql_path,
  sql_file = paste0(sql_path, sql_file, ".sql"),
  model_path = paste0(data_path, "modelled/"), #"../../dev/data/modelled/",
  dashboard_log = out_log_file,
  db_path = paste0(data_path, "dashboard.sqlite")
  )
cat(paste0("SQL file:", config_$sql_file), "\n", file = out_log_file, append = TRUE)
cat(paste0("Internal db path:", config_$db_path), "\n", file = out_log_file, append = TRUE)
## config_$db_path = paste0(config_$data_path, "dashboard.sqlite")
## message(paste0("Current working directory is:", getwd()))
## message(config_$dashboard_log)
## message(paste0("Purpose is:", purpose))


if ("sql" %in% purpose) {  ## Extract from Oracle database
  print("Start extraction")
  cat("Start execution...", "\n\n", file = out_log_file, append = TRUE)
  if (sql_file == "juv") {
    sql_file1 <- gsub(".sql$", "_1.sql", config_$sql_file)
    data_file1 <- gsub(".csv$", "_1.csv", config_$data_file)
    cat("data_file1:", data_file1, "\n", file = out_log_file, append = TRUE)
    system2("java",
            args = c("-jar",
                     "../dbExport.jar",
                     ## shQuote(paste0("../../dashboard/data/", sql_file1, ".sql")),
                     ## shQuote(paste0("../../data/", data_type1, ".csv")),
                     sql_file1,
                     data_file1,
                     "reef",
                     "reefmon",
                     paste0(">> ", config_$dashboard_log, " 2>&1")),
            wait = TRUE,
            stderr = sub("dashboard.log$", "dashboard_error.log", config_$dashboard_log)
            )
    cat("", "\n", file = out_log_file, append = TRUE)
    ## sql_file2 <- paste0(sql_file,"_2")
    ## data_type2 <- paste0(data_type, "_2")
    sql_file2 <- gsub(".sql$", "_2.sql", config_$sql_file)
    data_file2 <- gsub(".csv$", "_2.csv", config_$data_file)
    system2("java",
            args = c("-jar",
                     "../dbExport.jar",
                     ## shQuote(paste0("../../dashboard/data/", sql_file2, ".sql")),
                     ## shQuote(paste0("../../data/", data_type2, ".csv")),
                     sql_file2,
                     data_file2,
                     "reef",
                     "reefmon",
                     paste0(">> ", config_$dashboard_log, " 2>&1")),
            wait = TRUE,
            stderr = sub("dashboard.log$", "dashboard_error.log", config_$dashboard_log)
            )
    ## The following (as well as the sql's) was provided by Angus (although I have changed filenames)
    ##- add in zeros prior to any aggregation
    ## points <- read_csv(paste0("../../data/", data_type1, ".csv"))  |>
    points <- read_csv(data_file1)  |>
      mutate(SITE_DEPTH = ifelse(P_CODE == "IN", SITE_DEPTH, 9))

    avail_points_transect <- points |>
      filter(GROUP_CODE == "A" | BENTHOS_CODE == 'ST') |>
      group_by(P_CODE, AIMS_REEF_NAME, REEF_ZONE, SITE_DEPTH,
               VISIT_NO, SITE_NO, TRANSECT_NO)  |>
      summarise(av_points = sum(POINTS)) |>
      ungroup()

    avail_proportion_transect <- points  |>
      group_by(P_CODE, AIMS_REEF_NAME, REEF_ZONE, SITE_DEPTH,
               VISIT_NO, SITE_NO, TRANSECT_NO)  |>
      summarise(total_points = sum(POINTS)) |>
      ungroup() |>
      left_join(avail_points_transect) |>
      mutate(av_points = ifelse(is.na(av_points), 0, av_points),
             AVAILABLE_SUBSTRATE = av_points / total_points)

    avail_substrate_site <- avail_proportion_transect |>
      group_by(P_CODE, AIMS_REEF_NAME, REEF_ZONE,
               SITE_DEPTH, VISIT_NO, SITE_NO)  |>
      summarise(AVAILABLE_SUBSTRATE=mean(AVAILABLE_SUBSTRATE)) |>
      ungroup() |>
      mutate(AREA_TRANSECT = ifelse(P_CODE %in% c('RM','RAP','RMRAP'), 8.5,
                           ifelse(VISIT_NO < 3, 17, 34)))

    ## juv_data <- read_csv(paste0("../../data/", data_type2, ".csv"))  |>
    juv_data <- read_csv(data_file2)  |>
      mutate(SITE_DEPTH = ifelse(P_CODE == 'IN', SITE_DEPTH, 9)
             ## SURVEY_DATE = as.Date(SURVEY_DATE, format = "%d/%m/%Y")
             ) |>
      rename(REEFPAGE_CATEGORY = REEF_PAGE_CATEGORY) |>
      left_join(avail_substrate_site)
    ## write_csv(juv_data, file = paste0("../../data/", data_type, ".csv"))
    write_csv(juv_data, file = paste0(config_$data_path, data_type, ".csv"))
  } else {
    system2("java",
            args = c("-jar",
                     "../dbExport.jar",
                     ## shQuote(paste0("../../dashboard/data/", sql_file, ".sql")),
                     config_$sql_file,
                     ## shQuote(paste0("../../data/", data_type, ".csv")),
                     config_$data_file,
                     "reef",
                     "reefmon",
                     paste0(">> ", config_$dashboard_log, " 2>&1")),
            wait = TRUE,
            stderr = sub("dashboard.log$", "dashboard_error.log", config_$dashboard_log)
            )
  }
  print("Extraction complete")
  print("Update databases")
  create_db_table_from_extract(config_$db_path,
                               data_type = data_type,
                               csv_file = config_$data_file)
  create_db_summary_from_extract(config_$db_path,
                                 data_type = data_type,
                               csv_file = config_$data_file)
  print("Update databases complete")
  if (data_type == "fish") {
    cat("Start execution of fish_codes...", "\n", file = out_log_file, append = TRUE)
    system2("java",
            args = c("-jar",
                     "../dbExport.jar",
                     shQuote(paste0(sql_path, "fish_codes.sql")),
                     shQuote(paste0(data_path, "fish_codes.csv")),
                     "reef",
                     "reefmon",
                     paste0(">> ", config_$dashboard_log, " 2>&1")),
            wait = TRUE,
            stderr = sub("dashboard.log$", "dashboard_error.log", config_$dashboard_log)
            )
  }
  cat("Database extraction complete...", "\n", file = out_log_file, append = TRUE)
}

if ("post-process" %in% purpose) { ## Run post-processing script
  print("Start post-process")
  cat("Start post-process...", "\n\n", file = out_log_file, append = TRUE)
  csv_file <- paste0(config_$data_path, data_type, ".csv")
  cat(csv_file, "\n\n", file = out_log_file, append = TRUE)
  system(paste0("Rscript process_db_extract.R --method=", data_type,
                ## " --purpose=post-process --csv_file=../../data/", data_type, ".csv >> ",
                " --purpose=post-process --csv_file=", data_path, data_type, ".csv >> ",
                config_$dashboard_log, " 2>&1"))
  print("Post-process complete")
}

if (1 == 2) {

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
  ## print(paste0("Fit models for ", domains, " data scale is: ", data_scale))
  message(paste0("Fit models for ", domains, " data scale: ", data_scale, " data_type: ", data_type ))
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
    message(process$get_exit_status())
    message(process$get_error_file())
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

}

## Functions =========================================================

## Check if the "models" db table exists
## If it does, extract the entries
## If no, get those info from the data structures
## Filter the entries to relevant candidates

get_time <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
}

get_candidates <- function(tab_name, data_types, scale, domain = NULL) {
  cat("Inside get_candidates (", get_time(), ")\n",
    file = config_$dashboard_log, append = TRUE)
  cat("data_type: (", get_time(), ") ", data_types,
    ",\tscale: ", scale,
    ",\tdomain: ", domain,
    "\n",
    file = config_$dashboard_log, append = TRUE)
  ## alert("here")
  if(!does_db_table_exist("models")) {
    ## models <- get_config_models() |>
    ##     filter(data_type %in% data_types,
    ##            data_scale == scale,
    ##            !is.na(model_path))
    models <- NULL
  } else {
    models <- get_db_model_data(method = NULL, scale = scale, domain = domain)
    ## cat("MODELS (", get_time(), ") ",
    ##   capture.output(print(as.data.frame(models))),
    ##   "\n", sep = "\n",
    ##   file = config_$dashboard_log, append = TRUE)
    ## narrow to only those with a path, and of the correct data_type/method
    if (!is.null(models))
      models <- models |>
        filter(data_type %in% data_types,
               data_scale == scale,
               !is.na(model_path))
    ## alert(scale)
    ##   alert(unique(models$data_scale))
    ## write_csv(models, file = paste0("~/data/", "candidate_models_1.csv"))

    ## write_csv(models, file = paste0(config_$data_path, "candidate_models_2.csv"))
  }
  if (is.null(models))
    models <- blank_candidate_models() |>
        filter(data_type %in% data_types,
               data_scale == scale,
               !is.na(model_path))

  ## config_$model <- models
  ## assign("config_", config_, envir = .GlobalEnv)
  ## cat(file = stderr(), paste("refresh pressed:", models), "\n")
  current_candidates <- models
    ## filter(data_scale == scale,
    ##        data_type == reefs_tab_lookup[[tab_name]]$data_type)
  ## write_csv(current_candidates, file = paste0(config_$data_path, "candidate_models.csv"))
  current_candidates
}


extract_db_cases <- function(table_name, d_name) {
  if(does_db_table_exist(table_name)) {
    con <- dbConnect(RSQLite::SQLite(), config_$db_path)
    data <- tbl(con, table_name) |>
      filter(domain_name %in% d_name) |>
      collect() ##|>

    dbDisconnect(con)
    return(data)
  }
}

blank_candidate_models <- function() {
  data.frame(path = "",
             data_type = "",
             model_type = "", #
             data_scale = "",
             domain_name = "",
             group = "",
             family_type = "",
             reef_zone = "",
             depth = "",
             shelf = "",
             sub_model = "",
             selected_flag = ""
             )
}

## Extract the "models" table data

## create a database table out of the extracted data
create_db_table_from_extract <- function(db_path, data_type, csv_file) {
  out <- system(sprintf("sqlite3 %s 'DROP TABLE IF EXISTS \"%s\"';",
                        db_path, data_type),
                wait = TRUE, intern = TRUE)
  out <- system(sprintf("sqlite3 %s '.mode csv' '.headers on' '.import \"%s\" \"%s\"';",
                        db_path, csv_file, data_type),
                wait = TRUE, intern = TRUE)
}

create_db_summary_from_extract <- function(db_path, data_type, csv_file) {
  cat("\ndata_type:", data_type,
    file = "/export/project/monitoring-dashboard/log/a.log", append = TRUE)

  if (data_type == "annual_report") {
    cat("\ncsv_file:", csv_file,
      file = "/export/project/monitoring-dashboard/log/a.log", append = TRUE)
    out2 <- readRDS(csv_file) |>
      unnest(data) |>
      group_by(Region) |>
      summarise(SURVEY_DATE = max(SURVEY_DATE)) |>
      dplyr::rename_with(tolower)   ## the internal dashboard standard is for lowercase
    ## cat("\nnrow:", nrow(out2),
    ##   file = "/export/project/monitoring-dashboard/log/a.log", append = TRUE)
    ## cat("\ndata:", out2,
    ##   file = "/export/project/monitoring-dashboard/log/a.log", append = TRUE)
  }
  if (data_type == "photo-transect") {
    out2 <- system(paste0(
      ## "cut -d, -f17,6,28,9 < ", csv_file, " | uniq |
      "cut -d, -f17,6,29,9 < ", csv_file, " | uniq |
    Rscript -e \"library(dplyr); library(readr); input <- read_csv(stdin());
    input  <- input |> mutate(SURVEY_DATE = as.POSIXct(SURVEY_DATE, format='%d-%b-%Y %H:%M:%S')) |>
    group_by(NRM_REGION, A_SECTOR, AIMS_REEF_NAME) |>
    summarise(SURVEY_DATE=max(SURVEY_DATE));
    write.csv(input, row.names=FALSE)\"
    "
    ),
    intern = TRUE)
  }
  if (data_type == "manta") {
    out2 <- system(paste0(
      "cut -d, -f5,6,10,15 < ", csv_file, " | uniq |
    Rscript -e \"library(dplyr); library(readr); input <- read_csv(stdin());
    input  <- input |> mutate(SURVEY_DATE = as.POSIXct(SURVEY_DATE, format='%d-%b-%Y %H:%M:%S')) |>
    group_by(NRM_REGION, A_SECTOR, AIMS_REEF_NAME) |>
    summarise(SURVEY_DATE=max(SURVEY_DATE));
    write.csv(input, row.names=FALSE)\"
    "
    ),
    intern = TRUE)
  }
  if (data_type == "fish") {
    out2 <- system(paste0(
      "cut -d, -f7,9,20,29 < ", csv_file, " | uniq |
    Rscript -e \"library(dplyr); library(readr); input <- read_csv(stdin());
    input  <- input |> mutate(SURVEY_DATE = as.POSIXct(SURVEY_DATE, format='%d-%b-%Y %H:%M:%S')) |>
    group_by(NRM_REGION, A_SECTOR, AIMS_REEF_NAME) |>
    summarise(SURVEY_DATE=max(SURVEY_DATE));
    write.csv(input, row.names=FALSE)\"
    "
    ),
    intern = TRUE)
  }
  if (data_type == "juveniles") {
    out2 <- system(paste0(
      "cut -d, -f4,6,7,15 < ", csv_file, " | uniq |
    Rscript -e \"library(dplyr); library(readr); input <- read_csv(stdin());
    input  <- input |> mutate(SURVEY_DATE = as.POSIXct(SURVEY_DATE, format='%d-%b-%Y %H:%M:%S')) |>
    group_by(NRM_REGION, A_SECTOR, AIMS_REEF_NAME) |>
    summarise(SURVEY_DATE=max(SURVEY_DATE));
    write.csv(input, row.names=FALSE)\"
    "
    ),
    intern = TRUE)
  }

  cat("\nnrow:", nrow(out2),
    file = "/export/project/monitoring-dashboard/log/a.log", append = TRUE)
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  ## dbWriteTable(con, paste0(data_type, "_sum1"), out2, overwrite = TRUE)
  data_info <- file.mtime(csv_file)
  if (data_type == "annual_report") {
    data <- as.data.frame(out2) |>
      mutate(
        survey_date = as.POSIXct(survey_date, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"),
        extraction_date = data_info
      )
  } else {
    data <- as.data.frame(out2) |>
      slice(-1)  |>
      separate(everything(),
        into = c("nrm", "sector", "reef", "survey_date"), sep = ",")  |>
      mutate(## reef = str_replace_all(reef, '"', ""),
        across(everything(), ~ str_replace_all(.x, '"', "")),
        survey_date = as.POSIXct(survey_date, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"),
        ## extraction_date = format(data_info, "%Y-%m-%d %H:%M:%S"))
        extraction_date = data_info)
    ## add additional fields representing model hashes etc
    ## extra_fields <- c("sector_extract_data_hash" = NA,
    ##          "sector_model_date" = NA,
    ##          "sector_model_data_hash" = NA,
    ##          "nrm_extract_data_hash" = NA,
    ##          "nrm_model_date" = NA,
    ##          "nrm_model_data_hash" = NA,
    ##          "reef_extract_data_hash" = NA,
    ##          "reef_model_date" = NA,
    ##          "reef_model_data_hash" = NA,
    ##          "sector_model_data_hash_flag" = NA,
    ##          "nrm_model_data_hash_flag" = NA,
    ##          "reefe_model_data_hash_flag" = NA
    ##          )
    ## data <-
    ##   data |>
    ##   add_column(!!!extra_fields[setdiff(names(extra_fields), names(data))])
    ## print(head(data))
  }

  cat("\nnrow2:", nrow(data),
    file = "/export/project/monitoring-dashboard/log/a.log", append = TRUE)
  dbWriteTable(con, paste0(data_type, "_sum"), data, overwrite = TRUE)
  dbDisconnect(con)
}

## retrieve the first 20 rows of a database raw data table
make_dashboard_data <- function(method, scale) {
  data <- tibble("Blank")
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  tbls <- dbListTables(con)
  if (method %in% tbls) {
    data <- tbl(con, method) |>
      head(20) |>
      collect()
  }
  dbDisconnect(con)
 return(data)
}

## retrieve the summary data from the database
make_dashboard_summary <- function(method, scale) {
  ## cat(paste0("db_path:", config_$db_path), "\n",  file = config_$dashboard_log, append = TRUE)
  cat(paste0("\n\t Inside Make dashboard summary"), "\n",
    file = config_$dashboard_log, append = TRUE)
  cat(paste0("\n\t\tmethod: ", method), "\n",
    file = config_$dashboard_log, append = TRUE)
  cat(paste0("\n\t\tscale: ", scale), "\n",
    file = config_$dashboard_log, append = TRUE)
  data <- tibble("Blank")
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  tbls <- dbListTables(con)
  ## alert(tbls)
  ## cat(paste0("tbls:", tbls), "\n",  file = config_$dashboard_log, append = TRUE)
  if (scale == "Sectors") scale <- "sector"
  if (scale %in% c("reef", "sql") &
      paste0(method, "_sum") %in% tbls) {
    if (scale == "sql") {
      db_tbl <- paste0(method, "_sum")
      ## cat(paste0("db_tbl:", db_tbl), "\n",  file = config_$dashboard_log, append = TRUE)
      data <- tbl(con, db_tbl)
      ## alert(paste("make_dashboard_summary:",db_tbl))
      ## alert(method)
      ## alert(colnames(data))
      ## alert(nrow(data))
      ## alert(method)
      ## sector
      if (paste0(method, "_sector_sum") %in% tbls) {
        data <- data |>
          left_join(
            tbl(con, paste0(method, "_sector_sum")) |>
            dplyr::select(domain_name, extract_data_hash) |>
            distinct(),
            by = c("sector" = "domain_name")) |>
          dplyr::rename(sector_extract_data_hash = extract_data_hash)
          if ("models" %in% tbls) {
            data <- data  |>
              left_join(tbl(con, "models") |>
                        filter(data_type == method,
                               data_scale == "sector") |>
                        select(domain_name, sector_model_date = model_date,
                               sector_model_data_hash),
                        by = c("sector" = "domain_name"))
          }
        ## ttt <- tbl(con, "models") |> collect()
        ## alert(head(ttt$model_date))
        ## alert(colnames(data))
      } else {
        ## hash <- digest(paste0(config_$data_path, method, ".csv"),
        ##                algo = "sha256", file = TRUE)
        hash <- NA
        data <- data |>
        ##   collect() |>
          mutate(sector_extraction_data_hash = hash,
                 sector_model_date = NA,
                 sector_model_data_hash = NA)
        ## alert(colnames(data))
      }
      ## nrm
      if (paste0(method, "_nrm_sum") %in% tbls) {
        data <- data |>
          left_join(
            tbl(con, paste0(method, "_nrm_sum")) |>
            dplyr::select(domain_name, extract_data_hash) |>
            distinct(),
            by = c("nrm" = "domain_name")) |>
          dplyr::rename(nrm_extract_data_hash = extract_data_hash)
        if ("models" %in% tbls) {
          data <- data |>
          left_join(tbl(con, "models") |>
                    filter(data_type == method,
                           data_scale == "nrm") |>
                    select(domain_name, nrm_model_date = model_date,
                           nrm_model_data_hash),
                    by = c("nrm" = "domain_name"))
        }
      } else {
        ## hash <- digest(paste0(config_$data_path, method, ".csv"),
        ##                algo = "sha256", file = TRUE)
        hash <- NA
        data <- data |>
        ##   collect() |>
          mutate(nrm_extraction_data_hash = hash,
                 nrm_model_date = NA,
                 nrm_model_data_hash = NA)
      }
      ## reef
      if (paste0(method, "_reef_sum") %in% tbls) {
        data <- data |>
          left_join(
            tbl(con, paste0(method, "_reef_sum")) |>
            dplyr::select(domain_name, extract_data_hash) |>
            distinct(),
            by = c("reef" = "domain_name")) |>
          dplyr::rename(reef_extract_data_hash = extract_data_hash)
        if ("models" %in% tbls) {
          data <- data |>
            left_join(tbl(con, "models") |>
                      filter(data_type == method,
                             data_scale == "reef") |>
                      select(domain_name, reef_model_date = model_date,
                             reef_model_data_hash),
                      by = c("reef" = "domain_name"))
        }
      } else {
        ## hash <- digest(paste0(config_$data_path, method, ".csv"),
        ##                algo = "sha256", file = TRUE)
        hash <- NA
        data <- data |>
        ##   collect() |>
          mutate(reef_extraction_data_hash = hash,
                 reef_model_date = NA,
                 reef_model_data_hash = NA)
      }
      data <- data |>
        collect() |>
        distinct()

    } else if (paste0(method, "_sum") %in% tbls &
               paste0(method, "_reef_sum") %in% tbls) { ## e.g. it is "reef"
      db_tbl <- paste0(method, "_sum")
      data <- tbl(con, db_tbl)  |>
        dplyr::select(-nrm, -sector)  |>
        left_join(
          tbl(con, paste0(method, "_reef_sum")) |>
          dplyr::select(domain_name, extract_data_hash) |>
          distinct(),
          by = c("reef" = "domain_name")) |>
        dplyr::rename(reef_extract_data_hash = extract_data_hash)
      if ("models" %in% tbls) {
        data <- data |>
          left_join(tbl(con, "models") |>
                    filter(data_type == method,
                           data_scale == "reef") |>
                    select(domain_name, reef_model_date = model_date,
                           reef_model_data_hash),
                    by = c("reef" = "domain_name")) ## |>
          ## collect() |>
          ## distinct()
      }
      data <- data |> collect() |> distinct()
    }
  } else if (paste0(method, "_sum") %in% tbls &
             paste0(method, "_", scale, "_sum") %in% tbls) {
    scales_to_remove <- case_when(
      scale == "reef" ~ c("sector", "nrm"),
      scale == "sector" ~ c("reef", "nrm"),
      scale == "nrm" ~ c("reef", "sector"),
      scale == "region" ~ c("reef", "sector")
    )
    db_tbl <- paste0(method, "_sum")
    ## cat(paste0("db_tbl: ", db_tbl), "\n")
    if (scale == "region") {
      data <- tbl(con, db_tbl) |>
        dplyr::group_by(!!sym(scale)) |>
        summarise(survey_date = max(survey_date),
          extraction_date = min(extraction_date)) |>
        dplyr::ungroup()
      ## alert(colnames(data))
      ## data <- tbl(con, db_tbl)  |>
      ##   dplyr::group_by(!!sym(scale))
      ##   summarise(survey_date = max(survey_date),
      ##     extraction_date = min(extraction_date)) |>
      ##   dplyr::ungroup()
      ## return(NULL)
    } else {
      data <- tbl(con, db_tbl) |>
        dplyr::select(-all_of(scales_to_remove)) |>
        dplyr::group_by(!!sym(scale)) |>
        summarise(survey_date = max(survey_date),
          extraction_date = min(extraction_date)) |>
        dplyr::ungroup()  |>
        left_join(
          tbl(con, paste0(method, "_", scale, "_sum")) |>
            dplyr::select(domain_name, extract_data_hash) |>
            distinct(),
          by = setNames("domain_name", scale)) |>
        dplyr::rename(setNames("extract_data_hash", paste0(scale, "_extract_data_hash")))
      ## cat("\n\ndim of data:", dim(data), "\n")
      ## alert(dim(data))
    }
    if ("models" %in% tbls & !is.null(data)) {
      data <- data |>
        left_join(tbl(con, "models") |>
                    filter(data_type == method,
                      data_scale == scale) |>
                    select(domain_name,
                      !!sym(paste0(scale, "_model_date")) := model_date,
                      paste0(scale, "_model_data_hash")),
          by = setNames("domain_name", scale))
    }
    data <- data |>
      collect() |>
      distinct()
  ## } else if (paste0(method, "_sum") %in% tbls &                # so for annual_report
  ##            !paste0(method, "_", scale, "_sum") %in% tbls) {
  ##   db_tbl <- paste0(method, "_sum")
  ##   data <- tbl(con, db_tbl) |>
  ##     ## dplyr::select(-all_of(scales_to_remove)) |>
  ##     dplyr::group_by(!!sym(scale)) |>
  ##     summarise(survey_date = max(survey_date),
  ##               extraction_date = min(extraction_date)) |>
  ##     dplyr::ungroup()  |>
  ##     left_join(
  ##       tbl(con, paste0(method, "_", scale, "_sum")) |>
  ##       dplyr::select(domain_name, extract_data_hash) |>
  ##       distinct(),
  ##       by = setNames("domain_name", scale)) |>
  ##     dplyr::rename(setNames("extract_data_hash", paste0(scale, "_extract_data_hash")))

  }
  if (!is.null(data)) {
    ## alert(length(data$sector_model_date))
    ## alert(colnames(data))
    ## alert(data[88,])
    ##write_csv(data, file = "/home/mlogan/data/test.csv")
    ## alert(as.POSIXct(data$sector_model_date[11:20]))
    data <- data |>
      ## slice(70:80) |>
      mutate(across(ends_with("date"), as.POSIXct))
    ## alert(colnames(data))
  }
  dbDisconnect(con)
  return(data)
}

## Retrieve data from database models table
get_db_model_data<- function(method = NULL, scale = NULL, domain = NULL) {
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  ## DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  data <- 1
  data <- tbl(con, "models") |>
    collect() ##|>
    ## dplyr::select(data_type, data_scale, domain_name, matches(".*_hash"))
    ## dplyr::select(data_type, data_scale, domain_name, contains("_hash"))
  if (!is.null(method)) {
    data <- data |>
    filter(data_type == method)
  }
  if (!is.null(scale)) {
    data <- data |>
    filter(data_scale == scale)
  }
  if (!is.null(domain)) {
    data <- data |>
    filter(domain_name %in% domain)
  }
  dbDisconnect(con)
  return(data)
}

## trawl through model path and extract model metadata
## then join onto this, the model info in the database (if it exists)
## 1. trawl through model path and extract model metadata
## 2. determine whether the models database exists
## 2. if it does exist
##    2.1. read from the database
##    2.2. join in metadata
## 3. replace model database
update_db_models <- function() {
  ## Start by gleaning the info by scanning through the models folders
  fls <- list.files(config_$model_path,
                    recursive = TRUE, full.names = TRUE)
  ## write.csv(fls, file = "../data/temp1.csv")
  fls_wch <- str_detect(basename(fls), "^[^_]*_[^_]*_[^_]*\\.rds$")
  ## write.csv(fls_wch, file = "../data/temp2.csv")
  models <- tibble(path = unique(fls[fls_wch])) |>
    mutate(mod = map(.x = path,
                     .f = ~ {
                       ss <- readRDS(.x)$label |>
                                       unlist() |>
                                       unique()
                       selected_flag <- readRDS(.x)$selected
                       ## hash <- digest(.x, algo = "sha256", file = TRUE)
                       mtime <- file.mtime(.x)
                       tibble(str = ss) |>
                         separate(str, into = c("data_type",
                                                "data_scale",
                                                "domain_name",
                                                "group",
                                                "family_type",
                                                "reef_zone",
                                                "depth",
                                                "shelf",
                                                "model_type",
                                                "sub_model"),
                                  sep = "_") |>
                         mutate(model_date = mtime, #format(mtime, "%Y-%m-%d %H:%M:%S"),
                                ## model_path = .x,
                                reef_model_data_hash = "",
                                sector_model_data_hash = "",
                                nrm_model_data_hash = "",
                                annual_report_model_data_hash = "",
                                selected_flag) |>
                         as.data.frame()
                     })) |>
    unnest(c(mod)) |>
    dplyr::rename(model_path = path) |>
    mutate(data_scale = ifelse(data_scale == "Sectors", "sector", data_scale))

  write.csv(models, file = "../data/temp1.csv")
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  tbls <- dbListTables(con)
  dbDisconnect(con)
  if ("models" %in% tbls & 1 == 1) {
    models_db <- get_db_model_data()
    models <- models_db |>
      full_join(models |>
                dplyr::select(-matches(".*\\_hash$|^path$|^model\\_.*")),
                by = c("data_type" = "data_type",
                       "data_scale" = "data_scale",
                       "domain_name" = "domain_name",
                       "group" = "group",
                       "family_type" =  "family_type",
                       "reef_zone" = "reef_zone",
                       "depth" = "depth",
                       "shelf",
                       "model_type",
                       "sub_model",
                       "selected_flag"))
  }
  write_csv(models, file = paste0(config_$data_path, "models.csv"))
  con <- dbConnect(RSQLite::SQLite(), dbname = config_$db_path)
  copy_to(con, models, name = "models", temporary = FALSE, overwrite = TRUE)
  dbDisconnect(con)
  models
}

## Get a hash of the data after extracting, processing and bucketing
make_data_hash <- function(method, scale, domain) {
  method <- ifelse(method == "juveniles", "juvenile", method) ## for the juveniles method, the path is juvenile not juveniles
  data_paths <- paste0(config_$data_path, method)
  data_scale <- case_when(scale == "reef" ~ "reef",
                          scale == "sector" ~ "Sectors",
                          scale == "nrm" ~ "nrm",
                          scale == "region" ~ "Regions"
                          )
  ## alert(data_paths)
  fls <- list.files(data_paths,
                    recursive = TRUE, full.names = TRUE)
  ## alert(fls)
  fls_wch <- str_detect(fls, paste0(".*/process/[^/]*/[^/]*/[^/]*/",data_scale,"/.*.csv$"))
  models <- tibble(path = unique(fls[fls_wch])) |>
    mutate(data_type = str_replace(path, ".*/data/([^/]*)/.*", "\\1")) |>
    mutate(data_scale = str_replace(path, ".*/process/[^/]*/[^/]*/[^/]*/([^/]*)/.*", "\\1")) |>
    mutate(domain_name = str_replace(path, ".*/process/[^/]*/[^/]*/[^/]*/[^/]*/([^/]*)/.*", "\\1")) |>
    mutate(model_data_hash = map(.x = path,
                           .f =  ~ digest(.x, algo = "sha256", file = TRUE))) |>
    unnest(model_data_hash) |>
    mutate(data_scale = ifelse(data_scale == "Sectors", "sector", data_scale)) |>
    mutate(data_scale = ifelse(data_scale == "Regions", "region", data_scale)) |>
    filter(data_type == method,
           data_scale == scale
           )
  if (!is.null(domain)) models <- models |> filter(domain_name %in% domain)
  models
}

get_db_summary_table <- function(method, scale) {
  con <- dbConnect(RSQLite::SQLite(), config_$db_path,  busy_timeout = 5000)
  tbls <- dbListTables(con)
  ## if (scale == "reef") {
  ##   db_sum_tbl <- paste0(method, "_sum")
  ## } else {
    db_sum_tbl <- paste0(method, "_", scale, "_sum")
  ## }
  ## print(config_$db_path)
  ## print(db_sum_tbl)
  ## print(tbls)
  if (db_sum_tbl %in% tbls) {
    tb <- tbl(con, db_sum_tbl) |>
      collect()
  } else {
    tb <- NULL
  }
  dbDisconnect(con)
  tb
}

## Make/update summary db tables (including extract data hash)
update_db_summary_tables <- function(method, scale, domain = NULL) {
  message("Entering update_db_summary_tables")
  ## data_hashes <- make_data_hash(method = method, scale = scale, domain = domain) |>
  data_hashes <- make_data_hash(method = method, scale = scale, domain = NULL) |>
    dplyr::rename("extract_data_hash" = "model_data_hash",
                  "extract_data_path" = "path")
  message("Made data hashes")
  tb <- get_db_summary_table(method = method, scale = scale)
  if (!is.null(tb)) {
    if (nrow(tb) > 0) {
      tb <- tb |>
        dplyr::select(-any_of(c("extract_data_hash", "extract_data_path",
                                "data_type", "data_scale"))) |>
        left_join(data_hashes, by = c("domain_name" = "domain_name"))
    } else {
    tb <- data_hashes
    }
  } else {
    tb <- data_hashes
  }
  message("Here")

  ## if (scale == "reef") {
  ##   db_sum_tbl <- paste0(method, "_sum")
  ## } else {
    db_sum_tbl <- paste0(method, "_", scale, "_sum")
  ## }
  ## con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  ## tbls <- dbListTables(con)
  ## if (scale == "reef") {
  ##   db_sum_tbl <- paste0(method, "_sum")
  ## } else {
  ##   db_sum_tbl <- paste0(method, "_", scale, "_sum")
  ## }
  ## if (db_sum_tbl %in% tbls) {
  ##   tb <- tbl(con, db_sum_tbl) |>
  ##     collect() |>
  ##     dplyr::select(-any_of(c("extract_data_hash", "extract_data_path",
  ##                             "data_type", "data_scale"))) |>
  ##     left_join(data_hashes, by = c("domain_name" = "domain_name"))
  ## } else {
  ##   tb <- data_hashes
  ## }
  message(paste0("db_path: ", config_$db_path))
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  message(paste0("db_sum_tbl: ", db_sum_tbl))
  copy_to(con, tb, db_sum_tbl, temporary = FALSE, overwrite = TRUE)
  ## compute(db_sum_tbl, temporary = FALSE, overwrite = TRUE, copy = TRUE)
  message("done with update_db_summary_tables")
  dbDisconnect(con)
}

does_db_table_exist <- function(tbl_name) {
  message(paste0("\t does_db_table_exist: Determine if db table exists"), "\n")
  if (!file.exists(config_$db_path)) return(FALSE)
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  ## DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  tbls <- dbListTables(con)
  dbDisconnect(con)
  return (tbl_name %in% tbls)
}

scan_models_folder <- function(method = NULL, scale = NULL, domain = NULL) {

  cat("\t- In scan models folder (", get_time(), ") \n",
    file = config_$dashboard_log, append = TRUE)
  message(paste0("Scanning model folder"), "\n")
  if (!is.null(scale)) {
    data_scale <- case_when(scale == "reef" ~ "reef",
      scale == "sector" ~ "Sectors",
      scale == "nrm" ~ "nrm",
      scale == "region" ~ "region",
      )
  } else data_scale <- NULL
  fls <- list.files(config_$model_path,
    recursive = TRUE, full.names = TRUE)
  message(paste0("\t- length(fls): ", length(fls)), "\n")
  ## cat("\t- domain (", get_time(), ") ", domain, "\n",
  ##   file = config_$dashboard_log, append = TRUE)
  domain_str <- str_replace(domain, "\\(", "\\\\(") |>
    str_replace("\\)", "\\\\)")
  ## cat("\t- domain_str (", get_time(), ") ", domain_str, "\n",
  ##   file = config_$dashboard_log, append = TRUE)
  message(paste0("\t- domain_str: ", domain_str), "\n")
  ## message(paste0("\t- basename(fls): ", basename(fls)), "\n")

  regex_s <- paste0(
      ifelse(is.null(method), "[^_]*_", paste0(method, "_")),
      ifelse(is.null(data_scale), "[^_]*_", paste0(data_scale, "_")),
      ifelse(is.null(domain_str), "[^_]*", paste0("(",paste0(domain_str, collapse = "|"), ")")),
      "\\.rds$")
  cat("\t- regex_s (", get_time(), ") ", regex_s, "\n",
    file = config_$dashboard_log, append = TRUE)
  ## cat("\t- basename(fls) (", get_time(), ") ", basename(fls), "\n",
  ##   file = config_$dashboard_log, append = TRUE)
  fls_wch <- str_detect(basename(fls),
    paste0(
      ifelse(is.null(method), "[^_]*_", paste0(method, "_")),
      ifelse(is.null(data_scale), "[^_]*_", paste0(data_scale, "_")),
      ifelse(is.null(domain_str), "[^_]*", paste0("(",paste0(domain_str, collapse = "|"), ")")),
      "\\.rds$"))
  cat("\t- fls[fls_wch] (", get_time(), ") ", fls[fls_wch], "\n",
    file = config_$dashboard_log, append = TRUE)
  ## message(paste0("\t- fls_wch: ", fls_wch), "\n")
  ## message(paste0("\t- fls[fls_wch]: ", fls[fls_wch]), "\n")
  ## message(paste0("\t- fls[fls_wch] == \"\": ", fls[fls_wch] == ""), "\n")
  ## message(paste0("\t- length(fls[fls_wch]): ", length(fls[fls_wch])), "\n")
  if (length(fls[fls_wch]) == 0) {
    models <- data.frame(
      data_type = method,
      data_scale = scale,
      domain_name = domain_str,
      group = "", family_type = "",
      reef_zone = "", depth = "", shelf = "", model_type = "",
      sub_model = "",
      model_date = as.POSIXct(now()),
      reef_model_data_hash = "",
      sector_model_data_hash = "",
      nrm_model_data_hash = "",
      region_model_data_hash = "",
      selected_flag = "",
      path = "", model_path = "")
    return(models)
  }
  ## write.csv(fls_wch, file = "../data/temp2.csv")
  models <- tibble(path = unique(fls[fls_wch])) |>
    mutate(mod = map(.x = path,
      .f = ~ {
        ss <- readRDS(.x)$label |>
                        unlist() |>
                        unique()
        selected_flag <- readRDS(.x)$selected
        ## hash <- digest(.x, algo = "sha256", file = TRUE)
        mtime <- file.mtime(.x)
        tibble(str = ss) |>
          ## separate(str, into = c("data_type", "data_scale",
          ##   "domain_name", "group",
          ##   "family_type",
          ##   "reef_zone", "depth",
          ##   "shelf",
          ##   "model_type",
          ##   "sub_model"),
          ##   sep = "_") |>
          ## needed to replace the above since data_type can have an underscore...
          extract(str,
            into = c("data_type", "data_scale",
            "domain_name", "group",
            "family_type",
            "reef_zone", "depth",
            "shelf",
            "model_type",
            "sub_model"),
            regex = "^(.*)_([^_]+)_([^_]+)_([^_]+)_([^_]+)_([^_]+)_([^_]+)_([^_]+)_([^_]+)_([^_]+)$"
            ) |>
          mutate(model_date = as.POSIXct(format(mtime, "%Y-%m-%d %H:%M:%S")),#mtime, #format(mtime, "%Y-%m-%d %H:%M:%S"),
            model_path = .x,
            reef_model_data_hash = "",
            sector_model_data_hash = "",
            nrm_model_data_hash = "",
            region_model_data_hash = "",
            selected_flag = selected_flag) |>
          as.data.frame()
      })) |>
    unnest(c(mod)) |>
    ## dplyr::rename(model_path = path) |>
    mutate(data_scale = ifelse(data_scale == "Sectors", "sector", data_scale)
      )
  ## cat("\t- colnames models (", get_time(), ") ", colnames(models), "\n",
  ##   file = config_$dashboard_log, append = TRUE)
  ## th <- models |>
  ##   filter(data_type == "annual_report")
  ## cat("\t- MODELSTH (", get_time(), ") ", capture.output(print(th)), "\n", sep = "\n",
  ##   file = config_$dashboard_log, append = TRUE)
  models
}


update_db_model_hash <- function(method, scale, domain = NULL) {
  cat("\t- In update_db_model_hash (", get_time(), ") \n",
    file = config_$dashboard_log, append = TRUE)
  ## message(paste0("\n\nInside update_db_model_hash\n=========\n"))
  ## if (file.exists(paste0(config_$data_path, "db.lock"))) {
  ##   message(paste0("Database is locked, come back later"), "\n")
  ##   return(1)
  ## }
  if (domain == "all") domain <- NULL  ## dont think this is required any more
  file.create(paste0(config_$data_path, "db.lock"))
  ## message(paste0("method: ", method), "\n")
  ## message(paste0("scale: ", scale), "\n")
  ## message(paste0("domain: ", domain), "\n")
  ## get model_data_hash for the current domain(s)
  cat("\t- scale (", get_time(), ")", scale," \n",
    file = config_$dashboard_log, append = TRUE)

  ## message(paste0("Make a data hash"), "\n")
  data_hash <- make_data_hash(method = method, scale = scale, domain = domain) |>
    dplyr::rename(!!sym(paste0(scale, "_model_data_hash")) := "model_data_hash")
  ## cat("\t- colnames(data_hash) (", get_time(), ")", colnames(data_hash)," \n",
  ##   file = config_$dashboard_log, append = TRUE)
  ## th <- data_hash |> collect() |> as.data.frame() |> head()
  ## cat("\t- the head(data_hash) (", get_time(), ")", capture.output(print(th))," \n", sep = "\n",
  ##   file = config_$dashboard_log, append = TRUE)

  ## message(paste0("data_hash: ", capture.output(print(data_hash))), "\n")
  ## message(paste0("length data_hash: ", length(data_hash)), "\n")
  ## message(paste0("nrow data_hash: ", nrow(data_hash)), "\n")
  ## message(paste0("class data_hash: ", class(data_hash)), "\n")
  ## Incase a bogus *.flag file is encountered...
  if (nrow(data_hash) == 0) return(1)

  ## if models database table does not exist, make it
  if (!does_db_table_exist("models")) {
    message(paste0("Update models in db models table"), "\n")
    models_db <- update_db_models()
  }

  ## message(paste0("Get db model data"), "\n")
  ## get all but the focal model(s)
  models_db <- get_db_model_data() |>
    filter(!(data_type == method &
               data_scale == scale &
               domain_name %in% domain))
  ## message(paste(capture.output(models_db), collapse = "\n"))
    ## temp <- scan_models_folder(method = method,
    ##   scale = scale,
    ##   domain = domain) |>
    ##   select(model_path, everything())
    ## message(paste(capture.output(temp), collapse = "\n"))
    ## message("\n")
    ## message(paste(capture.output(temp$path), collapse = "\n"))
    ## message("\n")
    ## message(paste(capture.output(temp$model_path), collapse = "\n"))

  message(paste0("Get db model data (again)"), "\n")
  ## message(paste0("Columns:", colnames(get_db_model_data())), "\n")
  cat("\t- get_db_model_data nrows (", get_time(), ")", nrow(get_db_model_data())," \n",
    file = config_$dashboard_log, append = TRUE)
  ## th <- get_db_model_data() |>
  ##   filter(domain_name == "Northern GBR") |>
  ##   collect() |>
  ##   as.data.frame() |> head()
  ## cat("\t- get the db_model data (", get_time(), ")", capture.output(print(th))," \n", sep = "\n",
  ##   file = config_$dashboard_log, append = TRUE)
  ## cat("\t- data_type (", get_time(), ")", method, " \n",
  ##   file = config_$dashboard_log, append = TRUE)
  ## cat("\t- data_scale (", get_time(), ")", scale, " \n",
  ##   file = config_$dashboard_log, append = TRUE)

  ## temp <- get_db_model_data() |>
  ##   filter(!(data_type == method &
  ##              data_scale == scale &
  ##              domain_name %in% domain)) ## |>
  ## cat("\t- temp (", get_time(), ")", colnames(temp)," \n",
  ##   file = config_$dashboard_log, append = TRUE)
  ##   ## rbind(scan_models_folder(method = method,
  ##   ##   scale = scale,
  ##   ##   domain = domain) |>
  ##   ##     select(model_path, everything()) |> select(-path)
  ##   ## )
  ## ## cat("\t- temp (", get_time(), ")", colnames(temp)," \n",
  ## ##   file = config_$dashboard_log, append = TRUE)
  ## temp1 <- scan_models_folder(method = method,
  ##   scale = scale,
  ##   domain = domain) |>
  ##   select(model_path, everything()) |> select(-path)
  ## th <- temp1 |> as.data.frame()
  ## cat("\t- scan_models_folder data (", get_time(), ")", capture.output(print(th))," \n", sep = "\n",
  ##   file = config_$dashboard_log, append = TRUE)
  ## ## temp2 <- temp |> rbind(temp1)
  ## ## cat("\t- temp1 (", get_time(), ")", nrow(temp2)," \n",
  ## ##   file = config_$dashboard_log, append = TRUE)

  models_db <- get_db_model_data() |>
    ## remove the focal domains
    filter(!(data_type == method &
               data_scale == scale &
               ## str_detect(domain_name, domain_str))) |>
               domain_name %in% domain)) |>
    mutate(model_date = as.POSIXct(model_date)) |>
    rbind(scan_models_folder(method = method,
      scale = scale,
      domain = domain) |>
        select(model_path, everything()) |> select(-path)
    )
  ## cat("\t- nrows of models_db (", get_time(), ")", nrow(models_db)," \n",
  ##   file = config_$dashboard_log, append = TRUE)
  ## cat("\t- colnames(models_db) (", get_time(), ")", colnames(models_db)," \n",
  ##   file = config_$dashboard_log, append = TRUE)
  ## cat("\t- MODELS_DB (", get_time(), ")", capture.output(print(models_db))," \n", sep = "\n",
  ##   file = config_$dashboard_log, append = TRUE)
  ## th <- models_db |> filter(data_type == method)
  ## cat("\t- MODELS_DB (filtered) (", get_time(), ")", capture.output(print(th))," \n", sep = "\n",
  ##   file = config_$dashboard_log, append = TRUE)
  ## cat("\t- METHOD (", get_time(), ")", capture.output(print(method))," \n", sep = "\n",
  ##   file = config_$dashboard_log, append = TRUE)
  ## ## get the summary table (to know what all models would be)

  ## message(paste0("Make db summary table data"), "\n")
  ## Get the method_data_scale_sum db table info for focal model(s)
  tb <- get_db_summary_table(method = method, scale = scale)
  ## cat("\t- nrows of tb (", get_time(), ")", nrow(tb)," \n",
  ##   file = config_$dashboard_log, append = TRUE)

  ## initially, the database table from which tb is extracted might be empty.
  ## when this is the case, the fields might be the wrong class.  So we will
  ## check to make sure they are the correct class before joining.
  tb <- tb |>
    mutate(data_scale =  as.character(data_scale))

  ## alert(head(models_db))
  ## alert(models_db |> filter(data_type == "juveniles"))
  ## alert(class(models_db$data_scale))
  ## alert(class(tb$data_scale))
  ## alert(head(tb))
  ## Join tb to models_db to ensure that all possible models have a row
  ## cat("\t- column names of tb (", get_time(), ")", colnames(tb)," \n",
  ##   file = config_$dashboard_log, append = TRUE)
  models_db <- models_db |>
    full_join(tb |>
              select(data_type, data_scale, domain_name),
              by = c("data_type", "data_scale", "domain_name"))
  ## join in data_hash, to update the models database
  ## for the focal domain, there will be a two x_model_data_hash values
  ## (the old: .x,  and the new: .y) - always use the .y
  cat("\t- colnames(models_db) (", get_time(), ")", colnames(models_db)," \n",
    file = config_$dashboard_log, append = TRUE)
  cat("\t- colnames(data_hash) (", get_time(), ")", colnames(data_hash)," \n",
    file = config_$dashboard_log, append = TRUE)
  cat("\t- nrow(models_db) (", get_time(), ")", nrow(models_db)," \n",
    file = config_$dashboard_log, append = TRUE)
  cat("\t- nrow(data_hash) (", get_time(), ")", nrow(data_hash)," \n",
    file = config_$dashboard_log, append = TRUE)

  ## temp <- models_db |>
  ##   full_join(data_hash |> dplyr::select(-path),
  ##             by = c("data_type", "data_scale", "domain_name"))
  ## cat("\t- colnames(temp) (", get_time(), ")", colnames(temp)," \n",
  ##   file = config_$dashboard_log, append = TRUE)
  models_db <-
    models_db |>
    full_join(data_hash |> dplyr::select(-path),
              by = c("data_type", "data_scale", "domain_name")) |>
    (\(df) df |>
           mutate(across(ends_with(".x"), ~ coalesce(df[[sub(".x", ".y", cur_column())]], .),
                         .names = "{col}")))() |>
                                           select(-ends_with(".y")) |>
                                           rename_with(~ sub("\\.x", "", .), ends_with(".x"))

  ## pp <- models_db |> filter(data_type == "manta", data_scale == "reef", domain_name == "12068S") |> as.data.frame() |> head()
  ## print(pp)

  message(paste0("Copy data to database"), "\n")
  con <- dbConnect(RSQLite::SQLite(), dbname = config_$db_path)
  copy_to(con, models_db, name = "models", temporary = FALSE, overwrite = TRUE)
  dbDisconnect(con)

  file.remove(paste0(config_$data_path, "db.lock"))
  cat("\t- here (", get_time(), ")\n",
    file = config_$dashboard_log, append = TRUE)
  return(0)
}

which_models_to_update <- function(method, scale, all = FALSE) {
  ## data_scales <- case_when(scale == "reef" ~ "reef",
  ##                         scale == "sectors" ~ "sector",
  ##                         scale == "nrm" ~ "nrm"
  ##                         )
  ## get the full list of domain names from the summmary table
  ## print(method)
  ## print(scale)
  all_domains <- get_db_summary_table(method = method, scale = scale) |>
    pull(domain_name) |>
    unique()
  ## print(all_domains)
  ## get the set of domains that have previously been modelled
  data_hash <- make_data_hash(method = method, scale = scale, domain = NULL) |>
    dplyr::rename(!!sym(paste0(scale, "_model_data_hash")) := "model_data_hash")
  ## print(data_hash)
  if (all | !does_db_table_exist("models")) {
    domains <- data_hash |> pull(domain_name) |> c(all_domains) |> unique()
  } else {
    ## get the current model db table
    models_db <- get_db_model_data() |>
      dplyr::filter(data_type == method, data_scale == scale)
    ## print(models_db)
    ## if there is no information on any domains in models
    if(nrow(models_db) == 0) {
      domains <- data_hash |>
        pull(domain_name) |>
        c(all_domains) |>
        unique()
    } else {
      ## get the domains that were modelled with the current data (dont require remodelling)
      domains <-
        models_db |>
        full_join(data_hash |> dplyr::select(-path),
                  by = c("data_type", "data_scale", "domain_name")) |>
        mutate(across(ends_with(".x"), ~ . == get(sub(".x", ".y", cur_column())),
                      .names = "flag")) |>
        mutate(flag = ifelse(is.na(flag), FALSE, flag)) |>
        filter(flag & !is.na(model_date)) |>
        pull(domain_name) |>
        unique()
      if (length(domains) == 0) {
        domains <- all_domains
      } else {
        domains <- all_domains[!all_domains %in% domains]
      }
    }
  }
  domains
}

## Functions =========================================================

## Check if the "models" db table exists
## If it does, extract the entries
## If no, get those info from the data structures
## Filter the entries to relevant candidates
get_candidates <- function(tab_name, data_types, scale, domain = NULL) {
  ## alert("here")
  if(!does_db_table_exist("models")) {
    models <- get_config_models() |>
        filter(data_type %in% data_types,
               data_scale == scale,
               !is.na(path))
  } else {
    models <- get_db_model_data(method = NULL, scale = scale, domain = domain)
    ## narrow to only those with a path, and of the correct data_type/method
    if (!is.null(models))
      models <- models |>
        filter(data_type %in% data_types,
               data_scale == scale,
               !is.na(path))
    ## alert(scale)
    ##   alert(unique(models$data_scale))
    ## write_csv(models, file = paste0("~/data/", "candidate_models_1.csv")) 

    ## write_csv(models, file = paste0(config_$data_path, "candidate_models_2.csv")) 
  }
  if (is.null(models))
    models <- blank_candidate_models() |>
        filter(data_type %in% data_types,
               data_scale == scale,
               !is.na(path))
  
  ## config_$model <- models
  ## assign("config_", config_, envir = .GlobalEnv)
  cat(file = stderr(), paste("refresh pressed:", models), "\n")
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
             data_scale = "",
             domain_name = "",
             group = "",
             family_type = "",
             reef_zone = "",
             depth = "",
             shelf = ""
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
  if (data_type == "photo-transect") {
    out2 <- system(paste0(
      "cut -d, -f17,6,28,9 < ", csv_file, " | uniq |
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
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  ## dbWriteTable(con, paste0(data_type, "_sum1"), out2, overwrite = TRUE)
  data_info <- file.mtime(csv_file)
  data <- as.data.frame(out2) |> 
    slice(-1)  |> 
    separate(everything(),
             into = c("nrm", "sector", "reef", "survey_date"), sep = ",")  |>
    mutate(## reef = str_replace_all(reef, '"', ""),
           across(everything(), ~ str_replace_all(.x, '"', "")),
           survey_date = as.POSIXct(survey_date, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"),
           ## extraction_date = format(data_info, "%Y-%m-%d %H:%M:%S"))
           extraction_date = data_info)
  ## print(head(data))
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


make_dashboard_summary <- function(method, scale) {
  data <- tibble("Blank")
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  tbls <- dbListTables(con) 
  if (scale %in% c("reef", "sql") &
      paste0(method, "_sum") %in% tbls) {
    if (scale == "sql") {
      db_tbl <- paste0(method, "_sum")
      data <- tbl(con, db_tbl) 
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
      scale == "nrm" ~ c("reef", "sector")
    ) 
    db_tbl <- paste0(method, "_sum")
    data <- tbl(con, db_tbl) |> 
      dplyr::select(-all_of(scales_to_remove)) |> 
      dplyr::group_by(!!sym(scale)) |>
      summarise(survey_date = max(survey_date),
                extraction_date = min(extraction_date)) |> 
      dplyr::ungroup() |> 
      left_join(
        tbl(con, paste0(method, "_", scale, "_sum")) |>
        dplyr::select(domain_name, extract_data_hash) |>
        distinct(),
        by = setNames("domain_name", scale)) |> 
      dplyr::rename(setNames("extract_data_hash", paste0(scale, "_extract_data_hash"))) 
    if ("models" %in% tbls) {
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
  }
  dbDisconnect(con)
  return(data) 
}

## Retrieve data from database models table
get_db_model_data<- function(method = NULL, scale = NULL, domain = NULL) {
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
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
  data_paths <- paste0(config_$data_path, method)
  data_scale <- case_when(scale == "reef" ~ "reef",
                          scale == "sector" ~ "Sectors",
                          scale == "nrm" ~ "nrm"
                          )
  fls <- list.files(data_paths,
                    recursive = TRUE, full.names = TRUE)
  fls_wch <- str_detect(fls, paste0(".*/process/[^/]*/[^/]*/[^/]*/",data_scale,"/.*.csv$")) 
  models <- tibble(path = unique(fls[fls_wch])) |>
    mutate(data_type = str_replace(path, ".*/data/([^/]*)/.*", "\\1")) |>
    mutate(data_scale = str_replace(path, ".*/process/[^/]*/[^/]*/[^/]*/([^/]*)/.*", "\\1")) |>
    mutate(domain_name = str_replace(path, ".*/process/[^/]*/[^/]*/[^/]*/[^/]*/([^/]*)/.*", "\\1")) |>
    mutate(model_data_hash = map(.x = path,
                           .f =  ~ digest(.x, algo = "sha256", file = TRUE))) |>
    unnest(model_data_hash) |> 
    mutate(data_scale = ifelse(data_scale == "Sectors", "sector", data_scale)) |> 
    filter(data_type == method,
           data_scale == scale
           ) 
  if (!is.null(domain)) models <- models |> filter(domain_name %in% domain)
  models
}

get_db_summary_table <- function(method, scale) {
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
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
  tb
}

## Make/update summary db tables (including extract data hash)
update_db_summary_tables <- function(method, scale, domain = NULL) {
  ## data_hashes <- make_data_hash(method = method, scale = scale, domain = domain) |>
  data_hashes <- make_data_hash(method = method, scale = scale, domain = NULL) |>
    dplyr::rename("extract_data_hash" = "model_data_hash",
                  "extract_data_path" = "path")
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
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  copy_to(con, tb, db_sum_tbl, temporary = FALSE, overwrite = TRUE)
  ## compute(db_sum_tbl, temporary = FALSE, overwrite = TRUE, copy = TRUE)
  dbDisconnect(con)
}

does_db_table_exist <- function(tbl_name) {
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  tbls <- dbListTables(con) 
  dbDisconnect(con)
  return (tbl_name %in% tbls)
}

scan_models_folder <- function(method = NULL, scale = NULL, domain = NULL) {
  if (!is.null(scale)) {
    data_scale <- case_when(scale == "reef" ~ "reef",
                            scale == "sector" ~ "Sectors",
                            scale == "nrm" ~ "nrm",
                            )
  } else data_scale <- NULL
  fls <- list.files(config_$model_path,
                    recursive = TRUE, full.names = TRUE)
  domain_str <- str_replace(domain, "\\(", "\\\\(") |>
    str_replace("\\)", "\\\\)")
  fls_wch <- str_detect(basename(fls),
                        paste0(
                               ifelse(is.null(method), "[^_]*_", paste0(method, "_")),
                               ifelse(is.null(data_scale), "[^_]*_", paste0(data_scale, "_")),
                               ifelse(is.null(domain_str), "[^_]*", paste0("(",paste0(domain_str, collapse = "|"), ")")),
                               "\\.rds$"))
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
                         separate(str, into = c("data_type", "data_scale",
                                                "domain_name", "group",
                                                "family_type",
                                                "reef_zone", "depth",
                                                "shelf",
                                                "model_type",
                                                "sub_model"),
                                  sep = "_") |>
                         mutate(model_date = as.POSIXct(format(mtime, "%Y-%m-%d %H:%M:%S")),#mtime, #format(mtime, "%Y-%m-%d %H:%M:%S"),
                                model_path = .x,
                                reef_model_data_hash = "",
                                sector_model_data_hash = "",
                                nrm_model_data_hash = "",
                                selected_flag = selected_flag) |> 
                         as.data.frame()
                     })) |>
    unnest(c(mod)) |>
    ## dplyr::rename(model_path = path) |> 
    mutate(data_scale = ifelse(data_scale == "Sectors", "sector", data_scale))
  models
}


update_db_model_hash <- function(method, scale, domain = NULL) {
  ## get model_data_hash for the current domain(s)
  data_hash <- make_data_hash(method = method, scale = scale, domain = domain) |>
    dplyr::rename(!!sym(paste0(scale, "_model_data_hash")) := "model_data_hash")
  ## pp <- data_hash |> filter(data_type == "manta", data_scale == "reef", domain_name == "12068S") |> as.data.frame() |> head()
  ## print(pp)
  ## if models database table does not exist, make it
  if (!does_db_table_exist("models")) {
    models_db <- update_db_models()
  }
  ## domain_str <- str_replace(domain, "\\(", "\\\\(") |>
  ##   str_replace("\\)", "\\\\)")
  ## get the current model db table
  ## alert(scan_models_folder(method = method,
  ##                            scale = scale,
  ##                            domain = domain))
  ## alert(colnames(scan_models_folder(method = method,
  ##                            scale = scale,
  ##                            domain = domain)))
  ## alert(colnames(get_db_model_data()))
  models_db <- get_db_model_data() |> 
    ## remove the focal domains
    filter(!(data_type == method &
           data_scale == scale &
           ## str_detect(domain_name, domain_str))) |> 
           domain_name %in% domain)) |> 
    mutate(model_date = as.POSIXct(model_date)) |> 
    rbind(scan_models_folder(method = method,
                             scale = scale,
                             domain = domain))
  ## )
  ## }
  ## get the summary table (to know what all models would be)
  tb <- get_db_summary_table(method = method, scale = scale)
  ## Join tb to models_db to ensure that all possible models have a row
  models_db <- models_db |>
    full_join(tb |> select(data_type, data_scale, domain_name),
              by = c("data_type", "data_scale", "domain_name"))
  ## join in data_hash, to update the models database
  ## for the focal domain, there will be a two x_model_data_hash values
  ## (the old: .x,  and the new: .y) - always use the .y 
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
  con <- dbConnect(RSQLite::SQLite(), dbname = config_$db_path)
  copy_to(con, models_db, name = "models", temporary = FALSE, overwrite = TRUE)
  dbDisconnect(con)
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

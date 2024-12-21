## Functions =========================================================

## Extract the "models" table data

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
                       ## hash <- digest(.x, algo = "sha256", file = TRUE)
                       mtime <- file.mtime(.x)
                       tibble(str = ss) |>
                         separate(str, into = c("data_type", "data_scale",
                                                "domain_name", "group",
                                                "reef_zone", "depth",
                                                "shelf"),
                                  sep = "_") |>
                         mutate(model_date = format(mtime, "%Y-%m-%d %H:%M:%S"),
                                ## model_path = .x,
                                reef_model_data_hash = "",
                                sector_model_data_hash = "",
                                nrm_model_data_hash = "") |> 
                         as.data.frame()
                     })) |>
    unnest(c(mod)) |>
    dplyr::rename(model_path = path) |> 
    mutate(data_scale = ifelse(data_scale == "Sectors", "sector", data_scale))

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
                       "reef_zone" = "reef_zone",
                       "depth" = "depth",
                       "shelf")) 
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
  if (scale == "reef") {
    db_sum_tbl <- paste0(method, "_sum")
  } else {
    db_sum_tbl <- paste0(method, "_", scale, "_sum")
  }
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
  if (scale == "reef") {
    db_sum_tbl <- paste0(method, "_sum")
  } else {
    db_sum_tbl <- paste0(method, "_", scale, "_sum")
  }
  tb <- get_db_summary_table(method = method, scale = scale)
  if (!is.null(tb)) {
   tb <- tb |> 
      dplyr::select(-any_of(c("extract_data_hash", "extract_data_path",
                              "data_type", "data_scale"))) |> 
      left_join(data_hashes, by = c("domain_name" = "domain_name")) 
  } else {
    tb <- data_hashes
  }
  
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
  fls_wch <- str_detect(basename(fls),
                        paste0(
                               ifelse(is.null(method), "[^_]*_", paste0(method, "_")),
                               ifelse(is.null(data_scale), "[^_]*_", paste0(data_scale, "_")),
                               ifelse(is.null(domain), "[^_]*", paste0("(",paste0(domain, collapse = "|"), ")")),
                               "\\.rds$"))
  ## write.csv(fls_wch, file = "../data/temp2.csv")
  models <- tibble(path = unique(fls[fls_wch])) |> 
    mutate(mod = map(.x = path,
                     .f = ~ {
                       ss <- readRDS(.x)$label |>
                                       unlist() |>
                                       unique()
                       ## hash <- digest(.x, algo = "sha256", file = TRUE)
                       mtime <- file.mtime(.x)
                       tibble(str = ss) |>
                         separate(str, into = c("data_type", "data_scale",
                                                "domain_name", "group",
                                                "reef_zone", "depth",
                                                "shelf"),
                                  sep = "_") |>
                         mutate(model_date = format(mtime, "%Y-%m-%d %H:%M:%S"),
                                ## model_path = .x,
                                reef_model_data_hash = "",
                                sector_model_data_hash = "",
                                nrm_model_data_hash = "") |> 
                         as.data.frame()
                     })) |>
    unnest(c(mod)) |>
    dplyr::rename(model_path = path) |> 
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
  ## get the current model db table
  models_db <- get_db_model_data()
  ## if the models database does not have these domain(s) 
  ## if (nrow(models_db |> filter(data_type == method, data_scale == scale, domain_name %in% domain)) == 0) {
  models_db <-
    ## models_db |>
    ## bind_rows(
    scan_models_folder(method = method, scale = scale, domain = domain)
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
  all_domains <- get_db_summary_table(method = method, scale = scale) |>
    pull(domain_name) |>
    unique()
  ## get the set of domains that have previously been modelled
  data_hash <- make_data_hash(method = method, scale = scale, domain = NULL) |>
    dplyr::rename(!!sym(paste0(scale, "_model_data_hash")) := "model_data_hash")
  if (all | !does_db_table_exist("models")) {
    domains <- data_hash |> pull(domain_name) |> c(all_domains) |> unique()
  } else {
    ## get the current model db table
    models_db <- get_db_model_data() |>
      dplyr::filter(data_type == method, data_scale == scale)
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
        filter(flag) |>
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

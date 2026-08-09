#########################################################################
## The following function parses the command line arguments.           ##
## Note.  This is just about the only function that does not trigger a ##
## tryCatch log file - and this is primarily because before running    ##
## this function, the location of writable directories is unknown      ##
## since they are parsed as command line arguments.                    ##
#########################################################################
zoning_parse_args <- function(args) {
  cat("\nParsed command line arguments\n==================================\n")
  ## external directories (parsed from command line)
  has_data_path_argument <- any(grepl("--data_path=.*", args, perl = TRUE))
  if(has_data_path_argument) {
    arg <- args[grep("--data_path=.*", args)]
    data_path <- gsub("--data_path=(.)", "\\1", arg)
    cat(paste0("\t- data path: ", data_path), "\n",
      append = TRUE)
    assign("data_path", data_path, envir = .GlobalEnv)
  } else stop("no --data_path= component provided")

  spatial_path <- paste0(data_path, "spatial/")
  if(!dir.exists(spatial_path)) dir.create(spatial_path)
  assign("spatial_path", spatial_path, envir = .GlobalEnv)

  has_domain_argument <- any(grepl("--domain=.*", args, perl = TRUE))
  if(has_domain_argument) {
    arg <- args[grep("--domain=.*", args)]
    domain <- gsub("--domain=(.)", "\\1", arg)
    cat(paste0("\t- domain: ", domain), "\n",
      append = TRUE)
    assign("domain", domain, envir = .GlobalEnv)
  } else stop("no --domain= component provided")

  log_path <- paste0(data_path, "log/")
  if(!dir.exists(log_path)) dir.create(log_path)
  log_file <- paste0(log_path, "annual_report_", domain, ".log")
  if (file.exists(log_file)) file.remove(log_file)
  assign("log_file", log_file, envir = .GlobalEnv)
    cat(paste0("\t- temporary log file: ", log_file), "\n",
      append = TRUE)

  has_output_path_argument <- any(grepl("--output_path=.*", args, perl = TRUE))
  if(has_output_path_argument) {
    arg <- args[grep("--output_path=.*", args)]
    output_path <- gsub("--output_path=(.)", "\\1", arg)
    cat(paste0("\t- output path: ", output_path), "\n",
      append = TRUE)
    assign("output_path", output_path, envir = .GlobalEnv)

    fig_path <- paste0(output_path, "figures/")
    cat(paste0("\t- figures path: ", fig_path), "\n",
      append = TRUE)
    assign("fig_path", fig_path, envir = .GlobalEnv)

    tbl_path <- paste0(output_path, "tables/")
    cat(paste0("\t- tables path: ", tbl_path), "\n",
      append = TRUE)
    assign("tbl_path", tbl_path, envir = .GlobalEnv)
  } else stop("no --output_path= component provided")

  has_path_argument <- any(grepl("--path=.*", args, perl = TRUE))
  if(has_path_argument) {
    arg <- args[grep("--path=.*", args)]
    path <- gsub("--path=(.)", "\\1", arg)
    path <- gsub("'", "", path)
    cat(paste0("\t- path: ", path), "\n",
      append = TRUE)
    assign("path", path, envir = .GlobalEnv)
  } else stop("no --path= component provided")

  has_refit_argument <- any(grepl("--refit=.*", args, perl = TRUE))
  if(has_refit_argument) {
    arg <- args[grep("--refit=.*", args)]
    refit <- gsub("--refit=(.)", "\\1", arg)
    refit <- ifelse(refit == "TRUE", TRUE, FALSE)
    cat(paste0("\t- refit: ", refit, " class:", class(refit)), "\n",
      append = TRUE)
    assign("refit", refit, envir = .GlobalEnv)
  } else stop("no --refit= component provided")

  has_final_year_argument <- any(grepl("--final_year=.*", args, perl = TRUE))
  if(has_final_year_argument) {
    arg <- args[grep("--final_year=.*", args)]
    final_year <- as.numeric(as.character(gsub("--final_year=(.)", "\\1", arg)))
    cat(paste0("\t- final_year: ", final_year), "\n",
      append = TRUE)
    assign("final_year", final_year, envir = .GlobalEnv)
  } #else stop("no --final_year= component provided")

  has_sql_path_argument <- any(grepl("--sql_path=.*", args, perl = TRUE))
  if(has_sql_path_argument) {
    arg <- args[grep("--sql_path=.*", args)]
    sql_path <- gsub("--sql_path=(.)", "\\1", arg)
    cat(paste0("\t- sql_path: ", sql_path), "\n",
      append = TRUE)
    assign("sql_path", sql_path, envir = .GlobalEnv)
  }
}

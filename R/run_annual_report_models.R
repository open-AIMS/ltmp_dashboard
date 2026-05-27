cat("\nInside run_annual_report_model.R\n=============================================\n")
message("\nInside run_annual_report_model.R\n=============================================\n")
message("load packages\n")

library(tidyverse)

args <- commandArgs()

message(args)
print(args)

has_method_argument <- any(grepl("--method=.*", args, perl = TRUE))
if(has_method_argument) {
  arg <- args[grep("--method=.*", args)]
  data_type <- gsub("--method=(.)", "\\1", arg)
}
has_scale_argument <- any(grepl("--scale=.*", args, perl = TRUE))
if(has_scale_argument) {
  arg <- args[grep("--scale=.*", args)]
  data_scale <- gsub("--scale=(.)", "\\1", arg)
}

has_log_argument <- any(grepl("--log=.*", args, perl = TRUE))
if(has_log_argument) {
  arg <- args[grep("--log=.*", args)]
  out_log_file <- gsub("--log=(.)", "\\1", arg)
  ## out_log_file <- gsub(".log$", "_model1.log", out_log_file)
  old_log_file <- gsub(".log$", ".old", out_log_file)
  if (!file.exists(old_log_file)) file.create(old_log_file)
  print(paste0("run_annual_report_models.R: out_log_file=", out_log_file))
  message(paste0("run_annual_report_models.R: out_log_file=", out_log_file))
  message(paste0("run_annual_report_models.R: old_log_file=", old_log_file))
}

has_domain_argument <- any(grepl("--domain=.*", args, perl = TRUE))
if(has_domain_argument) {
  arg <- args[grep("--domain=.*", args)]
  domain <- gsub("--domain=(.)", "\\1", arg)
}
print(paste0("run_models.R: domain=", domain))
    ## file.append(file1 = old_log_file, file2 = out_log_file)
print(paste0("run_models.R: out_log_file=", out_log_file))

has_data_argument <- any(grepl("--data_path=.*", args, perl = TRUE))
if(has_data_argument) {
  arg <- args[grep("--data_path=.*", args)]
  data_path <- gsub("--data_path=(.)", "\\1", arg)
} else {
  data_path <- "../data"
}

has_output_argument <- any(grepl("--output_path=.*", args, perl = TRUE))
if(has_output_argument) {
  arg <- args[grep("--output_path=.*", args)]
  output_path <- gsub("--output_path=(.)", "\\1", arg)
} else {
  output_path <- "../output"
}

has_run_refit_argument <- any(grepl("--model_run_refit=.*", args, perl = TRUE))
if(has_run_refit_argument) {
  arg <- args[grep("--model_run_refit=.*", args)]
  refit <- gsub("--model_run_refit=(.)", "\\1", arg)
} else {
  refit <- TRUE
}
## print(paste0("run_models.R: model_run_location=", model_run_location))

cat(paste0("\nStart fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))

for (dom in domain) {
  .method <- "annual_report"
  old_log_file <- gsub(".log$", ".old", out_log_file)
  .path <- shQuote(paste0("--path='/data/", .method,
        "/2021-01-14/process/ALL/2024/",
        dom, "/Regions/", dom,
        "/raw/reef_data.csv'"))
 .scale <- "region"
  file.create(paste0(data_path, data_type, "_", .scale, "_", dom, ".job"))

  ## Note, this is only run on the HPC

  out_other_file <- gsub(".log$", ".other", out_log_file)
  remote_cmd <- paste(
    "jid=$(",
    "sbatch",
    paste0("--output='log/model_", data_type, "_", dom, "_", data_scale, ".log'"),
    "/export/project/monitoring-dashboard/annual_report_analysis.slurm",
    .path,
    paste0("--method=", data_type),
    paste0("--domain='", dom, "'"),
    paste0("--scale=", .scale),
    paste0("--refit=", refit),
    "--status=true",
    "--refresh_data=false",
    "--data_path=/data/",
    "--output_path=/output/",
    ");",
    "echo \"Submitted job $jid\";"
  )

  cat(paste0("sbatch:", remote_cmd, "\n"), file = out_other_file, append = TRUE)  #log/dashboard.other
  cat(paste0("\n\n", paste("bash -l -c", shQuote(remote_cmd)), "\n"), file = out_other_file, append = TRUE)
  cat(paste(remote_cmd, "\n"))   # log/dashboard_model.log

  process <- processx::process$new(
    "ssh",
    args = c(
      "hpc",
      paste("bash -l -c", shQuote(remote_cmd))
    ),
    stdout = out_log_file
  )
  process$wait()
  cat(paste0("After the processx call:", "\n"), file = out_other_file, append = TRUE)
}

cat(paste0("\nEnd fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))

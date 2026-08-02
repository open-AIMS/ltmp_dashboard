cat(paste0("\nInside of run_models.R ","\n=============================================\n"))
cat("\nInside run_model.R\n=============================================\n")
message("\nInside run_model.R\n=============================================\n")
message("load packages\n")

library(tidyverse)
## library(dplyr)
## library(processx)

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
  print(paste0("run_models.R: out_log_file=", out_log_file))
  message(paste0("run_models.R: out_log_file=", out_log_file))
  message(paste0("run_models.R: old_log_file=", old_log_file))
}

print(paste0("run_models.R: data_type=", data_type))
print(paste0("run_models.R: data_scale=", data_scale))
print(paste0("run_models.R: user=", Sys.info()["user"]))

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

has_model_run_location_argument <- any(grepl("--model_run_location=.*", args, perl = TRUE))
if(has_model_run_location_argument) {
  arg <- args[grep("--model_run_location=.*", args)]
  model_run_location <- gsub("--model_run_location=(.)", "\\1", arg)
} else {
  model_run_location <- "local"
}
print(paste0("run_models.R: model_run_location=", model_run_location))

message(paste0("data_type: ", data_type))
message(paste0("data_scale: ", data_scale))
message(paste0("data_path: ", data_path))
message(paste0("output_path: ", output_path))
message(paste0("model_run_location: ", model_run_location))

## Sys.sleep(30)

cat(paste0("\nStart fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))


for (dom in domain) {
  old_log_file <- gsub(".log$", ".old", out_log_file)
  .method <- case_when(
    data_type == "photo-transect" ~ "photo-transect",
    data_type == "manta" ~ "manta",
    data_type == "juveniles" ~ "juvenile",
    data_type == "fish" ~ "fish"
  )
  .path <- case_when(
    data_scale == "reef" ~
      shQuote(paste0("--path='/data/", .method,
        "/2021-01-14/process/ALL/2024/ALL",
        "/reef/", dom,
        "/raw/reef_data.zip'")),
    data_scale == "sector" ~
      shQuote(paste0("--path='/data/", .method,
        "/2021-01-14/process/ALL/2024/",
        dom, "/Sectors/", dom,
        "/raw/reef_data.zip'")),
    data_scale == "nrm" ~
      shQuote(paste0("--path='/data/", .method,
        "/2021-01-14/process/ALL/2024/",
        dom, "/nrm/", dom,
        "/raw/reef_data.zip'"))
  )
  .scale <- case_when(
    data_scale == "sector" ~ "Sectors",
    data_scale == "nrm" ~ "nrm",
    data_scale == "reef" ~ "reef"
  )
  file.create(paste0(data_path, data_type, "_", .scale, "_", dom, ".job"))
  if (model_run_location == "local") {
    cat("\nRunning model locally\n", append = TRUE)
    out_log_file_model <- gsub(
      "(.*model)_([^_]*)_ _(.*).log",
      paste0("\\1_\\2_", dom, "_\\3.log"),
      out_log_file)
    tmp_out_log_file <- gsub(".log$", "tmp.log", out_log_file)
    unlink(tmp_out_log_file)
    file.copy(from = out_log_file, to = tmp_out_log_file)
    unlink(out_log_file)
    cat("\nAbout to run local docker\n",
      file = tmp_out_log_file, append = TRUE)
    cat("\nAbout to run local docker\n",
      append = TRUE)
    uid <- system("id -u", intern = TRUE)
    gid <- system("id -g", intern = TRUE)
    ## user_arg <- paste0(uid, ":", gid)
    user_arg <- paste0("root", ":", 664)
    cat("\n\nUser_arg: ", user_arg, "\n\n",
      file = tmp_out_log_file,
      append = TRUE)

    run_string <-
      paste('bash -c',
        '"umask 002 && Rscript /home/Project/R/00_main.R',
        gsub('"', '', .path),
        paste0('--method=', data_type),
        paste0("--domain='", dom, "'"),
        paste0("--scale=", .scale),
        "--status=true",
        "--refresh_data=false",
        "--data_path=/data/",
        "--output_path=/output/",
        '"')
    cat("\nRun string: ", run_string, file = tmp_out_log_file, append = TRUE)
    system2("docker",
      args = c(
        "run",
        "-i",
        "--rm",
        "-u", user_arg,
        "-v /etc/localtime:/etc/localtime",
        "-v /etc/timezone:/etc/timezone",
        "-v /export/project/monitoring-dashboard/dev:/home/Project",
        ## "-v /home/mlogan/data:/data",
        paste0("-v ", data_path, ":/data"),
        paste0("-v ", output_path, ":/output"),
        "-w /home/Project/R",
        ## "ltmp-monitoring-model:latest",
        "ghcr.io/open-aims/ltmp_dashboard:main",
        run_string
      ),
      stdout = out_log_file,
      wait = TRUE)
    ## system2("docker",
    ##   args = c(
    ##     "run",
    ##     "-i",
    ##     "--rm",
    ##     "-u", user_arg,
    ##     "-v /etc/localtime:/etc/localtime",
    ##     "-v /etc/timezone:/etc/timezone",
    ##     "-v /export/project/monitoring-dashboard/dev:/home/Project",
    ##     ## "-v /home/mlogan/data:/data",
    ##     paste0("-v ", data_path, ":/data"),
    ##     paste0("-v ", output_path, ":/output"),
    ##     "-w /home/Project/R",
    ##     ## "ltmp-monitoring-model:latest",
    ##     "ghcr.io/open-aims/ltmp_dashboard:main",
    ##     ## "Rscript",
    ##     ## "/home/Project/R/00_main.R",
    ##     ## .path,
    ##     ## shQuote(paste0("--method=", data_type)),
    ##     ## shQuote(paste0("--domain=", dom)),
    ##     ## shQuote(paste0("--scale=", .scale)),
    ##     ## ## shQuote(paste0("--scale=", "Sectors")),
    ##     ## "--status=true",
    ##     ## "--refresh_data=false",
    ##     ## "--data_path=/data/",
    ##     ## "--output_path=/output/"
    ##   ),
    ##   stdout = out_log_file,
    ##   wait = TRUE)
    file.create(paste0(data_path, data_type, "_", .scale, "_", dom, ".flag"))
    file.copy(from = out_log_file, to = out_log_file_model)
    file.copy(from = tmp_out_log_file, to = out_log_file)
    file.remove(tmp_out_log_file)
    file.remove(paste0(data_path, data_type, "_", .scale, "_", dom, ".job"))

    ## file.append(file1 = old_log_file, file2 = out_log_file)
  } else if (model_run_location == "hpc") {
    out_other_file <- gsub(".log$", ".other", out_log_file)
    remote_cmd <- paste(
      "jid=$(",
      "sbatch",
      paste0("--output='log/model_", data_type, "_", dom, "_", data_scale, ".log'"),
      "/export/project/monitoring-dashboard/analysis.slurm",
      .path,
      paste0("--method=", data_type),
      paste0("--domain='", dom, "'"),
      paste0("--scale=", .scale),
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
    cat(paste0("The log file for the call to the HPC will be:", out_log_file, "\n"), file = out_other_file, append = TRUE)

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
}

cat(paste0("\nEnd fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))

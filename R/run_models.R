print("I AM HERE")
library(tidyverse)
library(processx)
## library(dplyr)
## library(processx)

run_models_log <- "~/data/run_models.log"
cat("Start of run_models\n", file = run_models_log) 
message(run_models_log)
run_location <- "HPC" #"local"
print(run_location)

args <- commandArgs()

## fake args
## args = c(
##   "run_models.R",
##   "--method=fish",
##   "--scale=reef",
##   "--domain=John Brewer Reef",
##   "--log=../../data/dashboard.log"
## )
##

message(args)
print("The arguments are:")
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
}

print(paste0("data_type: ", data_type))
print(paste0("data_scale: ", data_scale))
print(Sys.info()["user"])

has_domain_argument <- any(grepl("--domain=.*", args, perl = TRUE))
if(has_domain_argument) {
  arg <- args[grep("--domain=.*", args)]
  domain <- gsub("--domain=(.)", "\\1", arg)
}
print(paste0("domain: ", domain))
    ## file.append(file1 = old_log_file, file2 = out_log_file)
print(paste0("out_log_file: ", out_log_file))
cat(paste0("out_log_file:", out_log_file, "\n"), file = run_models_log, append = TRUE)


message(paste0("data_type: ", data_type))
message(paste0("data_scale: ", data_scale))
## Sys.sleep(30)

if (data_type == "photo-transect" & data_scale == "reef") {
  cat(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  for (rf in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    ## file.copy(from = out_log_file, to = old_log_file)
    system2("docker",
            args = c(
              "run",
              "-i",
              "--rm",
              "-v /etc/localtime:/etc/localtime",
              "-v /etc/timezone:/etc/timezone",
              "-v /home/mlogan/dev:/home/Project",
              "-v /home/mlogan/data:/data",
              "-w /home/Project/R",
              ## "ltmp-monitoring-model:latest",
              "ghcr.io/open-aims/ltmp_dashboard:dev",
              "Rscript",
              "/home/Project/R/00_main.R",
              shQuote(paste0("--path='/data/", data_type,
                             "/2021-01-14/process/ALL/2024/ALL",
                             "/reef/", rf,
                             "/raw/reef_data.zip'")),
              paste0("--method=", data_type),
              shQuote(paste0("--domain=", rf)),
              "--scale=reef",
              "--status=true",
              "--refresh_data=false"
            ),
            stdout = out_log_file,
            wait = TRUE)
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}
if (data_type == "photo-transect" & data_scale == "sector") {
  cat(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  for (sec in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    ## file.copy(from = out_log_file, to = old_log_file)
    system2("docker",
            args = c(
              "run",
              "-i",
              "--rm",
              "-v /etc/localtime:/etc/localtime",
              "-v /etc/timezone:/etc/timezone",
              "-v /home/mlogan/dev:/home/Project",
              "-v /home/mlogan/data:/data",
              "-w /home/Project/R",
              ## "ltmp-monitoring-model:latest",
              "ghcr.io/open-aims/ltmp_dashboard:dev",
              "Rscript",
              "/home/Project/R/00_main.R",
              shQuote(paste0("--path='/data/", data_type,
                             "/2021-01-14/process/ALL/2024/",
                             sec, "/Sectors/", sec,
                             "/raw/reef_data.zip'")),
              paste0("--method=", data_type),
              shQuote(paste0("--domain=", sec)),
              "--scale=Sectors",
              "--status=true",
              "--refresh_data=false"
            ),
            stdout = out_log_file,
            wait = TRUE)
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}
if (data_type == "photo-transect" & data_scale == "nrm") {
  cat(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  for (nrm in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    ## file.copy(from = out_log_file, to = old_log_file)
    system2("docker",
            args = c(
              "run",
              "-i",
              "--rm",
              "-v /etc/localtime:/etc/localtime",
              "-v /etc/timezone:/etc/timezone",
              "-v /home/mlogan/dev:/home/Project",
              "-v /home/mlogan/data:/data",
              "-w /home/Project/R",
              ## "ltmp-monitoring-model:latest",
              "ghcr.io/open-aims/ltmp_dashboard:dev",
              "Rscript",
              "/home/Project/R/00_main.R",
              shQuote(paste0("--path='/data/", data_type,
                             "/2021-01-14/process/ALL/2024/",
                             nrm, "/nrm/", nrm,
                             "/raw/reef_data.zip'")),
              paste0("--method=", data_type),
              shQuote(paste0("--domain=", nrm)),
              "--scale=nrm",
              "--status=true",
              "--refresh_data=false"
            ),
            stdout = out_log_file,
            wait = TRUE)
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}

if (data_type == "manta" & data_scale == "reef") {
  cat(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  for (rf in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    ## file.copy(from = out_log_file, to = old_log_file)
    system2("docker",
            args = c(
              "run",
              "-i",
              "--rm",
              "-v /etc/localtime:/etc/localtime",
              "-v /etc/timezone:/etc/timezone",
              "-v /home/mlogan/dev:/home/Project",
              "-v /home/mlogan/data:/data",
              "-w /home/Project/R",
              ## "ltmp-monitoring-model:latest",
              "ghcr.io/open-aims/ltmp_dashboard:dev",
              "Rscript",
              "/home/Project/R/00_main.R",
              shQuote(paste0("--path='/data/", data_type,
                             "/2021-01-14/process/ALL/2024/ALL",
                             "/reef/", rf,
                             "/raw/reef_data.zip'")),
              paste0("--method=", data_type),
              shQuote(paste0("--domain=", rf)),
              "--scale=reef",
              "--status=true",
              "--refresh_data=false"
            ),
            stdout = out_log_file,
            wait = TRUE)
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}
if (data_type == "manta" & data_scale == "sector") {
  cat(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  for (sec in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    ## file.copy(from = out_log_file, to = old_log_file)
    system2("docker",
            args = c(
              "run",
              "-i",
              "--rm",
              "-v /etc/localtime:/etc/localtime",
              "-v /etc/timezone:/etc/timezone",
              "-v /home/mlogan/dev:/home/Project",
              "-v /home/mlogan/data:/data",
              "-w /home/Project/R",
              ## "ltmp-monitoring-model:latest",
              "ghcr.io/open-aims/ltmp_dashboard:dev",
              "Rscript",
              "/home/Project/R/00_main.R",
              shQuote(paste0("--path='/data/", data_type,
                             "/2021-01-14/process/ALL/2024/",
                             sec, "/Sectors/", sec,
                             "/raw/reef_data.zip'")),
              paste0("--method=", data_type),
              shQuote(paste0("--domain=", sec)),
              "--scale=Sectors",
              "--status=true",
              "--refresh_data=false"
            ),
            stdout = out_log_file,
            wait = TRUE)
    ## system(paste0("cat ", old_log_file, " ", out_log_file, " > ", out_log_file)) 
    ## system(paste0("cat ", old_log_file, " ", out_log_file, " > temp.log")) 
    ## file.create("temp.log")
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}
if (data_type == "manta" & data_scale == "nrm") {
  cat(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  for (nrm in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    ## file.copy(from = out_log_file, to = old_log_file)
    system2("docker",
            args = c(
              "run",
              "-i",
              "--rm",
              "-v /etc/localtime:/etc/localtime",
              "-v /etc/timezone:/etc/timezone",
              "-v /home/mlogan/dev:/home/Project",
              "-v /home/mlogan/data:/data",
              "-w /home/Project/R",
              ## "ltmp-monitoring-model:latest",
              "ghcr.io/open-aims/ltmp_dashboard:dev",
              "Rscript",
              "/home/Project/R/00_main.R",
              shQuote(paste0("--path='/data/", data_type,
                             "/2021-01-14/process/ALL/2024/",
                             nrm, "/nrm/", nrm,
                             "/raw/reef_data.zip'")),
              paste0("--method=", data_type),
              shQuote(paste0("--domain=", nrm)),
              "--scale=nrm",
              "--status=true",
              "--refresh_data=false"
            ),
            stdout = out_log_file,
            wait = TRUE)
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}

print(data_type)
print(data_scale)

## print("here we go")
## print("Juveniles analysis ====")

if (data_type == "juveniles" & data_scale == "sector") {
  cat(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  message(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  for (sec in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    ## file.copy(from = out_log_file, to = old_log_file)
    system2("docker",
            args = c(
              "run",
              "-i",
              "--rm",
              "-v /etc/localtime:/etc/localtime",
              "-v /etc/timezone:/etc/timezone",
              "-v /home/mlogan/dev:/home/Project",
              "-v /home/mlogan/data:/data",
              "-w /home/Project/R",
              ## "ltmp-monitoring-model:latest",
              "ghcr.io/open-aims/ltmp_dashboard:dev",
              "Rscript",
              "/home/Project/R/00_main.R",
              ## shQuote(paste0("--path='/data/", data_type,
              shQuote(paste0("--path='/data/", "juvenile",
                             "/2021-01-14/process/ALL/2024/",
                             sec, "/Sectors/", sec,
                             "/raw/reef_data.zip'")),
              ## paste0("--method=", data_type),
              paste0("--method=", "juveniles"),
              shQuote(paste0("--domain=", sec)),
              "--scale=Sectors",
              "--status=true",
              "--refresh_data=false"
            ),
            stdout = out_log_file,
            wait = TRUE)
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}

if (data_type == "juveniles" & data_scale == "nrm") {
  cat(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  for (nrm in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    ## file.copy(from = out_log_file, to = old_log_file)
    system2("docker",
            args = c(
              "run",
              "-i",
              "--rm",
              "-v /etc/localtime:/etc/localtime",
              "-v /etc/timezone:/etc/timezone",
              "-v /home/mlogan/dev:/home/Project",
              "-v /home/mlogan/data:/data",
              "-w /home/Project/R",
              ## "ltmp-monitoring-model:latest",
              "ghcr.io/open-aims/ltmp_dashboard:dev",
              "Rscript",
              "/home/Project/R/00_main.R",
              ## shQuote(paste0("--path='/data/", data_type,
              shQuote(paste0("--path='/data/", "juvenile",
                             "/2021-01-14/process/ALL/2024/",
                             nrm, "/nrm/", nrm,
                             "/raw/reef_data.zip'")),
              ## paste0("--method=", data_type),
              paste0("--method=", "juveniles"),
              shQuote(paste0("--domain=", nrm)),
              "--scale=nrm",
              "--status=true",
              "--refresh_data=false"
            ),
            stdout = out_log_file,
            wait = TRUE)
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}

if (data_type == "juveniles" & data_scale == "reef") {
  cat(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  for (rf in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    ## file.copy(from = out_log_file, to = old_log_file)
    system2("docker",
            args = c(
              "run",
              "-i",
              "--rm",
              "-v /etc/localtime:/etc/localtime",
              "-v /etc/timezone:/etc/timezone",
              "-v /home/mlogan/dev:/home/Project",
              "-v /home/mlogan/data:/data",
              "-w /home/Project/R",
              ## "ltmp-monitoring-model:latest",
              "ghcr.io/open-aims/ltmp_dashboard:dev",
              "Rscript",
              "/home/Project/R/00_main.R",
              ## shQuote(paste0("--path='/data/", data_type,
              shQuote(paste0("--path='/data/", "juvenile",
                             "/2021-01-14/process/ALL/2024/ALL",
                             "/reef/", rf,
                             "/raw/reef_data.zip'")),
              ## paste0("--method=", data_type),
              paste0("--method=", "juveniles"),
              shQuote(paste0("--domain=", rf)),
              "--scale=reef",
              "--status=true",
              "--refresh_data=false"
            ),
            stdout = out_log_file,
            wait = TRUE)
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}


## print("Fish analysis ====")

if (data_type == "fish" & data_scale == "reef" & 1 == 1) {
  print("Step 1")
  for (rf in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    system2("docker",
            args = c(
              "run",
              "-i",
              "--rm",
              "-v /etc/localtime:/etc/localtime",
              "-v /etc/timezone:/etc/timezone",
              "-v /home/mlogan/dev:/home/Project",
              "-v /home/mlogan/data:/data",
              "-w /home/Project/R",
              ## "ltmp-monitoring-model:latest",
              "ghcr.io/open-aims/ltmp_dashboard:dev",
              "Rscript",
              "/home/Project/R/00_main.R",
              shQuote(paste0("--path='/data/", data_type,
                             "/2021-01-14/process/ALL/2024/ALL",
                             "/reef/", rf,
                             "/raw/reef_data.zip'")),
              paste0("--method=", data_type),
              shQuote(paste0("--domain=", rf)),
              "--scale=reef",
              "--status=true",
              "--refresh_data=false"
            ),
            stdout = out_log_file,
            wait = TRUE)
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}
## print("now here Fish analysis ====")
## print("and now here Fish analysis ====")
if (data_type == "fish" & data_scale == "sector") {
  cat(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  for (sec in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    ## file.copy(from = out_log_file, to = old_log_file)
    system2("docker",
            args = c(
              "run",
              "-i",
              "--rm",
              "-v /etc/localtime:/etc/localtime",
              "-v /etc/timezone:/etc/timezone",
              "-v /home/mlogan/dev:/home/Project",
              "-v /home/mlogan/data:/data",
              "-w /home/Project/R",
              ## "ltmp-monitoring-model:latest",
              "ghcr.io/open-aims/ltmp_dashboard:dev",
              "Rscript",
              "/home/Project/R/00_main.R",
              shQuote(paste0("--path='/data/", data_type,
                             "/2021-01-14/process/ALL/2024/",
                             sec, "/Sectors/", sec,
                             "/raw/reef_data.zip'")),
              paste0("--method=", data_type),
              shQuote(paste0("--domain=", sec)),
              "--scale=Sectors",
              "--status=true",
              "--refresh_data=false"
            ),
            stdout = out_log_file,
            wait = TRUE)
    ## system(paste0("cat ", old_log_file, " ", out_log_file, " > ", out_log_file)) 
    ## system(paste0("cat ", old_log_file, " ", out_log_file, " > temp.log")) 
    ## file.create("temp.log")
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}

## print("and yet now here Fish analysis ====")
if (data_type == "fish" & data_scale == "nrm") {
  cat(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  for (nrm in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    ## file.copy(from = out_log_file, to = old_log_file)
    system2("docker",
            args = c(
              "run",
              "-i",
              "--rm",
              "-v /etc/localtime:/etc/localtime",
              "-v /etc/timezone:/etc/timezone",
              "-v /home/mlogan/dev:/home/Project",
              "-v /home/mlogan/data:/data",
              "-w /home/Project/R",
              ## "ltmp-monitoring-model:latest",
              "ghcr.io/open-aims/ltmp_dashboard:dev",
              "Rscript",
              "/home/Project/R/00_main.R",
              shQuote(paste0("--path='/data/", data_type,
                             "/2021-01-14/process/ALL/2024/",
                             nrm, "/nrm/", nrm,
                             "/raw/reef_data.zip'")),
              paste0("--method=", data_type),
              shQuote(paste0("--domain=", nrm)),
              "--scale=nrm",
              "--status=true",
              "--refresh_data=false"
            ),
            stdout = out_log_file,
            wait = TRUE)
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}

## these are dev versions------------------------------------0

if (data_type == "fish" & data_scale == "reef" & 1 == 2) {
  print("away we go")
  print("here we really go")
  print(paste0("Start fitting of ", data_type,", ", data_scale ," data\n============================================="))
  for (rf in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    print("inside HPC bit")
    message("inside HPC bit")
    out_error_file <- gsub(".log", ".err", out_log_file)
    out_other_file <- gsub(".log", ".other", out_log_file)
    print(out_other_file)
    message(out_other_file)
    
    ## mirror the appropriate path on the HPC
    ## Note, need to surround the non ~ part of the path in single quotes
    ## inorder to accomodate spaces in the path
    hpc_data_path <- paste0("~/'Projects/LTMP_dashboard/data/",
                            data_type,
                            "/2021-01-14/process/ALL/2024/ALL/reef/",
                            rf, "/raw/'")
    print(paste0("HPC path:", hpc_data_path))
    cat("starting\n", file = out_other_file, append = FALSE)

    ## Format the hpc_data_path into a command that can be run on the HPC
    remote_cmd <- sprintf("\"mkdir -p %s\"", hpc_data_path)
    print(paste0("HPC ssh mkdir -p cmd:", remote_cmd))
    system2("ssh",
            args = c(
              "hpc",
              remote_cmd
            ),
            stdout = out_other_file,
            stderr =  out_error_file,
            wait = TRUE
            )
    ## Copy the input zip file over the the HPC
    source_path <- paste0(
      "'/home/shiny/data/", data_type, 
      "/2021-01-14/process/ALL/2024/ALL/reef/", rf, 
      "'/raw/reef_data.zip"
    )

    ## Construct the destination path
    ##Need to make sure all the spaces are escaped
    destination_path <- paste0(
      "hpc:", gsub(" ", "\\\\ ", hpc_data_path), "reef_data.zip"
    )

    ## Debugging: Print the constructed paths
    cat(paste0("Source path:", source_path,"\n"), file = out_other_file, append = TRUE)
    cat(paste0("Destination path:", destination_path,"\n"), file = out_other_file, append = TRUE)

    ## Execute the scp command
    system2(
      "scp",
      args = c(source_path, destination_path),
      wait = TRUE
    )

    print("File copied")

    ## Run singularity on the HPC
    
    remote_cmd <- paste(
      "jid=$(",
      "sbatch",
      ## "--parsable",  # to ensure that only the slurm job number is returned (rather than Submitted batch job #)
      "~/Projects/LTMP_dashboard/analysis.slurm",
      paste0("--path='", "/data/", data_type,
             "/2021-01-14/process/ALL/2024/ALL",
             "/reef/", rf,
             ## "/reef/", gsub(" ", "\\\\ ", rf),
             "/raw/reef_data.zip'"),
      paste0("--method=", data_type),
      paste0("--domain=\"", rf, "\""),
      "--scale=reef",
      "--status=true",
      "--refresh_data=true",
      ");",
      "echo \"Submitted job $jid\";"#,
      ## ## "log=~/Projects/LTMP_dashboard/analysis_${jid}.log;",
      ## "log=~/Projects/LTMP_dashboard/test.log;",
      ## ## "while [ ! -f $log ]; do sleep 1; done;",
      ## ## "tail -f $log;"
      ## ## "while squeue -j \"$jid\" >/dev/null 2>&1; do",
      ## ## "tail -n +1 \"$log\";",
      ## ## "sleep 10;",
      ## ## "done;",
      ## "echo \"Job $jid\" finished;"
    )
    cat(paste0("sbatch:", remote_cmd,"\n"), file = out_other_file, append = TRUE)
    cat(paste0("\n\n", paste("bash -l -c", shQuote(remote_cmd)),"\n"), file = out_other_file, append = TRUE)

    cat(paste(remote_cmd, "\n"))
    process <- processx::process$new(
                                   "ssh", 
                                   args = c(
                                     "hpc",
                                     ## paste0("bash -l -c 'sbatch ~/Projects/LTMP_dashboard/analysis.slurm --scale=reef --status=true --refresh_data=false'")
                                     paste("bash -l -c", shQuote(remote_cmd))
                                     ## paste("bash -l -c", shQuote(remote_cmd)),
                                   ),
                                   stdout = out_log_file,
                                   ## stdout = out_other_file,
                                   ## stderr =  out_error_file
                                   ## stdout = "|",
                                   stderr =  out_error_file
                                 )
    process$wait()
    cat(paste0("After the processx call:","\n"), file = out_other_file, append = TRUE)
  }
  ## file.append(file1 = old_log_file, file2 = out_log_file)
}
if (data_type == "fish" & data_scale == "reef" & 1 == 2) {
  print("Step 1")
  ## print(paste0("Start fitting of ", data_type,", ", data_scale ," data\n============================================="))
  ## cat(paste0("Start fitting of ", data_type,", ", data_scale ," data\n=============================================\n"))
  for (rf in domain) {
    old_log_file <- gsub(".log", ".old", out_log_file)
    ## file.copy(from = out_log_file, to = old_log_file)
    ## if (run_location == "local") {
    if (run_location == "HPC") {
      system2("docker",
              args = c(
                "run",
                "-i",
                "--rm",
                "-v /etc/localtime:/etc/localtime",
                "-v /etc/timezone:/etc/timezone",
                "-v /home/mlogan/dev:/home/Project",
                "-v /home/mlogan/data:/data",
                "-w /home/Project/R",
                ## "ltmp-monitoring-model:latest",
                "ghcr.io/open-aims/ltmp_dashboard:dev",
                "Rscript",
                "/home/Project/R/00_main.R",
                shQuote(paste0("--path='/data/", data_type,
                               "/2021-01-14/process/ALL/2024/ALL",
                               "/reef/", rf,
                               "/raw/reef_data.zip'")),
                paste0("--method=", data_type),
                shQuote(paste0("--domain=", rf)),
                "--scale=reef",
                "--status=true",
                "--refresh_data=false"
              ),
              stdout = out_log_file,
              wait = TRUE)
    } else {   ## run_location == "HPC"
      print("inside HPC bit")
      message("inside HPC bit")
      
      ## send the zip of data to a path
      hpc_data_path <- paste0("~/Projects/LTMP_dashboard/data/",
                              , data_type,
                             "/2021-01-14/process/ALL/2024/ALL",
                             "/reef/", rf)
      out_error_file <- gsub(".log", ".err", out_log_file)
      out_other_file <- gsub(".log", ".other", out_log_file)
      ## print(out_other_file)
      message(out_other_file)
      ## system2("ssh",
      ##         args = c(
      ##           "hpc",
      ##           ## shQuote(paste0("mkdir -p ", hpc_data_path))
      ##           ## shQuote(paste0("mkdir -p /export/home/l-p/mlogan/Projects/LTMP_dashboard/data/reef"))
      ##           ## "whoami"
      ##           ## shQuote(paste0("mkdir -p /export/home/l-p/mlogan/Projects/LTMP_dashboard/data/j"))
      ##           ## shQuote(paste0("mkdir -p /export/home/l-p/mlogan/Projects/LTMP_dashboard/data/j"))
      ##           "mkdir -p /export/home/l-p/mlogan/Projects/LTMP_dashboard/data/j"
      ##         ),
      ##         stdout = out_log_file,
      ##         ## stdout = "~/data/AAA.log",
      ##         ## stdout = "|",
      ##         ## stderr =  out_error_file,
      ##         stderr =  out_other_file,
      ##         wait = TRUE
      ##         )
      print("Finished copying")
      if (1 == 2) {
        system2("scp",
                args = c(
                  shQuote(paste0("/home/shiny/data/", data_type,
                                 "/2021-01-14/process/ALL/2024/ALL",
                                 "/reef/", rf,
                                 "/raw/reef_data.zip")),
                  shQuote(paste0("hpc:", hpc_data_path, "raw/reef_data.zip"))
                ),
                stdout = out_log_file,
                ## stdout = out_other_file,
                stderr =  out_error_file,
                wait = TRUE
                )
        ## file.copy(from = out_error_file, to = run_models_log)
        cat(paste0("files copied to the HPC\n"), file = run_models_log, append = TRUE)
      }
      if (1 == 2) {
        ## Submit the slurm job on the HPC
        ## system2("ssh",
        ##         args = c(
        ##           "hpc",
        ##           shQuote(paste0("bash -l -c 'sbatch ~/Projects/LTMP_dashboard/analysis.slurm --scale=reef --status=true --refresh_data=false'"))
        ##         ),
        ##         stdout = out_log_file,
        ##         stderr =  out_error_file,
        ##         wait = TRUE
        ##         )

        ## remote_cmd <- paste(
        ##   "sbatch",
        ##   ## "--parsable",
        ##   "~/Projects/LTMP_dashboard/analysis.slurm",
        ##   "--scale=reef"
        ##   )
        remote_cmd <- paste(
          "jid=$(",
          "sbatch",
          ## "--parsable",  # to ensure that only the slurm job number is returned (rather than Submitted batch job #)
          "~/Projects/LTMP_dashboard/analysis.slurm",
          paste0("--path=", "data/", data_type,
                 "/2021-01-14/process/ALL/2024/ALL",
                 "/reef/", rf,
                 "/raw/reef_data.zip"),
          paste0("--method=", data_type),
          paste0("--domain=", rf),
          "--scale=reef",
          "--status=true",
          "--refresh_data=true"
          ");",
          "echo \"Submitted job $jid\";"#,
          ## ## "log=~/Projects/LTMP_dashboard/analysis_${jid}.log;",
          ## "log=~/Projects/LTMP_dashboard/test.log;",
          ## ## "while [ ! -f $log ]; do sleep 1; done;",
          ## ## "tail -f $log;"
          ## ## "while squeue -j \"$jid\" >/dev/null 2>&1; do",
          ## ## "tail -n +1 \"$log\";",
          ## ## "sleep 10;",
          ## ## "done;",
          ## "echo \"Job $jid\" finished;"
        )
        ##       remote_cmd <- "
        ## jid=$(sbatch --parsable ~/path/analysis.slurm --scale=reef);
        ## echo \"Submitted job $jid\";
        ## log=~/Projects/LTMP_dashboard/analysis_${jid}.log;
        ## while [ ! -f $log ]; do sleep 1; done;
        ## tail -f $log
        ## " 
        cat(paste(remote_cmd, "\n"))
        process <- processx::process$new(
                                       "ssh", 
                                       args = c(
                                         "hpc",
                                         ## paste0("bash -l -c 'sbatch ~/Projects/LTMP_dashboard/analysis.slurm --scale=reef --status=true --refresh_data=false'")
                                         paste("bash -l -c", shQuote(remote_cmd))
                                         ## paste("bash -l -c", shQuote(remote_cmd)),
                                       ),
                                       ## stdout = out_log_file,
                                       ## stdout = out_other_file,
                                       ## stderr =  out_error_file
                                       stdout = "|",
                                       stderr =  out_error_file
                                     )
        ## while (process$is_alive()) {
        ##   cat(file = out_log_file, process$read_output_lines(), sep = "\n")
        ##   ## cat(p$read_error_lines(), sep = "\n")
        ##   Sys.sleep(1)
        ## }
        ## cat(file = out_log_file, process$read_all_output_lines(), sep = "\n")
        ## cat(file = out_other_file, process$print(), sep = "\n", append = TRUE)
        process$print()
        ## process$read_all_output_lines()
        ## process$wait()
        ## cat(file = out_other_file, "print", sep = "\n", append = TRUE)
        ## ## cat(file = out_other_file, process$read_all_output_lines(), sep = "\n", append = TRUE)
        ## cat(file = out_other_file, "hello", sep = "\n", append = TRUE)

        ## when the job is complete:
        ## 1. zip the contents of ltmp_dashboard/data/modelled/*.csv (actually, if parallel, may need to be able to distinguish)
        ## 2. scp this zip back to the appropriate local directory
        ## 3. unzip the zip file
        ## 4. scp the log file back
      }
    }
    file.append(file1 = old_log_file, file2 = out_log_file)
  }
}

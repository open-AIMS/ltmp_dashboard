
library(tidyverse)
## library(processx)

args <- commandArgs()

message(args)
## print(args)
  
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


print(data_type)
print(data_scale)
print(Sys.info()["user"])

has_domain_argument <- any(grepl("--domain=.*", args, perl = TRUE))
if(has_domain_argument) {
  arg <- args[grep("--domain=.*", args)]
  domain <- gsub("--domain=(.)", "\\1", arg)
}
print(domain)
    ## file.append(file1 = old_log_file, file2 = out_log_file)
print(out_log_file)

message(data_type)
message(data_scale)
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


if (data_type == "fish" & data_scale == "reef") {
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


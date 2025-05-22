## Run the following
## docker container kill ltmp-monitoring-model:latest
##1. Run the docker container
### on dev VM
#### docker run --entrypoint R -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest
#### docker run --rm -it --entrypoint R -v ~/data:/home/analysis/data2 -v ~/dev/R:/home/analysis/scripts --name ltmp ltmp

## If running from terminal command line
##docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript /home/Project/R/00_main.R

### on server
##2. direct editor to run R within the container
## M-x ess-remote
##3. set wd to /home/Project/R
##setwd("/home/Project/R")

## remotes::install_github('open-AIMS/status', ref = 'adapt', force = TRUE)


## Remove the following and replace with pulling from a repo
#system("cp -r /data/spatial ../data/spatial")


library(status)

source("ltmp_startup_functions.R")


args <- commandArgs()

## print(args)
## print("more")

if (ltmp_is_parent()) ltmp_start_matter(args)
## print(status::get_setting(element = "data_method"))
## Photo-transect analyses
if (status::get_setting(element = "data_method") == "photo-transect") 
  source("ltmp_pt_cover.R")

## Manta-tow analyses
if (status::get_setting(element = "data_method") == "manta") 
  source("ltmp_manta_cover.R")

## Juveniles analyses
if (status::get_setting(element = "data_method") == "juveniles") 
  source("ltmp_juvenile_cover.R")

## Fish analyses
if (status::get_setting(element = "data_method") == "fish") 
  source("ltmp_fish_cover.R")


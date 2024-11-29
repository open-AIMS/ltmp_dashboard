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
setwd("/home/Project/R")

remotes::install_github('open-AIMS/status', ref = 'adapt', force = TRUE)


## Remove the following and replace with pulling from a repo
system("cp -r /data/spatial ../data/spatial")



library(status)

source("ltmp_startup_functions.R")

## assign(x = "DATA_TYPE", value = "COVER", envir = globalenv())
## assign(x = "DATA_PROGRAM", value = "LTMP", envir = globalenv())

## args <- commandArgs()
## print(args)

args <- commandArgs()

## Photo-transect ------
## args = c(
##   "R",
##   "Rscript",
##   "--file=pt_cover.R",                            ##the name of the target script
##   "--path=/data/photo-transect/2021-01-14/process/ALL/2024//Burdekin/nrm/Burdekin/raw/reef_data.zip",
##   ## "--path=s3://dev-aims-gov-au-data/5a8a4b00-4ade-11dc-8f56-00008a07204e/photo-transect/2021-01-14/process/ALL/2024/Burdekin/nrm/Burdekin/raw/reef_data.zip",   ##path to data
##   "--method=photo-transect",
##   "--domain=Burdekin",
##   "--scale=nrm",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

## args = c(
##   "R",
##   "Rscript",
##   "--file=pt_cover.R",                            ##the name of the target script
##   "--path=/data/photo-transect/2021-01-14/process/ALL/2024/Mackay Whitsunday/nrm/Mackay Whitsunday/raw/reef_data.zip",
##   "--method=photo-transect",
##   "--domain=Mackay Whitsunday",
##   "--scale=nrm",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

## args = c(
##   "R",
##   "Rscript",
##   "--file=pt_cover.R",                            ##the name of the target script
##   "--path=/data/photo-transect/2021-01-14/process/ALL/2024/Burnett Mary/nrm/Burnett Mary/raw/reef_data.zip",
##   "--method=photo-transect",
##   "--domain=Burnett Mary",
##   "--scale=nrm",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )


## args = c(
##   "R",
##   "Rscript",
##   "--file=pt_cover.R",                            ##the name of the target script
##   "--path=/data/photo-transect/2021-01-14/process/ALL/2024/ALL/reef/Barren Island/raw/reef_data.zip",
##   "--method=photo-transect",
##   "--domain=Barren Island",
##   "--scale=reef",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )


## args = c(
##   "R",
##   "Rscript",
##   "--file=pt_cover.R",                            ##the name of the target script
##   "--path=/data/photo-transect/2021-01-14/process/ALL/2024/ALL/reef/Havannah Island/raw/reef_data.zip",
##   "--method=photo-transect",
##   "--domain=Havannah Island",
##   "--scale=reef",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )


## args = c(
##   "R",
##   "Rscript",
##   "--file=pt_cover.R",                            ##the name of the target script
##   "--path=/data/photo-transect/2021-01-14/process/ALL/2024/ALL/reef/Hayman Island/raw/reef_data.zip",
##   "--method=photo-transect",
##   "--domain=Hayman Island",
##   "--scale=reef",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

## args = c(
##   "R",
##   "Rscript",
##   "--file=pt_cover.R",                            ##the name of the target script
##   "--path=/data/photo-transect/2021-01-14/process/ALL/2024/ALL/reef/Pandora Reef/raw/reef_data.zip",
##   "--method=photo-transect",
##   "--domain=Pandora Reef",
##   "--scale=reef",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

## args = c(
##   "R",
##   "Rscript",
##   "--file=pt_cover.R",                            ##the name of the target script
##   "--path=/data/photo-transect/2021-01-14/process/ALL/2024/Wet Tropics/nrm/Wet Tropics/raw/reef_data.zip",
##   "--method=photo-transect",
##   "--domain=Wet Tropics",
##   "--scale=nrm",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

## args = c(
##   "R",
##   "Rscript",
##   "--file=pt_cover.R",                            ##the name of the target script
##   "--path=/data/photo-transect/2021-01-14/process/ALL/2024/CA/Sectors/CA/raw/reef_data.zip",
##   "--method=photo-transect",
##   "--domain=CA",
##   "--scale=Sectors",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

## Manta ---------------
## args = c(
##   "R",
##   "Rscript",
##   "--file=manta_cover.R",                            ##the name of the target script
##   "--path=/data/manta/2021-01-14/process/ALL/2024/ALL/reef/Bowden Reef/raw/reef_data.zip",
##   "--method=manta",
##   "--domain=Bowden Reef",
##   "--scale=reef",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )
## args = c(
##   "R",
##   "Rscript",
##   "--file=manta_cover.R",                            ##the name of the target script
##   "--path=/data/manta/2021-01-14/process/ALL/2024/ALL/reef/Briggs Reef/raw/reef_data.zip",
##   "--method=manta",
##   "--domain=Briggs Reef",
##   "--scale=reef",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )
## args = c(
##   "R",
##   "Rscript",
##   "--file=manta_cover.R",                            ##the name of the target script
##   "--path=/data/manta/2021-01-14/process/ALL/2024/ALL/reef/Reef 22-084/raw/reef_data.zip",
##   "--method=manta",
##   "--domain=Reef 22-084",
##   "--scale=reef",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )
## args = c(
##   "R",
##   "Rscript",
##   "--file=manta_cover.R",                            ##the name of the target script
##   "--path=/data/manta/2021-01-14/process/ALL/2024/CL/Sectors/CL/raw/reef_data.zip",
##   "--method=manta",
##   "--domain=CL",
##   "--scale=Sectors",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

## args = c(
##   "R",
##   "Rscript",
##   "--file=manta_cover.R",                            ##the name of the target script
##   "--path=/data/manta/2021-01-14/process/ALL/2024/Fitzroy/nrm/Fitzroy/raw/reef_data.zip",
##   "--method=manta",
##   "--domain=Fitzroy",
##   "--scale=nrm",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

## Juvenile ------------
## args = c(
##   "R",
##   "Rscript",
##   "--file=pt_cover.R",                            ##the name of the target script
##   "--path=/juvenile/2021-01-14/process/MMP/2024//PO/reef/Snapper Island/raw/reef_data.zip",
##   "--method=juvenile",
##   "--domain=Snapper Island",
##   "--scale=reef",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )
## args = c(
##   "R",
##   "Rscript",
##   "--file=pt_cover.R",                            ##the name of the target script
##   "--path=/juvenile/2021-01-14/process/ALL/2024/Wet Tropics/nrm/Wet Tropics/raw/reef_data.zip",
##   "--method=juvenile",
##   "--domain=Wet Tropics",
##   "--scale=nrm",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

if (ltmp_is_parent()) ltmp_start_matter(args)

## Photo-transect analyses
if (status::get_setting(element = "data_method") == "photo-transect") 
  source("ltmp_pt_cover.R")

## Manta-tow analyses
if (status::get_setting(element = "data_method") == "manta") 
  source("ltmp_manta_cover.R")

## Juveniles analyses
if (status::get_setting(element = "data_method") == "juvenile") 
  source("ltmp_juvenile_cover.R")

## Fish analyses

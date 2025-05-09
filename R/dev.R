## Fully through docker
docker pull ghcr.io/open-aims/ltmp_dashboard:dev
docker run --entrypoint R --rm -v ~/dev:/home/Project -v ~/data:/data ghcr.io/open-aims/ltmp_dashboard:dev Rscript /home/Project/R/00_main.R --path='/data/fish/2021-01-14/process/ALL/2024/ALL/reef/Pompey Reef No.1/raw/reef_data.zip' --method=fish --domain='Pompey Reef No.1' --scale=reef

#### docker run --entrypoint R -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest

setwd("/home/Project/R")
system("cp -r /data/spatial ../data/spatial")
library(status)
source("ltmp_startup_functions.R")

args = c(
  "R",
  "Rscript",
  "--file=fish_cover.R",                            ##the name of the target script
  "--path=/data/fish/2021-01-14/process/ALL/2024/ALL/reef/No Name Reef/raw/reef_data.zip",
  "--method=fish",
  "--domain=No Name Reef",
  "--scale=reef",
  "--status=true",                                  ##whether to show full status 
  "--refresh_data=false"                            ##whether to refresh all the data 
)
args = c(
  "R",
  "Rscript",
  "--file=fish_cover.R",                            ##the name of the target script
  "--path=/data/fish/2021-01-14/process/ALL/2024/ALL/reef/Pompey Reef No.1/raw/reef_data.zip",
  "--method=fish",
  "--domain=Pompey Reef No.1",
  "--scale=reef",
  "--status=true",                                  ##whether to show full status 
  "--refresh_data=false"                            ##whether to refresh all the data 
)

args = c(
  "R",
  "Rscript",
  "--file=manta_cover.R",                            ##the name of the target script
  "--path=/data/manta/2021-01-14/process/ALL/2024/ALL/reef/JUKES REEF/raw/reef_data.zip",
  "--method=manta",
  "--domain=JUKES REEF",
  "--scale=reef",
  "--status=true",                                  ##whether to show full status 
  "--refresh_data=false"                            ##whether to refresh all the data 
)


source("ltmp_startup_functions.R")
source("ltmp_model_functions.R")
if (ltmp_is_parent()) ltmp_start_matter(args)

status::status_set_stage(stage = 4, title = "Fit models")

for (s in  str_subset(status_$status[[4]]$items, "_pt$|_manta$|_juv$"))
  status::remove_status_item(stage = 4, item = s)

filenm <- "../data/modelled/fish_reef_Erskine Island.rds"
data <- readRDS(file = filenm)

dat <- data

.x <- dat$raw_sum[[1]]
.y <- dat$posteriors[[1]]

sub_var <- dat$sub_variable[[1]]
posts <- dat$posteriors[[1]]$year_sum
raw <- dat$raw_sum[[1]] |> 
  mutate(Mean = ifelse(sub_var == "Biomass", Mean_biomass, Mean))

## Photo-transect ------

args = c(
  "R",
  "Rscript",
  "--file=pt_cover.R",                            ##the name of the target script
  "--path=/data/photo-transect/2021-01-14/process/ALL/2024/ALL/reef/Middle Island/raw/reef_data.zip",
  "--method=photo-transect",
  "--domain=Middle Island",
  "--scale=reef",
  "--status=true",                                  ##whether to show full status 
  "--refresh_data=false"                            ##whether to refresh all the data 
)
args = c(
  "R",
  "Rscript",
  "--file=pt_cover.R",                            ##the name of the target script
  "--path=/data/photo-transect/2021-01-14/process/ALL/2024/ALL/reef/Rib Reef/raw/reef_data.zip",
  "--method=photo-transect",
  "--domain=Rib Reef",
  "--scale=reef",
  "--status=true",                                  ##whether to show full status 
  "--refresh_data=false"                            ##whether to refresh all the data 
)

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
##   "--path=/data/photo-transect/2021-01-14/process/ALL/2024/ALL/reef/Centipede Reef/raw/reef_data.zip",
##   "--method=photo-transect",
##   "--domain=Centipede Reef",
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
##   "--path=/data/manta/2021-01-14/process/ALL/2024/ALL/reef/Carter Reef/raw/reef_data.zip",
##   "--method=manta",
##   "--domain=Carter Reef",
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
##   "--path=/data/manta/2021-01-14/process/ALL/2024/CG/Sectors/CG/raw/reef_data.zip",
##   "--method=manta",
##   "--domain=CG",
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

## fish

args = c(
  "R",
  "Rscript",
  "--file=fish_cover.R",                            ##the name of the target script
  "--path=/data/fish/2021-01-14/process/ALL/2024/ALL/reef/Ribb Reef/raw/reef_data.zip",
  "--method=fish",
  "--domain=Ribb Reef",
  "--scale=reef",
  "--status=true",                                  ##whether to show full status 
  "--refresh_data=false"                            ##whether to refresh all the data 
)

## args = c(
##   "R",
##   "Rscript",
##   "--file=fish_cover.R",                            ##the name of the target script
##   "--path=/data/fish/2021-01-14/process/ALL/2024/ALL/reef/Carter Reef/raw/reef_data.zip",
##   "--method=fish",
##   "--domain=Carter Reef",
##   "--scale=reef",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

## args = c(
##   "R",
##   "Rscript",
##   "--file=fish_cover.R",                            ##the name of the target script
##   "--path=/data/fish/2021-01-14/process/ALL/2024/ALL/reef/Centipede Reef/raw/reef_data.zip",
##   "--method=fish",
##   "--domain=Centipede Reef",
##   "--scale=reef",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

## args = c(
##   "R",
##   "Rscript",
##   "--file=fish_cover.R",                            ##the name of the target script
##   "--path=/data/fish/2021-01-14/process/ALL/2024/ALL/reef/Erskine Island/raw/reef_data.zip",
##   "--method=fish",
##   "--domain=Erskine Island",
##   "--scale=reef",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )
## args = c(
##   "R",
##   "Rscript",
##   "--file=fish_cover.R",                            ##the name of the target script
##   "--path=/data/fish/2021-01-14/process/ALL/2024/ALL/reef/No Name Reef/raw/reef_data.zip",
##   "--method=fish",
##   "--domain=No Name Reef",
##   "--scale=reef",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

## args = c(
##   "R",
##   "Rscript",
##   "--file=fish_cover.R",                            ##the name of the target script
##   "--path=/data/fish/2021-01-14/process/ALL/2024/TO/Sectors/TO/raw/reef_data.zip",
##   "--method=fish",
##   "--domain=TO",
##   "--scale=Sectors",
##   "--status=true",                                  ##whether to show full status 
##   "--refresh_data=false"                            ##whether to refresh all the data 
## )

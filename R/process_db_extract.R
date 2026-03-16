library(tidyverse)
args <- commandArgs()

## Change the umask to allow read/write on files and directories created by this script
system("umask 0000")

has_method_argument <- any(grepl("--method=.*", args, perl = TRUE))
if(has_method_argument) {
  arg <- args[grep("--method=.*", args)]
  data_type <- gsub("--method=(.)", "\\1", arg)
}
has_method_argument <- any(grepl("--purpose=.*", args, perl = TRUE))
if(has_method_argument) {
  arg <- args[grep("--purpose=.*", args)]
  purpose <- gsub("--purpose=(.)", "\\1", arg)
}
has_csv_argument <- any(grepl("--csv_file=.*", args, perl = TRUE))
if(has_csv_argument) {
  arg <- args[grep("--csv_file=.*", args)]
  csv_file <- gsub("--csv_file=(.)", "\\1", arg)
  csv_path <- dirname(csv_file)
  filenm <- basename(csv_file) 
  make_log <- paste0(csv_path, "/dashboard.log")
}
has_rds_argument <- any(grepl("--rds_file=.*", args, perl = TRUE))
if(has_rds_argument) {
  arg <- args[grep("--rds_file=.*", args)]
  rds_file <- gsub("--rds_file=(.)", "\\1", arg)
  rds_path <- dirname(rds_file)
  filenm <- basename(rds_file) 
  make_log <- paste0(rds_path, "/dashboard.log")
}


## Clear log
## unlink(make_log)

## update_log <- function(content) {
##  write(content, file = make_log, append = TRUE)
## }
## cat(paste0("The data type is :", data_type, "\n"))
## cat(paste0("The csv_file is :", csv_file, "\n"))
## cat(paste0("The csv_path is :", csv_path, "\n"))
## cat(paste0("The filename is :", filenm, "\n"))

## photo-transect

if (data_type == "photo-transect" & purpose == "post-process") {
  cat("Start post-processing of photo-transect data\n=============================================\n")
  cat("Read in extracted data\n")
  pt <- read_csv(paste0(csv_file)) 
  cat("Process extracted data\n")
  pt <- pt |> 
    mutate(P_CODE = ifelse(P_CODE=="IN", "MMP", "LTMP"),
           SURVEY_DATE = lubridate::ymd_hms(SURVEY_DATE), 
           ## SURVEY_DATE = lubridate::parse_date_time(SURVEY_DATE), 
           ## REEF_ZONE = ifelse(is.null(REEF_ZONE), "_", REEF_ZONE),
           REEF_ZONE = ifelse(is.na(REEF_ZONE), "_", REEF_ZONE),
           LATITUDE = round(LATITUDE, 6),
           LONGITUDE = round(LONGITUDE, 6),
           SITE_DEPTH = ifelse(P_CODE == 'MMP', SITE_DEPTH, 9))
  cat("Save processed data\n")
  rds_file <- gsub(".csv", ".rds", csv_file)
  print(rds_file)
  ## saveRDS(pt, file = paste0(csv_path, '/pt.rds'))
  saveRDS(pt, file = rds_file)
  cat("The following is the first 6 rows of the saved data\n")
  options(width = 200)
  print(head(as.data.frame(pt)))
  cat("\nAnd a glimpse of the saved data\n")
  glimpse(pt)
  options(width = 80)
  cat("Done!\n")
}

if (data_type == "photo-transect" & purpose == "make_reefs") {
  cat("Start preparing reefs for modelling\n=============================================\n")
  if (1 == 1) {
    cat("Read in extracted data\n")
    pt <- readRDS(file = paste0(rds_file))
    cat("Nest data\n")
    pt_reefs <- pt |>
      group_by(AIMS_REEF_NAME) |>
      nest() |>
      mutate(data = map2(.x = data, .y = AIMS_REEF_NAME,
                         .f = ~ .x |>
                           mutate(AIMS_REEF_NAME = .y) |> 
                           dplyr::select(P_CODE, ID = VPOINT_SID,
                                         CRUISE_CODE, REEF_NAME,
                                         AIMS_REEF_NAME,
                                         SECTOR = A_SECTOR,
                                         LATITUDE, LONGITUDE,
                                         SITE_NO, TRANSECT_NO,
                                         SITE_DEPTH, REEF_ZONE,
                                         REPORT_YEAR = REPORT_YEAR,
                                         SURVEY_DATE, FRAME,
                                         POINT_NO, FAMILY,
                                         GROUP_DESC, REEFPAGE_CATEGORY,
                                         SHELF)
                         )) |> 
      mutate(path = map(.x = AIMS_REEF_NAME,
                        .f =  ~ {
                          paste0(rds_path, "/photo-transect",
                                 "/2021-01-14/process/ALL/2024/ALL/reef/",
                                 .x,
                                 "/raw/")                       
                        }
                        )) |> 
      mutate(system = map2(.x = AIMS_REEF_NAME, .y = path,
                           .f =  ~ {
                             .y <- gsub("\\.\\./\\.\\.", "", .y)
                             paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=photo-transect --domain="', .x, '" --scale=reef --status=true --refresh_data=false')
                           }
                           ))
    saveRDS(pt_reefs, file = paste0(rds_path, '/pt_reefs.rds'))
  }
  cat("Nest data\n")
  pt_reefs <- readRDS(file = paste0(rds_path, '/pt_reefs.rds'))
  ## print(pt_reefs$AIMS_REEF_NAME |> unique())
  pwalk(.l = list(pt_reefs$data, pt_reefs$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          ## system(paste0("chmod 777 '", .y, "reef_data.csv'"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
          ## system(paste0("chmod 777 '", .y, "reef_data.zip'"))
          print(.y)
          write(.y, file = make_log, append = TRUE)
          ## write(dir.exists(.y), file = make_log, append = TRUE)
        })
  cat("Done!\n")
}

if (data_type == "photo-transect" & purpose == "make_nrm") {
  cat("Start preparing NRMs for modelling\n=============================================\n")
  if (1 == 1) {
    cat("Read in extracted data\n")
    pt <- readRDS(file = paste0(rds_file))
    cat("Nest data\n")
    print(colnames(pt))
    pt_nrm <- pt |>
      group_by(NRM_REGION) |>
      nest() |>
      mutate(data = map2(.x = data, .y = NRM_REGION,
                         .f = ~ .x |>
                           ## mutate(NRM_REGION = .y) |> 
                           dplyr::select(P_CODE,
                                         ID = VPOINT_SID,
                                         CRUISE_CODE,
                                         REEF_NAME,
                                         AIMS_REEF_NAME,
                                         SECTOR = A_SECTOR,
                                         ## NRM_REGION,
                                         LATITUDE,
                                         LONGITUDE,
                                         SITE_NO,
                                         TRANSECT_NO,
                                         SITE_DEPTH,
                                         REEF_ZONE,
                                         REPORT_YEAR = REPORT_YEAR,
                                         SURVEY_DATE,
                                         FRAME,
                                         POINT_NO,
                                         FAMILY,
                                         GROUP_DESC,
                                         REEFPAGE_CATEGORY,
                                         SHELF)
                         ))  |> 
      mutate(path = map(.x = NRM_REGION,
                        .f =  ~ {
                          paste0(rds_path, "/photo-transect",
                                 "/2021-01-14/process/ALL/2024/", .x, "/nrm/",
                                 .x,
                                 "/raw/")                       
                        }
                        ))  |> 
      mutate(system = map2(.x = NRM_REGION, .y = path,
                           .f =  ~ {
                             .y <- gsub("\\.\\./\\.\\.", "", .y)
                             paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=photo-transect --domain="', .x, '" --scale=nrm --status=true --refresh_data=false')
                           }
                           ))
    cat("Save nested data\n")
    saveRDS(pt_nrm, file = paste0(rds_path, '/pt_nrm.rds'))
  }
  cat("Write individual csv and zips\n")
  pt_nrm <- readRDS(file = paste0(rds_path, '/pt_nrm.rds'))
  ## print(pt_reefs$AIMS_REEF_NAME |> unique())
  pwalk(.l = list(pt_nrm$data, pt_nrm$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          ## system(paste0("chmod 777 '", .y, "reef_data.csv'"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
          ## system(paste0("chmod 777 '", .y, "reef_data.zip'"))
          print(.y)
          write(.y, file = make_log, append = TRUE)
          ## write(dir.exists(.y), file = make_log, append = TRUE)
        })
  cat("Done!\n")
}

if (data_type == "photo-transect" & purpose == "make_sectors") {
  cat("Start preparing Sectors for modelling\n=============================================\n")
  if (1 == 1) {
    cat("Read in extracted data\n")
    pt <- readRDS(file = paste0(rds_file))
    cat("Nest data\n")
    ## print(colnames(pt))
    pt_sector <- pt |>
      dplyr::rename(SECTOR = A_SECTOR) |> 
      group_by(SECTOR) |>
      nest() |>
      mutate(data = map2(.x = data, .y = SECTOR,
                         .f = ~ .x |>
                           mutate(SECTOR = .y) |> 
                           dplyr::select(P_CODE,
                                         ID = VPOINT_SID,
                                         CRUISE_CODE,
                                         REEF_NAME,
                                         ## NRM_REGION,
                                         SECTOR, ## = A_SECTOR,
                                         AIMS_REEF_NAME,
                                         LATITUDE, LONGITUDE,
                                         SITE_NO, TRANSECT_NO,
                                         SITE_DEPTH, REEF_ZONE,
                                         REPORT_YEAR,
                                         SURVEY_DATE, FRAME,
                                         POINT_NO, FAMILY,
                                         GROUP_DESC, REEFPAGE_CATEGORY,
                                         SHELF)
                         )) |>
      mutate(path = map(.x = SECTOR,
                        .f =  ~ {
                          paste0(rds_path, "/photo-transect",
                                 "/2021-01-14/process/ALL/2024/", .x, "/Sectors/",
                                 .x,
                                 "/raw/")                       
                        }
                        ))  |> 
      mutate(system = map2(.x = SECTOR, .y = path,
                           .f =  ~ {
                             .y <- gsub("\\.\\./\\.\\.", "", .y)
                             paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=photo-transect --domain="', .x, '" --scale=Sectors --status=true --refresh_data=false')
                           }
                           ))
    cat("Save nested data\n")
    saveRDS(pt_sector, file = paste0(rds_path, '/pt_sector.rds'))
  }
  cat("Write individual csv and zips\n")
  pt_sector <- readRDS(file = paste0(rds_path, '/pt_sector.rds'))
  ## print(pt_reefs$AIMS_REEF_NAME |> unique())
  pwalk(.l = list(pt_sector$data, pt_sector$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          ## system(paste0("chmod 777 '", .y, "reef_data.csv'"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
          ## system(paste0("chmod 777 '", .y, "reef_data.zip'"))
          print(.y)
          write(.y, file = make_log, append = TRUE)
          ## write(dir.exists(.y), file = make_log, append = TRUE)
        })
  cat("Done!\n")
}

## Juveniles

if (data_type == "juveniles" & purpose == "post-process") {
  cat("Start post-processing of juveniles data\n=============================================\n")
  cat("Read in extracted data\n")
  pt <- read_csv(paste0(csv_file)) 
  cat("Process extracted data\n")
  pt <- pt |> 
    mutate(P_CODE = ifelse(P_CODE=="IN", "MMP", "LTMP"),
           SURVEY_DATE = lubridate::ymd_hms(SURVEY_DATE), 
           ## SURVEY_DATE = lubridate::parse_date_time(SURVEY_DATE), 
           ## REEF_ZONE = ifelse(is.null(REEF_ZONE), "_", REEF_ZONE),
           REEF_ZONE = ifelse(is.na(REEF_ZONE), "_", REEF_ZONE),
           LATITUDE = round(LATITUDE, 6),
           LONGITUDE = round(LONGITUDE, 6),
           SITE_DEPTH = ifelse(P_CODE == 'MMP', SITE_DEPTH, 9))
  cat("Save processed data\n")
  rds_file <- gsub(".csv", ".rds", csv_file)
  print(rds_file)
  ## saveRDS(pt, file = paste0(csv_path, '/pt.rds'))
  saveRDS(pt, file = rds_file)
  cat("The following is the first 6 rows of the saved data\n")
  options(width = 200)
  print(head(as.data.frame(pt)))
  cat("\nAnd a glimpse of the saved data\n")
  glimpse(pt)
  options(width = 80)
  cat("Done!\n")
}

if (data_type == "juveniles" & purpose == "make_reefs") {
  cat("Start preparing reefs for modelling\n=============================================\n")
  if (1 == 1) {
    cat("Read in extracted data\n")
    juv <- readRDS(file = paste0(rds_file))
    cat("Nest data\n")
    juv_reefs <- juv |>
      group_by(AIMS_REEF_NAME) |>
      nest() |>
      mutate(data = map2(.x = data, .y = AIMS_REEF_NAME,
                         .f = ~ .x |>
                           mutate(AIMS_REEF_NAME = .y) |> 
                           dplyr::select(P_CODE,
                                         ## ID = VPOINT_SID,
                                         CRUISE_CODE,
                                         REEF_NAME,
                                         AIMS_REEF_NAME,
                                         SITE_DEPTH,
                                         REEF_ZONE,
                                         SECTOR = A_SECTOR,
                                         LATITUDE, LONGITUDE,
                                         SITE_NO,
                                         REPORT_YEAR = REPORT_YEAR,
                                         SURVEY_DATE, 
                                         AVAILABLE_SUBSTRATE,
                                         AREA_TRANSECT,
                                         ABUNDANCE,
                                         REEFPAGE_CATEGORY,
                                         SHELF)
                         )) |> 
      mutate(path = map(.x = AIMS_REEF_NAME,
                        .f =  ~ {
                          paste0(rds_path, "/juvenile",
                                 "/2021-01-14/process/ALL/2024/ALL/reef/",
                                 .x,
                                 "/raw/")                       
                        }
                        )) |> 
      mutate(system = map2(.x = AIMS_REEF_NAME, .y = path,
                           .f =  ~ {
                             .y <- gsub("\\.\\./\\.\\.", "", .y)
                             paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=juvenile --domain="', .x, '" --scale=reef --status=true --refresh_data=false')
                           }
                           ))
    saveRDS(juv_reefs, file = paste0(rds_path, '/juv_reefs.rds'))
  }
  cat("Nest data\n")
  juv_reefs <- readRDS(file = paste0(rds_path, '/juv_reefs.rds'))
  ## print(juv_reefs$AIMS_REEF_NAME |> unique())
  pwalk(.l = list(juv_reefs$data, juv_reefs$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          ## system(paste0("chmod 777 '", .y, "reef_data.csv'"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
          ## system(paste0("chmod 777 '", .y, "reef_data.zip'"))
          print(.y)
          write(.y, file = make_log, append = TRUE)
          ## write(dir.exists(.y), file = make_log, append = TRUE)
        })
  cat("Done!\n")
}

if (data_type == "juveniles" & purpose == "make_nrm") {
  cat("Start preparing NRMs for modelling\n=============================================\n")
  if (1 == 1) {
    cat("Read in extracted data\n")
    juv <- readRDS(file = paste0(rds_file))
    cat("Nest data\n")
    ## print(colnames(pt))
    juv_nrm <- juv |>
      group_by(NRM_REGION) |>
      nest() |>
      mutate(data = map2(.x = data, .y = NRM_REGION,
                         .f = ~ .x |>
                           mutate(SECTOR = .y) |> 
                           dplyr::select(P_CODE,
                                         ## ID = VPOINT_SID,
                                         CRUISE_CODE,
                                         REEF_NAME,
                                         AIMS_REEF_NAME,
                                         SITE_DEPTH,
                                         REEF_ZONE,
                                         ## NRM_REGION,
                                         SECTOR = A_SECTOR,
                                         LATITUDE, LONGITUDE,
                                         SITE_NO,
                                         ## TRANSECT_NO,
                                         REPORT_YEAR,
                                         SURVEY_DATE, 
                                         AVAILABLE_SUBSTRATE,
                                         AREA_TRANSECT,
                                         ABUNDANCE,
                                         REEFPAGE_CATEGORY,
                                         SHELF)
                         )) |>
      mutate(path = map(.x = NRM_REGION,
                        .f =  ~ {
                          paste0(rds_path, "/juvenile",
                                 "/2021-01-14/process/ALL/2024/", .x, "/nrm/",
                                 .x,
                                 "/raw/")                       
                        }
                        ))  |> 
      mutate(system = map2(.x = NRM_REGION, .y = path,
                           .f =  ~ {
                             .y <- gsub("\\.\\./\\.\\.", "", .y)
                             paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=juvenile --domain="', .x, '" --scale=nrm --status=true --refresh_data=false')
                           }
                           ))
    cat("Save nested data\n")
    saveRDS(juv_nrm, file = paste0(rds_path, '/juv_nrm.rds'))
  }
  cat("Write individual csv and zips\n")
  juv_nrm <- readRDS(file = paste0(rds_path, '/juv_nrm.rds'))
  ## print(pt_reefs$AIMS_REEF_NAME |> unique())
  pwalk(.l = list(juv_nrm$data, juv_nrm$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          ## system(paste0("chmod 777 '", .y, "reef_data.csv'"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
          ## system(paste0("chmod 777 '", .y, "reef_data.zip'"))
          print(.y)
          write(.y, file = make_log, append = TRUE)
          ## write(dir.exists(.y), file = make_log, append = TRUE)
        })
  cat("Done!\n")
}

if (data_type == "juveniles" & purpose == "make_sectors") {
  cat("Start preparing Sectors for modelling\n=============================================\n")
  if (1 == 1) {
    cat("Read in extracted data\n")
    juv <- readRDS(file = paste0(rds_file))
    cat("Nest data\n")
    ## print(colnames(pt))
    juv_sector <- juv |>
      dplyr::rename(SECTOR = A_SECTOR) |> 
      group_by(SECTOR) |>
      nest() |>
      mutate(data = map2(.x = data, .y = SECTOR,
                         .f = ~ .x |>
                           mutate(SECTOR = .y) |> 
                           dplyr::select(P_CODE,
                                         ## ID = VPOINT_SID,
                                         CRUISE_CODE,
                                         REEF_NAME,
                                         AIMS_REEF_NAME,
                                         SITE_DEPTH,
                                         REEF_ZONE,
                                         ## NRM_REGION,
                                         SECTOR, ## = A_SECTOR,
                                         LATITUDE, LONGITUDE,
                                         SITE_NO,
                                         ## TRANSECT_NO,
                                         REPORT_YEAR,
                                         SURVEY_DATE, 
                                         AVAILABLE_SUBSTRATE,
                                         AREA_TRANSECT,
                                         ABUNDANCE,
                                         REEFPAGE_CATEGORY,
                                         SHELF)
                         )) |>
      mutate(path = map(.x = SECTOR,
                        .f =  ~ {
                          paste0(rds_path, "/juvenile",
                                 "/2021-01-14/process/ALL/2024/", .x, "/Sectors/",
                                 .x,
                                 "/raw/")                       
                        }
                        ))  |> 
      mutate(system = map2(.x = SECTOR, .y = path,
                           .f =  ~ {
                             .y <- gsub("\\.\\./\\.\\.", "", .y)
                             paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=juvenile --domain="', .x, '" --scale=Sectors --status=true --refresh_data=false')
                           }
                           ))
    cat("Save nested data\n")
    saveRDS(juv_sector, file = paste0(rds_path, '/juv_sector.rds'))
  }
  cat("Write individual csv and zips\n")
  juv_sector <- readRDS(file = paste0(rds_path, '/juv_sector.rds'))
  ## print(pt_reefs$AIMS_REEF_NAME |> unique())
  pwalk(.l = list(juv_sector$data, juv_sector$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          ## system(paste0("chmod 777 '", .y, "reef_data.csv'"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
          ## system(paste0("chmod 777 '", .y, "reef_data.zip'"))
          print(.y)
          write(.y, file = make_log, append = TRUE)
          ## write(dir.exists(.y), file = make_log, append = TRUE)
        })
  cat("Done!\n")
}


## manta

if (data_type == "manta" & purpose == "post-process") {
  cat("Start post-processing of manta data\n=============================================\n")
  cat("Read in extracted data\n")
  manta <- read_csv(paste0(csv_file)) 
  cat("Process extracted data\n")
  manta <- manta |> 
    mutate(P_CODE = ifelse(P_CODE=="IN", "MMP", "LTMP"),
           SURVEY_DATE = lubridate::ymd_hms(SURVEY_DATE), 
           LATITUDE = round(LATITUDE, 6),
           LONGITUDE = round(LONGITUDE, 6))
  cat("Save processed data\n")
  rds_file <- gsub(".csv", ".rds", csv_file)
  print(rds_file)
  ## saveRDS(pt, file = paste0(csv_path, '/pt.rds'))
  saveRDS(manta, file = rds_file)
  cat("The following is the first 6 rows of the saved data\n")
  options(width = 200)
  print(head(as.data.frame(manta)))
  cat("\nAnd a glimpse of the saved data\n")
  glimpse(manta)
  options(width = 80)
  cat("Done!\n")
}

if (data_type == "manta" & purpose == "make_reefs") {
  cat("Start preparing reefs for modelling\n=============================================\n")
  if (1 == 1) {
    cat("Read in extracted data\n")
    manta <- readRDS(file = paste0(rds_file))
    cat("Nest data\n")
    manta_reefs <- manta |>
      group_by(AIMS_REEF_NAME) |>
      nest() |>
      mutate(data = map2(.x = data, .y = AIMS_REEF_NAME,
                         .f = ~ .x |>
                           mutate(AIMS_REEF_NAME = .y) |> 
                           mutate(LC_MID =  1) |> ## this is a fake replacement
                           dplyr::select(P_CODE,
                                         ID,
                                         CRUISE_CODE,
                                         REEF_NAME,
                                         AIMS_REEF_NAME,
                                         SECTOR = A_SECTOR,
                                         LATITUDE, LONGITUDE,
                                         ## SITE_NO, TRANSECT_NO,
                                         ## SITE_DEPTH, REEF_ZONE,
                                         REPORT_YEAR,
                                         SURVEY_DATE,
                                         TOW_SEQ_NO,
                                         LIVE_CORAL,
                                         LC_MID,
                                         SHELF#,
                                         ## NRM_REGION
                                         )
                         )) |> 
      mutate(path = map(.x = AIMS_REEF_NAME,
                        .f =  ~ {
                          paste0(rds_path, "/manta",
                                 "/2021-01-14/process/ALL/2024/ALL/reef/",
                                 .x,
                                 "/raw/")                       
                        }
                        )) |> 
      mutate(system = map2(.x = AIMS_REEF_NAME, .y = path,
                           .f =  ~ {
                             .y <- gsub("\\.\\./\\.\\.", "", .y)
                             paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=manta --domain="', .x, '" --scale=reef --status=true --refresh_data=false')
                           }
                           ))
    saveRDS(manta_reefs, file = paste0(rds_path, '/manta_reefs.rds'))
  }
  cat("Nest data\n")
  manta_reefs <- readRDS(file = paste0(rds_path, '/manta_reefs.rds'))
  print(manta_reefs)
  print(manta_reefs$path[[1]])
  ## print(manta_reefs$AIMS_REEF_NAME |> unique())
  pwalk(.l = list(manta_reefs$data, manta_reefs$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          ## system(paste0("chmod 777 '", .y, "reef_data.csv'"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
          ## system(paste0("chmod 777 '", .y, "reef_data.zip'"))
          print(.y)
          write(.y, file = make_log, append = TRUE)
          ## write(dir.exists(.y), file = make_log, append = TRUE)
        })
  cat("Done!\n")
}

if (data_type == "manta" & purpose == "make_nrm") {
  cat("Start preparing NRMs for modelling\n=============================================\n")
  if (1 == 1) {
    cat("Read in extracted data\n")
    manta <- readRDS(file = paste0(rds_file))
    cat("Nest data\n")
    print(colnames(manta))
    manta_nrm <- manta |>
      group_by(NRM_REGION) |>
      nest() |>
      mutate(data = map2(.x = data, .y = NRM_REGION,
                         .f = ~ .x |>
                           ## mutate(NRM_REGION = .y) |> 
                           mutate(LC_MID =  1) |> ## this is a fake replacement
                           dplyr::select(P_CODE,
                                         ID,
                                         CRUISE_CODE,
                                         REEF_NAME,
                                         AIMS_REEF_NAME,
                                         SECTOR = A_SECTOR,
                                         ## NRM_REGION,
                                         LATITUDE,
                                         LONGITUDE,
                                         REPORT_YEAR,
                                         SURVEY_DATE,
                                         TOW_SEQ_NO,
                                         LIVE_CORAL,
                                         LC_MID,
                                         SHELF)
                         ))  |> 
      mutate(path = map(.x = NRM_REGION,
                        .f =  ~ {
                          paste0(rds_path, "/manta",
                                 "/2021-01-14/process/ALL/2024/", .x, "/nrm/",
                                 .x,
                                 "/raw/")                       
                        }
                        ))  |> 
      mutate(system = map2(.x = NRM_REGION, .y = path,
                           .f =  ~ {
                             .y <- gsub("\\.\\./\\.\\.", "", .y)
                             paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=manta --domain="', .x, '" --scale=nrm --status=true --refresh_data=false')
                           }
                           ))
    cat("Save nested data\n")
    saveRDS(manta_nrm, file = paste0(rds_path, '/manta_nrm.rds'))
  }
  cat("Write individual csv and zips\n")
  manta_nrm <- readRDS(file = paste0(rds_path, '/manta_nrm.rds'))
  ## print(manta_reefs$AIMS_REEF_NAME |> unique())
  pwalk(.l = list(manta_nrm$data, manta_nrm$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          ## system(paste0("chmod 777 '", .y, "reef_data.csv'"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
          ## system(paste0("chmod 777 '", .y, "reef_data.zip'"))
          print(.y)
          write(.y, file = make_log, append = TRUE)
          ## write(dir.exists(.y), file = make_log, append = TRUE)
        })
  cat("Done!\n")
}

if (data_type == "manta" & purpose == "make_sectors") {
  cat("Start preparing Sectors for modelling\n=============================================\n")
  if (1 == 1) {
    cat("Read in extracted data\n")
    manta <- readRDS(file = paste0(rds_file))
    cat("Nest data\n")
    ## print(colnames(manta))
    manta_sector <- manta |>
      dplyr::rename(SECTOR = A_SECTOR) |> 
      group_by(SECTOR) |>
      nest() |>
      mutate(data = map2(.x = data, .y = SECTOR,
                         .f = ~ .x |>
                           mutate(SECTOR = .y) |> 
                           mutate(LC_MID =  1) |> ## this is a fake replacement
                           dplyr::select(P_CODE,
                                         ID,
                                         CRUISE_CODE,
                                         REEF_NAME,
                                         AIMS_REEF_NAME,
                                         ## NRM_REGION,
                                         SECTOR, ## = A_SECTOR,
                                         LATITUDE, LONGITUDE,
                                         ## SITE_NO, TRANSECT_NO,
                                         ## SITE_DEPTH, REEF_ZONE,
                                         REPORT_YEAR,
                                         SURVEY_DATE,
                                         TOW_SEQ_NO,
                                         LIVE_CORAL,
                                         LC_MID,
                                         SHELF)
                         )) |>
      mutate(path = map(.x = SECTOR,
                        .f =  ~ {
                          paste0(rds_path, "/manta",
                                 "/2021-01-14/process/ALL/2024/", .x, "/Sectors/",
                                 .x,
                                 "/raw/")                       
                        }
                        ))  |> 
      mutate(system = map2(.x = SECTOR, .y = path,
                           .f =  ~ {
                             .y <- gsub("\\.\\./\\.\\.", "", .y)
                             paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=manta --domain="', .x, '" --scale=Sectors --status=true --refresh_data=false')
                           }
                           ))
    cat("Save nested data\n")
    saveRDS(manta_sector, file = paste0(rds_path, '/manta_sector.rds'))
  }
  cat("Write individual csv and zips\n")
  manta_sector <- readRDS(file = paste0(rds_path, '/manta_sector.rds'))
  ## print(pt_reefs$AIMS_REEF_NAME |> unique())
  pwalk(.l = list(manta_sector$data, manta_sector$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          ## system(paste0("chmod 777 '", .y, "reef_data.csv'"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
          ## system(paste0("chmod 777 '", .y, "reef_data.zip'"))
          print(.y)
          write(.y, file = make_log, append = TRUE)
          ## write(dir.exists(.y), file = make_log, append = TRUE)
        })
  cat("Done!\n")
}

## fish

if (data_type == "fish" & purpose == "post-process") {
  cat("Start post-processing of fish data\n=============================================\n")
  cat("Read in extracted data\n")
  fish <- read_csv(paste0(csv_file)) 
  cat("Process extracted data\n")
  fish <- fish |> 
    mutate(P_CODE = ifelse(P_CODE=="IN", "MMP", "LTMP"),
           SURVEY_DATE = lubridate::ymd_hms(SURVEY_DATE), 
           LATITUDE = round(LATITUDE, 6),
           LONGITUDE = round(LONGITUDE, 6)) |>
    dplyr::rename(SECTOR = A_SECTOR) |>
    dplyr::select(P_CODE, CRUISE_CODE, REEF_NAME, AIMS_REEF_NAME,
                  REEF_ZONE, RAP_REEF_PAIR, RAP_OPEN_CLOSED,
                  SECTOR, LATITUDE, LONGITUDE, SITE_NO, TRANSECT_NO,
                  REPORT_YEAR = YEAR, SURVEY_DATE, FAMILY, FISH_CODE, LENGTH,
                  ABUNDANCE, GENUS, SHELF, NRM_REGION)

  cat("Process extracted fish codes\n")
  fish_codes <- read_csv(gsub("fish", "fish_codes", csv_file)) |>
    dplyr::select(FISH_CODE,
                  FAMILY,
                  ## TROPHIC_CODE,
                  ## TROPHIC_GUILD_2021,
                  GENUS = REALGENUS) |>
    filter(str_detect(FISH_CODE, "[A-Z]{3}.[A-Z]{3,4}"))
  ## The AWS data have filled in the zeros.  This makes the data much larger
  ## yet if it is necessary to include the zeros data, then do the following..
  ## fish <- fish |>
  ##   group_by(across(-c("FISH_CODE", "LENGTH", "ABUNDANCE", "FAMILY", "GENUS"))) |>
  ##   nest() |>
  ##   mutate(data = map(.x = data,
  ##                     .f = ~
  ##                       .x |>
  ##                       full_join(fish_codes, by = c("FISH_CODE", "FAMILY", "GENUS")) |>
  ##                       mutate(ABUNDANCE = replace_na(ABUNDANCE, replace = 0)))) |>
  ##   unnest(data)
  cat("Save processed data\n")
  rds_file <- gsub(".csv", ".rds", csv_file)
  print(rds_file)
  ## saveRDS(pt, file = paste0(csv_path, '/pt.rds'))
  saveRDS(fish, file = rds_file)
  cat("The following is the first 6 rows of the saved data\n")
  options(width = 200)
  print(head(as.data.frame(fish)))
  cat("\nAnd a glimpse of the saved data\n")
  glimpse(fish)
  options(width = 80)
  cat("Done!\n")
}

if (data_type == "fish" & purpose == "make_reefs") {
  cat("Start preparing reefs for modelling\n=============================================\n")
  if (1 == 1) {
    cat("Read in extracted data\n")
    fish <- readRDS(file = paste0(rds_file))
    fish_codes <- read_csv(gsub("fish.rds", "fish_codes.csv", rds_file))
    cat("Nest data\n")
    fish_reefs <- fish |>
      group_by(AIMS_REEF_NAME) |>
      nest() |>
      mutate(data = map2(.x = data, .y = AIMS_REEF_NAME,
                         .f = ~ .x |>
                           mutate(AIMS_REEF_NAME = .y) |> 
                           ## fill gaps - this is very slow - is it necessary? ==============
                           ## group_by(across(-c("FISH_CODE", "LENGTH",
                           ##                    "ABUNDANCE", "FAMILY", "GENUS"))) |>
                           ## nest() |>
                           ## mutate(data = map(.x = data,
                           ##                   .f = ~
                           ##                     .x |>
                           ##                     full_join(fish_codes,
                           ##                               by = c("FISH_CODE", "FAMILY", "GENUS")) |>
                           ##                     mutate(ABUNDANCE = replace_na(ABUNDANCE, replace = 0)))) |>
                           ## unnest(data) |> 
                           ## ungroup() |> 
                           ## finish fill gaps =============================================
                           dplyr::select(P_CODE,
                                         CRUISE_CODE,
                                         REEF_NAME,
                                         AIMS_REEF_NAME,
                                         REEF_ZONE,
                                         RAP_REEF_PAIR,
                                         RAP_OPEN_CLOSED,
                                         SECTOR,
                                         LATITUDE, LONGITUDE,
                                         SITE_NO, TRANSECT_NO,
                                         REPORT_YEAR,
                                         SURVEY_DATE,
                                         FAMILY, FISH_CODE,
                                         LENGTH, ABUNDANCE,
                                         GENUS,
                                         SHELF#,
                                         ## NRM_REGION
                                         )
                         )) |> 
      mutate(path = map(.x = AIMS_REEF_NAME,
                        .f =  ~ {
                          paste0(rds_path, "/fish",
                                 "/2021-01-14/process/ALL/2024/ALL/reef/",
                                 .x,
                                 "/raw/")                       
                        }
                        )) |> 
      mutate(system = map2(.x = AIMS_REEF_NAME, .y = path,
                           .f =  ~ {
                             .y <- gsub("\\.\\./\\.\\.", "", .y)
                             paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=fish --domain="', .x, '" --scale=reef --status=true --refresh_data=false')
                           }
                           ))
    saveRDS(fish_reefs, file = paste0(rds_path, '/fish_reefs.rds'))
  }
  cat("Nest data\n")
  fish_reefs <- readRDS(file = paste0(rds_path, '/fish_reefs.rds'))
  print(fish_reefs)
  print(fish_reefs$path[[1]])
  ## print(fish_reefs$AIMS_REEF_NAME |> unique())
  pwalk(.l = list(fish_reefs$data, fish_reefs$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          ## system(paste0("chmod 777 '", .y, "reef_data.csv'"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
          ## system(paste0("chmod 777 '", .y, "reef_data.zip'"))
          print(.y)
          write(.y, file = make_log, append = TRUE)
          ## write(dir.exists(.y), file = make_log, append = TRUE)
        })
  cat("Done!\n")
}

if (data_type == "fish" & purpose == "make_nrm") {
  cat("Start preparing NRMs for modelling\n=============================================\n")
  if (1 == 1) {
    cat("Read in extracted data\n")
    fish <- readRDS(file = paste0(rds_file))
    fish_codes <- read_csv(gsub("fish.rds", "fish_codes.csv", rds_file))
    cat("Nest data\n")
    print(colnames(fish))
    fish_nrm <- fish |>
      group_by(NRM_REGION) |>
      nest() |>
      mutate(data = map2(.x = data, .y = NRM_REGION,
                         .f = ~ .x |>
                           ## mutate(NRM_REGION = .y) |> 
                           dplyr::select(P_CODE,
                                         CRUISE_CODE,
                                         REEF_NAME,
                                         AIMS_REEF_NAME,
                                         REEF_ZONE,
                                         RAP_REEF_PAIR,
                                         RAP_OPEN_CLOSED,
                                         SECTOR,
                                         LATITUDE,
                                         LONGITUDE,
                                         SITE_NO, TRANSECT_NO,
                                         REPORT_YEAR,
                                         SURVEY_DATE,
                                         FAMILY, FISH_CODE,
                                         LENGTH, ABUNDANCE,
                                         GENUS,
                                         SHELF)
                         ))  |> 
      mutate(path = map(.x = NRM_REGION,
                        .f =  ~ {
                          paste0(rds_path, "/fish",
                                 "/2021-01-14/process/ALL/2024/", .x, "/nrm/",
                                 .x,
                                 "/raw/")                       
                        }
                        ))  |> 
      mutate(system = map2(.x = NRM_REGION, .y = path,
                           .f =  ~ {
                             .y <- gsub("\\.\\./\\.\\.", "", .y)
                             paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=fish --domain="', .x, '" --scale=nrm --status=true --refresh_data=false')
                           }
                           ))
    cat("Save nested data\n")
    saveRDS(fish_nrm, file = paste0(rds_path, '/fish_nrm.rds'))
  }
  cat("Write individual csv and zips\n")
  fish_nrm <- readRDS(file = paste0(rds_path, '/fish_nrm.rds'))
  ## print(fish_reefs$AIMS_REEF_NAME |> unique())
  pwalk(.l = list(fish_nrm$data, fish_nrm$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          ## system(paste0("chmod 777 '", .y, "reef_data.csv'"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
          ## system(paste0("chmod 777 '", .y, "reef_data.zip'"))
          print(.y)
          write(.y, file = make_log, append = TRUE)
          ## write(dir.exists(.y), file = make_log, append = TRUE)
        })
  cat("Done!\n")
}

if (data_type == "fish" & purpose == "make_sectors") {
  cat("Start preparing Sectors for modelling\n=============================================\n")
  if (1 == 1) {
    cat("Read in extracted data\n")
    fish <- readRDS(file = paste0(rds_file))
    cat("Nest data\n")
    ## print(colnames(fish))
    fish_sector <- fish |>
      ## dplyr::rename(SECTOR = A_SECTOR) |> 
      group_by(SECTOR) |>
      nest() |>
      mutate(data = map2(.x = data, .y = SECTOR,
                         .f = ~ .x |>
                           mutate(SECTOR = .y) |> 
                           dplyr::select(P_CODE,
                                         CRUISE_CODE,
                                         REEF_NAME,
                                         AIMS_REEF_NAME,
                                         REEF_ZONE,
                                         RAP_REEF_PAIR,
                                         RAP_OPEN_CLOSED,
                                         SECTOR, 
                                         LATITUDE, LONGITUDE,
                                         SITE_NO, TRANSECT_NO,
                                         REPORT_YEAR,
                                         SURVEY_DATE,
                                         FAMILY, FISH_CODE,
                                         LENGTH, ABUNDANCE,
                                         GENUS,
                                         SHELF)
                         )) |>
      mutate(path = map(.x = SECTOR,
                        .f =  ~ {
                          paste0(rds_path, "/fish",
                                 "/2021-01-14/process/ALL/2024/", .x, "/Sectors/",
                                 .x,
                                 "/raw/")                       
                        }
                        ))  |> 
      mutate(system = map2(.x = SECTOR, .y = path,
                           .f =  ~ {
                             .y <- gsub("\\.\\./\\.\\.", "", .y)
                             paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=fish --domain="', .x, '" --scale=Sectors --status=true --refresh_data=false')
                           }
                           ))
    cat("Save nested data\n")
    saveRDS(fish_sector, file = paste0(rds_path, '/fish_sector.rds'))
  }
  cat("Write individual csv and zips\n")
  fish_sector <- readRDS(file = paste0(rds_path, '/fish_sector.rds'))
  ## print(pt_reefs$AIMS_REEF_NAME |> unique())
  pwalk(.l = list(fish_sector$data, fish_sector$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          ## system(paste0("chmod 777 '", .y, "reef_data.csv'"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
          ## system(paste0("chmod 777 '", .y, "reef_data.zip'"))
          print(.y)
          write(.y, file = make_log, append = TRUE)
          ## write(dir.exists(.y), file = make_log, append = TRUE)
        })
  cat("Done!\n")
}
## restore umask
system("umask 0022")

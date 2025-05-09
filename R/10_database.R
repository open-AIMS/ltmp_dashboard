library(tidyverse)

args <- commandArgs()

args = c(
  "R",
  "Rscript",
  "--file=pt_cover.R",                            ##the name of the target script
  "--path=../../data",
  "--method=photo-transect"
)

arg <- args[grep("--path=.*", args)]
DATA_PATH <- gsub('--path=(.*)', '\\1/', arg)
assign("DATA_PATH", DATA_PATH, envir = .GlobalEnv)

arg <- args[grep("--method=.*", args)]
DATA_TYPE <- gsub('--method=(.*)', '\\1', arg)
assign("DATA_TYPE", DATA_TYPE, envir = .GlobalEnv)

## Photo-transect data 
if (DATA_TYPE == "photo-transect") {
  
  writeLines("select s.sample_id as survey_id, b.vpoint_sid,
s.p_code, s.cruise_code, s.reef_name, s.aims_reef_name,
s.MMP_SITE_NAME, reef_zone, s.a_sector, s.shelf,
s.site_lat as latitude, s.site_long as longitude, s.site_no,
s.transect_no, s.site_depth, s.report_year as year,
s.sample_date as survey_date, b.frame, b.point_no, c.group_desc,
c.benthos_code as benthos, c.family_2021 as family,
c.genus_2021 as genus, c.spec_code as species,
c.reef_page as reefpage_category, b.video_code as label,
mp_zone_site as mp_zone, nrm_region_from_gbrmpa_shape as nrm_region,
water_body
from v_rm_sample s, vpoint_codes c, rm_vpoint b
            where s.sample_id = b.sample_id
            and b.video_code=c.video_code
             and p_code in ('IN', 'RM', 'RMRAP','RAP')
             and visit_no is not null 
             and b.video_code is not null
            and time_sec >0
            and group_code not like 'IN'
            and transect_no in (1,2,3,4,5)
            ",
paste0(DATA_PATH, "photo-transect.sql"))

  system(paste0("java -jar ../dbExport.jar ",
                DATA_PATH, "photo-transect.sql ",
                DATA_PATH, "photo-transect.csv ",
                "reef reefmon")) 

  pt <- read_csv(paste0(DATA_PATH, 'photo-transect.csv')) 
  pt <- pt |> 
    mutate(P_CODE = ifelse(P_CODE=="IN", "MMP", "LTMP"),
           ## REEF_ZONE = ifelse(is.null(REEF_ZONE), "_", REEF_ZONE),
           REEF_ZONE = ifelse(is.na(REEF_ZONE), "_", REEF_ZONE),
           LATITUDE = round(LATITUDE, 6),
           LONGITUDE = round(LONGITUDE, 6),
           SITE_DEPTH = ifelse(P_CODE == 'MMP', SITE_DEPTH, 9))

  saveRDS(pt, file = paste0(DATA_PATH, 'photo-transect.rds'))

  ## make reefs
{
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
                                       REPORT_YEAR = YEAR,
                                       SURVEY_DATE, FRAME,
                                       POINT_NO, FAMILY,
                                       GROUP_DESC, REEFPAGE_CATEGORY,
                                       SHELF)
                       )) |> 
    mutate(path = map(.x = AIMS_REEF_NAME,
                      .f =  ~ {
                        paste0(DATA_PATH, "photo-transect",
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
  saveRDS(pt_reefs, file = paste0(DATA_PATH, 'photo-transect_reefs.rds'))
  pwalk(.l = list(pt_reefs$data, pt_reefs$path),
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-r9X")
        })
  ## Trigger the analyses (run the backend)
  walk(pt_reefs$system[[10]],
        .f =  ~ {
          system(.x)
        })
}
  ## make nrm
{
  pt_nrm <- pt |>
    group_by(NRM_REGION) |>
    nest() |>
    mutate(data = map2(.x = data, .y = NRM_REGION,
                       .f = ~ .x |>
                         ## mutate(NRM_REGION = .y) |>
                         dplyr::select(P_CODE, ID = VPOINT_SID,
                                       CRUISE_CODE, REEF_NAME,
                                       AIMS_REEF_NAME,
                                       SECTOR = A_SECTOR,
                                       LATITUDE, LONGITUDE,
                                       SITE_NO, TRANSECT_NO,
                                       SITE_DEPTH, REEF_ZONE,
                                       REPORT_YEAR = YEAR,
                                       SURVEY_DATE, FRAME,
                                       POINT_NO, FAMILY,
                                       GROUP_DESC, REEFPAGE_CATEGORY,
                                       SHELF)
                       )) |> 
    mutate(path = map(.x = NRM_REGION,
                      .f =  ~ {
                        paste0(DATA_PATH, "photo-transect",
                               "/2021-01-14/process/ALL/2024/",
                               .x,
                               "/nrm/",
                               .x,
                               "/raw/")                       
                      }
                      )) |>
    mutate(system = map2(.x = NRM_REGION, .y = path,
                         .f =  ~ {
                           .y <- gsub("\\.\\./\\.\\.", "", .y)
                           paste0('docker run -it --rm -v ~/dev:/home/Project -v ~/data:/data ltmp-monitoring-model:latest Rscript home/Project/R/00_main.R --path="', .y, 'reef_data.zip"  --method=photo-transect --domain="', .x, '" --scale=nrm --status=true --refresh_data=false')
                           }
                         ))
  
  saveRDS(pt_nrm, file = paste0(DATA_PATH, 'photo-transect_nrm.rds'))
  ## Save the data in preparation for analyses
  walk2(.x = pt_nrm$data, .y = pt_nrm$path,
        .f =  ~ {
          if (!dir.exists(.y)) dir.create(.y, recursive = TRUE)
          write_csv(.x, file = paste0(.y, "reef_data.csv"))
          zip(zipfile = paste0(.y, "reef_data.zip"),
              files = paste0(.y, "reef_data.csv"),
              flags = "-rFS9X")
        })
  ## Trigger the analyses (run the backend)
  walk(pt_nrm$system,
        .f =  ~ {
          system(.x)
        })
}

}


## Manta tow data
writeLines("
SELECT V_RM_SAMPLE.P_CODE, V_RM_SAMPLE.A_SECTOR, V_RM_SAMPLE.SHELF, V_RM_SAMPLE.REEF_NAME,V_RM_SAMPLE.REEF_ID, V_RM_SAMPLE.REEF_LONG,
  V_RM_SAMPLE.REEF_LAT, V_RM_SAMPLE.REPORT_YEAR, RM_MANTA.TOW_SEQ_NO, RM_MANTA.LIVE_CORAL, V_RM_SAMPLE.SAMPLE_CLASS
FROM RM_MANTA INNER JOIN V_RM_SAMPLE ON RM_MANTA.SAMPLE_ID = V_RM_SAMPLE.SAMPLE_ID
WHERE (((V_RM_SAMPLE.SAMPLE_CLASS) In ('K','C','G','Z') Or (V_RM_SAMPLE.SAMPLE_CLASS) Is Null))
ORDER BY V_RM_SAMPLE.P_CODE, V_RM_SAMPLE.A_SECTOR, V_RM_SAMPLE.SHELF, V_RM_SAMPLE.REEF_NAME, V_RM_SAMPLE.REEF_ID, V_RM_SAMPLE.REEF_LAT,
  V_RM_SAMPLE.REPORT_YEAR, RM_MANTA.TOW_SEQ_NO","manta.sql")
## ----

system("java -jar dbExport.jar manta.sql manta.csv reef reefmon") 

manta <- read_csv(paste0(DATA_PATH, 'manta.csv'))  
manta <- read.csv(paste0(DATA_PATH, 'manta.csv'))  

## Photo-transect specific functions
ltmp_load_data_pt <- function() {
  status::status_try_catch(
  {
    load(file=paste0(DATA_PATH, 'primary/', RDATA_FILE))
    data=data %>% dplyr::rename(REEF=AIMS_REEF_NAME) %>%
      filter(GROUP_DESC != "INDETERMINATE") %>%
      droplevels() |>
      suppressMessages() |>
      suppressWarnings()
  },
  stage_ = 3,
  order_ = 1,
  name_ = "Load photo-transect data",
  item_ = "load_data_pt"
  )
  return(data)  
}

ltmp_fill_gaps_pt <- function(dat) {
  status::status_try_catch(
  {
    data.1 <-
      dat |>
      unite(Groups, c(GROUP_DESC, REEFPAGE_CATEGORY), 
            sep = "_", remove = TRUE) |>
      group_by(P_CODE,  SECTOR, SHELF, REEF_NAME, REEF, REEF_ZONE, 
               SITE_DEPTH, LATITUDE, LONGITUDE, SITE_NO, TRANSECT_NO, 
               REPORT_YEAR, SURVEY_DATE, Groups)  |>
      suppressMessages() |>
      suppressWarnings()

    data.wide <- 
      data.1 |>
      summarise(points=n()) |>
      pivot_wider(names_from=Groups, 
                  values_from = points, 
                  values_fill=0) |>
      suppressMessages() |>
      suppressWarnings()

    data.filled <- 
      data.wide |>
      pivot_longer(!c(P_CODE,  SECTOR, SHELF, REEF_NAME, REEF, REEF_ZONE, 
                      SITE_DEPTH, LATITUDE, LONGITUDE, SITE_NO, TRANSECT_NO, 
                      REPORT_YEAR, SURVEY_DATE), 
                   names_to="Groups", values_to="Points") |>
      separate(Groups, into = c("GROUP_DESC", "REEFPAGE_CATEGORY"), 
               sep = "_") |> 
      ungroup() |>
      suppressMessages() |>
      suppressWarnings()
  },
  stage_ = 3,
  order_ = 2,
  name_ = "Fill in zeros",
  item_ = "fill_zeros_pt"
  )
  return(data.filled)
}

ltmp_spatial_domain <- function(data) {
  status::status_try_catch(
  {
    data.shelf <- data |> dplyr::select(REEF, SHELF) |> distinct()
    data.spatial <-
      data |>
      group_by(REEF) |>
      summarise(Longitude=mean(LONGITUDE,na.rm=TRUE),
                Latitude=mean(LATITUDE, na.rm=TRUE)) |>
      ungroup() |>
      ltmp_assign_spatial_domain_zones() |>
      ltmp_assign_spatial_domain_nrm() |>
      ltmp_assign_spatial_domain_bioregions() |>
      ltmp_assign_spatial_domain_sectors() |>
      ltmp_assign_spatial_domain_shelf() |>
      mutate(Shelf = ifelse(Shelf %in% c('Enclosed Coastal','Open Coastal'),
                            'Inshore', 'Offshore')) |>
      ltmp_assign_spatial_domain_shelf_from_database(data.shelf) |>  #this overwrites the previous
      suppressMessages() |>
      suppressWarnings()
    save(data.spatial,
         file=paste0(DATA_PATH, 'processed/',
                     gsub('\\.RData', '.spatial.RData', RDATA_FILE)))
  },
  stage_ = 3,
  order_ = 3,
  name_ = "Generate spatial domains",
  item_ = "spatial_domains"
  )
  return(data.spatial)
}

######################################################################################################
## The following function determines which De'ath Zone a reef is in based on Latitude and Longitude ##
## The process involves locating the reef (based on long/lat) within one of the polygons defined    ##
## within ../data/spatial/gbr_3Zone.RData (an sf object)                                            ##
## Arguments:                                                                                       ##
##    - dat:    a dataframe containing at least the following:                                      ##
##              - REEF:  the name of Reefs                                                          ##
##              - Longitude:  the Reef longitude                                                    ##
##              - Latitude:   the Reef latitude                                                     ##
## Returns:                                                                                         ##
##    - a dataframe containing Reef-level Zone classifications (Region)                             ##
######################################################################################################
ltmp_assign_spatial_domain_zones <- function(dat) {
  load('../data/spatial/gbr_3Zone.RData')
  ## Only need to work on Reef-level average lat/longs
  dat.sum <-
    dat |>
    group_by(REEF) |>
    summarise(Longitude=mean(Longitude,na.rm=TRUE),
              Latitude=mean(Latitude, na.rm=TRUE)) |>
    ungroup() |>
    suppressMessages() |> suppressWarnings()

  dat.sum |>
    st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(gbr_3Zone), remove = FALSE) |>
    st_join(gbr_3Zone |> dplyr::select(Region=Region)) |>
    st_drop_geometry() |>
    right_join(dat |> dplyr::select(-Longitude, -Latitude)) |>
    suppressMessages() |>
    suppressWarnings()
}

#####################################################################################################
## The following function determines which NRM region a reef is in based on Latitude and Longitude ##
## The process involves locating the reef (based on long/lat) within one of the polygons defined   ##
## within ../data/spatial/nrm.RData (an sf object)                                                 ##
## Arguments:                                                                                      ##
##    - dat:    a dataframe containing at least the following:                                     ##
##              - REEF:  the name of Reefs                                                         ##
##              - Longitude:  the Reef longitude                                                   ##
##              - Latitude:   the Reef latitude                                                    ##
## Returns:                                                                                        ##
##    - a dataframe containing Reef-level NRM region classifications (NRM_region)                  ##
#####################################################################################################
ltmp_assign_spatial_domain_nrm <- function(dat) {
  load('../data/spatial/nrm.RData')
  ## Only need to work on Reef-level average lat/longs
  dat.sum =
    dat |>
    group_by(REEF) |>
    summarise(Longitude=mean(Longitude,na.rm=TRUE),
              Latitude=mean(Latitude, na.rm=TRUE)) |>
    ungroup() |>
    suppressMessages() |> suppressWarnings()

  dat.sum |>
    st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(nrm), remove = FALSE) |>
    st_join(nrm |>  dplyr::select(NRM_region=NAME)) |>
    st_drop_geometry() |>
    right_join(dat |> dplyr::select(-Longitude, -Latitude)) |>
    suppressMessages() |>
    suppressWarnings()
}

####################################################################################################
## The following function determines which Bioregion a reef is in based on Latitude and Longitude ##
## The process involves locating the reef (based on long/lat) within one of the polygons defined  ##
## within ../data/spatial/bioregions.RData (an sf object)                                         ##
## Arguments:                                                                                     ##
##    - dat:    a dataframe containing at least the following:                                    ##
##              - REEF:  the name of Reefs                                                        ##
##              - Longitude:  the Reef longitude                                                  ##
##              - Latitude:   the Reef latitude                                                   ##
## Returns:                                                                                       ##
##    - a dataframe containing Reef-level Bioregion classifications (Bioregion)                   ##
####################################################################################################
ltmp_assign_spatial_domain_bioregions <- function(dat) {
  load('../data/spatial/bioregions.RData')
  ## Only need to work on Reef-level average lat/longs
  dat.sum <- 
    dat |>
    group_by(REEF) |>
    summarise(Longitude=mean(Longitude,na.rm=TRUE),
              Latitude=mean(Latitude, na.rm=TRUE)) |>
    ungroup() |>
    suppressMessages() |> suppressWarnings()

  dat.sum |>
    st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(bioregions), remove = FALSE) |>
    st_join(bioregions |>  dplyr::select(Bioregion=BIO)) |>
    st_drop_geometry() |>
    right_join(dat |> dplyr::select(-Longitude, -Latitude)) |>
    suppressMessages() |>
    suppressWarnings()
}


####################################################################################################
## The following function determines which Sector a reef is in based on Latitude and Longitude    ##
## The process involves locating the reef (based on long/lat) within one of the polygons defined  ##
## within ../data/spatial/sectors.RData (an sf object)                                            ##
## Arguments:                                                                                     ##
##    - dat:    a dataframe containing at least the following:                                    ##
##              - REEF:  the name of Reefs                                                        ##
##              - Longitude:  the Reef longitude                                                  ##
##              - Latitude:   the Reef latitude                                                   ##
## Returns:                                                                                       ##
##    - a dataframe containing Reef-level Sector classifications (Sector)                         ##
####################################################################################################
ltmp_assign_spatial_domain_sectors <- function(dat) {
  load('../data/spatial/sectors.RData')
  ## Only need to work on Reef-level average lat/longs
  dat.sum <- 
    dat |>
    group_by(REEF) |>
    summarise(Longitude=mean(Longitude,na.rm=TRUE),
              Latitude=mean(Latitude, na.rm=TRUE)) |>
    ungroup() |>
    suppressMessages() |> suppressWarnings()

  dat.sum |>
    st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(sectors), remove = FALSE) |>
    st_join(sectors |>  dplyr::select(Sector=SECTOR)) |>
    st_drop_geometry() |>
    right_join(dat |> dplyr::select(-Longitude, -Latitude)) |>
    suppressMessages() |>
    suppressWarnings()
}


####################################################################################################
## The following function determines which Shelf a reef is in based on Latitude and Longitude     ##
## The process involves locating the reef (based on long/lat) within one of the polygons defined  ##
## within ../data/spatial/waterbodies.RData (an sf object)                                        ##
## Arguments:                                                                                     ##
##    - dat:    a dataframe containing at least the following:                                    ##
##              - REEF:  the name of Reefs                                                        ##
##              - Longitude:  the Reef longitude                                                  ##
##              - Latitude:   the Reef latitude                                                   ##
## Returns:                                                                                       ##
##    - a dataframe containing Reef-level Shelf (inshore, midshelf or outer)                      ##
####################################################################################################
ltmp_assign_spatial_domain_shelf <- function(dat) {
  load('../data/spatial/waterbodies.RData')
  ## Only need to work on Reef-level average lat/longs
  dat.sum =
    dat |>
    group_by(REEF) |>
    summarise(Longitude=mean(Longitude,na.rm=TRUE),
              Latitude=mean(Latitude, na.rm=TRUE)) |>
    ungroup() |>
    suppressMessages() |> suppressWarnings()

  dat.sum |>
    st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(waterbodies), remove = FALSE) |>
    st_join(waterbodies |>  dplyr::select(Shelf=WATERBODY)) |>
    st_drop_geometry() |>
    right_join(dat |> dplyr::select(-Longitude, -Latitude)) |>
    suppressMessages() |>
    suppressWarnings()
}

ltmp_assign_spatial_domain_shelf_from_database <- function(data, data.shelf) {
  data |>
    full_join(data.shelf) |>
    mutate(Shelf = factor(ifelse(SHELF == "I", "Inshore", "Offshore")))
}




ltmp_split_ma <- function(data) {
  status::status_try_catch(
  {
    data.hc.sc.a <- data |>
      mutate(Set=1)
    data.mac <- data |>
      mutate(GROUP_DESC = 
               ifelse(GROUP_DESC ==  "ALGAE" & 
                      !REEFPAGE_CATEGORY %in% c("Turf algae", "Coralline algae"), 
                      "MACROALGAE", GROUP_DESC)) |> 
      mutate(Set = 2)
    data <- data.hc.sc.a |>
      bind_rows(data.mac)
  },
  stage_ = 3,
  order_ = 4,
  name_ = "Split out macroalgae",
  item_ = "split_ma_pt"
  )
  return(data)  
}

ltmp_group_lookup_pt <- function(data) {
  status::status_try_catch(
  {
  lookup <- data |>
    group_by(Set, GROUP_DESC, REEFPAGE_CATEGORY) |>
    summarise(COUNT=sum(Points)) |>
    arrange(Set, GROUP_DESC, -COUNT) |>
    ungroup() |>
    group_by(Set, GROUP_DESC) |>
    slice(1:6) |>
    ungroup() |>
    mutate(fGROUP=REEFPAGE_CATEGORY)
  },
  stage_ = 3,
  order_ = 5,
  name_ = "Reference group lookup",
  item_ = "group_lookup_pt"
  )
  return(lookup)  
}

ltmp_reduce_groups_pt <- function(data) {
  status::status_try_catch(
  {
  data <- data |>
    full_join(lookup |> dplyr::select(-COUNT)) |>
    mutate(fGROUP=ifelse(is.na(fGROUP), 'Other', fGROUP)) |>
    dplyr::select(-REEFPAGE_CATEGORY)
  ## groups <- data |> pull(fGROUP) |> unique()
  ## report_years <- data |> pull(REPORT_YEAR) |> unique
  if (status::get_setting(element = "data_scale") != 'reef')
    data <- data |> mutate(fGROUP=NA)
  },
  stage_ = 3,
  order_ = 5,
  name_ = "Reduce the groups",
  item_ = "reduce_groups_pt"
  )
  return(data)  
}

ltmp_process_points_pt <- function(data, data.spatial) {
  status::status_try_catch(
  {
    data <-
      data |>
      dplyr::select(-matches("^AIMS_REEF_NAME$|^REGION$|^NRM_REGION$|^A_SECTOR$|^FRAME$|^CRUISE_CODE$|$MMP_REGION$")) |>
      mutate(
        fYEAR = REPORT_YEAR,
        fDEPTH = SITE_DEPTH,
        across(c(Set, SITE_NO, TRANSECT_NO, fYEAR, fDEPTH),
               function(x) factor(as.character(x))),
        DATE = as.Date(SURVEY_DATE, format = '%Y-%m-%d'),
        VARIABLE = GROUP_DESC) |>
      dplyr::select(-matches("^GROUP_DESC$|^FAMILY$")) |>
      group_by(Set, REEF, SITE_NO, TRANSECT_NO, VARIABLE, REPORT_YEAR,
               fYEAR, DATE, fDEPTH, REEF_ZONE, fGROUP) |>
      summarise(COUNT = sum(Points)) |> 
      ungroup() |>
      group_by(Set, REEF, SITE_NO, TRANSECT_NO, REPORT_YEAR, fYEAR,
               DATE, fDEPTH, REEF_ZONE) |>
      mutate(TOTAL = sum(COUNT),
             PERC_COVER = COUNT/TOTAL) |>
      ungroup() |>
      left_join(data.spatial |> dplyr::select(-Latitude,-Longitude)) |>
      filter(VARIABLE %in% c('HARD CORAL','ALGAE','MACROALGAE','SOFT CORAL')) |>
      droplevels() |>
      mutate(ZONE_DEPTH = interaction(REEF_ZONE, fDEPTH),
             SITE = factor(interaction(SITE_NO,REEF_ZONE,fDEPTH))) |>
      filter(!(Set == 2 & VARIABLE != "MACROALGAE")) |>
      filter(!(Set == 1 & VARIABLE == "MACROALGAE")) |>
      suppressMessages() |>
      suppressWarnings()
  },
  stage_ = 3,
  order_ = 6,
  name_ = "Process points",
  item_ = "process_points_pt"
  )
  return(data)  
  
}

ltmp_save_data_pt <- function(data) {
  status::status_try_catch(
  {
  save(data,  file = paste0(DATA_PATH, "processed/", RDATA_FILE))
  write_csv(data |> dplyr::select(-fYEAR),  file = paste0(DATA_PATH, "processed/", CSV_FILE))
  if (status::get_setting(element = "data_from") == "AWS")
    write_aws(file = CSV_FILE,  level = "processed/")
  },
  stage_ = 3,
  order_ = 7,
  name_ = "Save processed data",
  item_ = "save_data"
  )
  cat(paste0('Data successfully processed:\n'))
}

## Juvenile specific functions

ltmp_load_data_juv <- function() {
  status::status_try_catch(
  {
    load(file=paste0(DATA_PATH, 'primary/', RDATA_FILE))
    data <- data |>
      dplyr::rename(REEF=AIMS_REEF_NAME) |>
      dplyr::rename_with(recode, REEF_PAGE_CATEGORY='REEFPAGE_CATEGORY') |> # incase this has been named incorrectly
      suppressMessages() |>
      suppressWarnings()
  },
  stage_ = 3,
  order_ = 1,
  name_ = "Load juvenile data",
  item_ = "load_data_juv"
  )
  return(data)  
}

ltmp_fill_gaps_juv <- function(dat) {
  status::status_try_catch(
  {
    data.1 <-
      dat |>
      unite(Groups, c(REEFPAGE_CATEGORY), 
            sep = "_", remove = TRUE) |>
      group_by(P_CODE,  SECTOR, SHELF, REEF_NAME, REEF, REEF_ZONE, 
               SITE_DEPTH, LATITUDE, LONGITUDE, SITE_NO, AVAILABLE_SUBSTRATE, AREA_TRANSECT, 
               REPORT_YEAR, SURVEY_DATE, Groups) |> 
      suppressMessages() |>
      suppressWarnings()

    data.wide <- 
      data.1 |>
      ## summarise(points=n()) |>
      summarise(ABUNDANCE=sum(ABUNDANCE)) |>
      pivot_wider(names_from=Groups, 
                  values_from = ABUNDANCE, 
                  values_fill=0) |> 
      suppressMessages() |>
      suppressWarnings()

    data.filled <- 
      data.wide |>
      pivot_longer(!c(P_CODE,  SECTOR, SHELF, REEF_NAME, REEF, REEF_ZONE, 
                      SITE_DEPTH, LATITUDE, LONGITUDE, SITE_NO, AVAILABLE_SUBSTRATE, AREA_TRANSECT, 
                      REPORT_YEAR, SURVEY_DATE), 
                   names_to="Groups", values_to="ABUNDANCE") |>
      separate(Groups, into = c("REEFPAGE_CATEGORY"), 
               sep = "_") |> 
      ungroup() |> 
      suppressMessages() |>
      suppressWarnings()
    data.filled
  },
  stage_ = 3,
  order_ = 2,
  name_ = "Fill in zeros",
  item_ = "fill_zeros_juv"
  )
  return(data.filled)
}

ltmp_group_lookup_juv <- function(data) {
  status::status_try_catch(
  {
  lookup <- data |>
    group_by(REEFPAGE_CATEGORY) |>
    summarise(COUNT=sum(ABUNDANCE)) |>
    arrange(-COUNT) |>
    ungroup() |>
    slice(1:6) |>
    ungroup() |>
    mutate(fGROUP=REEFPAGE_CATEGORY)
  },
  stage_ = 3,
  order_ = 5,
  name_ = "Reference group lookup",
  item_ = "group_lookup_juv"
  )
  return(lookup)  
}

ltmp_reduce_groups_juv <- function(data) {
  status::status_try_catch(
  {
  data <- data |>
    full_join(lookup |> dplyr::select(-COUNT)) |>
    mutate(fGROUP=ifelse(is.na(fGROUP), 'Other', fGROUP)) |>
    dplyr::select(-REEFPAGE_CATEGORY)
  ## groups <- data |> pull(fGROUP) |> unique()
  ## report_years <- data |> pull(REPORT_YEAR) |> unique
  if (status::get_setting(element = "data_scale") != 'reef')
    data <- data |> mutate(fGROUP=NA)
  ## Ensure that there is only a single entry per fGROUP pear site/year
  data <- data |> 
    group_by(P_CODE, SECTOR, SHELF, REEF_NAME, REEF, REEF_ZONE, SITE_DEPTH,
             LATITUDE, LONGITUDE, SITE_NO, AVAILABLE_SUBSTRATE, AREA_TRANSECT, REPORT_YEAR,
             SURVEY_DATE, fGROUP) |> 
    summarise(ABUNDANCE = sum(ABUNDANCE)) |>
    ungroup()
  },
  stage_ = 3,
  order_ = 5,
  name_ = "Reduce the groups",
  item_ = "reduce_groups_juv"
  )
  return(data)  
}

ltmp_process_points_juv <- function(data, data.spatial) {
  status::status_try_catch(
  {
  data <-
    data |>
    dplyr::select(-matches("^P_CODE$|^AIMS_REEF_NAME$|^REGION$|^NRM_REGION$|^A_SECTOR$|^SECTOR$|^FRAME$|^CRUISE_CODE$|$MMP_REGION$")) |>
    mutate(
      fYEAR = REPORT_YEAR,
      fDEPTH = SITE_DEPTH,
      across(c(SITE_NO, fYEAR, fDEPTH), function(x) factor(as.character(x))),
      DATE = as.Date(SURVEY_DATE, format = '%Y-%m-%d'),
      VARIABLE = 'ABUNDANCE',
      AVAILABLE_SUBSTRATE = AVAILABLE_SUBSTRATE*AREA_TRANSECT) |>  #AVAILABLE_SUBSTRATE in the input data is a percentage
    arrange(REEF, SITE_NO, REPORT_YEAR, fGROUP) |>
    left_join(data.spatial |>
              dplyr::select(-Latitude,-Longitude)) |>
    mutate(ZONE_DEPTH = interaction(REEF_ZONE, fDEPTH),
           SITE = factor(interaction(SITE_NO, REEF_ZONE, fDEPTH))) |>
    suppressMessages() |>
    suppressWarnings()
  },
  stage_ = 3,
  order_ = 6,
  name_ = "Process points",
  item_ = "process_points_juv"
  )
  return(data)  
  
}

## Manta specific functions

ltmp_load_data_manta <- function() {
  status::status_try_catch(
  {
    load(file=paste0(DATA_PATH, 'primary/', RDATA_FILE))
    data <- data |>
      dplyr::rename(REEF=AIMS_REEF_NAME) |>
      suppressMessages() |>
      suppressWarnings()
  },
  stage_ = 3,
  order_ = 1,
  name_ = "Load manta data",
  item_ = "load_data_manta"
  )
  return(data)  
}

ltmp_process_tows_manta <- function(data, data.spatial) {
  status::status_try_catch(
  {
    data <-
      data |>
      dplyr::select(-matches("^AIMS_REEF_NAME$|^REGION$|^NRM_REGION$|^A_SECTOR$|^FRAME$|^CRUISE_CODE$|$MMP_REGION$")) |>
      ## dplyr::rename(YEAR=one_of("REPORT_YEAR","YEAR"),
      ##               SURVEY_DATE=one_of("SAMPLE_DATE","SURVEY_DATE")) |>
      filter(REPORT_YEAR > 1985) |>
      mutate(Cover = ltmp_calc_percent(LIVE_CORAL)) |>
      mutate(
        fYEAR = REPORT_YEAR,
        across(c(TOW_SEQ_NO, fYEAR), function(x) factor(as.character(x))),
        DATE = as.Date(SURVEY_DATE, format = "%Y-%m-%d")) |>
      dplyr::select(-matches("^SAMPLE_CLASS$")) |>
                                        #group_by(REEF, REPORT_YEAR, fYEAR, DATE) |>
                                        #summarise(Cover=mean(Cover, na.rm=TRUE),
                                        #          Tows=length(unique(TOW_SEQ_NO))) |>
      mutate(fTOW = factor(paste0(REEF, TOW_SEQ_NO)),
             nTows = length(unique(TOW_SEQ_NO))) |>
      ungroup() |>
      left_join(data.spatial |>
                dplyr::select(-Latitude, -Longitude)) |>
      mutate(VARIABLE = 'HC') |>
      suppressMessages() |>
      suppressWarnings()
  },
  stage_ = 3,
  order_ = 6,
  name_ = "Process tows",
  item_ = "process_tows_manta"
  )
  return(data)  
  
}

#######################################################################
## The following function converts the Manta tow coral cover classes ##
## into percent cover values in the range [0,1].                     ##
##   parameters:                                                     ##
##      x:     a character vector of coral class categories          ##
##   returns:  a numeric cover abundance                             ##
#######################################################################
ltmp_calc_percent = function(x) {
    ifelse(x == '0', 0,
    ifelse(x == '1', 0.05,
    ifelse(x == '1L', 0.025,
    ifelse(x == '1U', 0.075,
    ifelse(x == '2', 0.2,
    ifelse(x == '2L', 0.15,
    ifelse(x == '2U', 0.25,
    ifelse(x == '3', 0.4,
    ifelse(x == '3L', 0.35,
    ifelse(x == '3U', 0.45,
    ifelse(x == '4', 0.625,
    ifelse(x == '4L', 0.5625,
    ifelse(x == '4U', 0.6875,
    ifelse(x == '5', 0.875,
    ifelse(x == '5L',0.8125,0.9375)))))))))))))))
}

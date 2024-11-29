source("ltmp_startup_functions.R")
source("ltmp_process_functions.R")
if (ltmp_is_parent()) ltmp_start_matter(args)

status::status_set_stage(stage = 3, title = "Process data")

for (s in  str_subset(status_$status[[3]]$items, "_pt$|_juv$"))
  status::remove_status_item(stage = 3, item = s)

#######################################################################
## Load the data in preparaton for data processing.                  ##
## There is two processing steps done at this stage.                 ##
## - rename AIMS_REEF_NAME to REEF (used as the reef id throughout)  ##
## - ensure there are no "INDETERMINATE" GROUP_DESC values           ##
#######################################################################
data <- ltmp_load_data_manta()

##################################################################################
## Determine the spatial domains (Region, NRM_region, Bioregion) for each reef   ##
## - aggregate the lat/longs to reef level                                       ##
## - join to the spatial lookup to add Regions, Zones,  Bioregions etc           ##
###################################################################################
data.spatial <- ltmp_spatial_domain(data)


###################################################################################
## Processing steps                                                              ##
## - remove REGION and FRAME                                                     ##
## - ensure that REEF, SITE_NO, TRANSECT_NO and YEAR are all declared as factors ##
## - ensure that each Site and Transect are uniquely identified                  ##
## - declare Date as date version of SURVEY_DATE                                 ##
## - count the number of points of each GROUP_CODE per REEF/SITE_NO/TRANSECT_NO  ##
## - generate a total number of points per REEF/SITE_NO/TRANSECT_NO              ##
## - calculate a percent cover for each GROUP_CODE (not for analyses)            ##
###################################################################################
data <- ltmp_process_tows_manta(data, data.spatial)

#########################################################################
## Save and export the processed data                                  ##
## - ../data/processed/  (native RData format)                         ##
## - ../data/processed/  (csv format)                                  ##
## - <aws_path>/processed  (if data_from is AWS)                       ##
#########################################################################
ltmp_save_data_pt(data)

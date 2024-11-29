source("ltmp_startup_functions.R")
source("ltmp_process_functions.R")
if (ltmp_is_parent()) ltmp_start_matter(args)

status::status_set_stage(stage = 3, title = "Process data")

for (s in  str_subset(status_$status[[3]]$items, "_juv$"))
  status::remove_status_item(stage = 3, item = s)

#######################################################################
## Load the data in preparaton for data processing.                  ##
## There is two processing steps done at this stage.                 ##
## - rename AIMS_REEF_NAME to REEF (used as the reef id throughout)  ##
## - ensure there are no "INDETERMINATE" GROUP_DESC values           ##
#######################################################################
data <- ltmp_load_data_pt()

#######################################################################
## Fill in any gaps in the data.  These data gaps are primarily when ##
## there are no instances of a group in a particular year            ##
#######################################################################
data <- ltmp_fill_gaps_pt(data)

##################################################################################
## Determine the spatial domains (Region, NRM_region, Bioregion) for each reef   ##
## - aggregate the lat/longs to reef level                                       ##
## - join to the spatial lookup to add Regions, Zones,  Bioregions etc           ##
###################################################################################
data.spatial <- ltmp_spatial_domain(data)

##################################################################################
## As of 2024, there is has been a change in what should be considered          ##
## as MACROALGAE.  Angus has suggested that we should consider everything       ##
## that is not Turf algae or Coralline algae as Macroalgae.                     ##
##################################################################################
data <- ltmp_split_ma(data)

#########################################################################
## Generate a reference lookup to determine the major (top 6) groups   ##
## (REEFPAGE_CATEGORYs) and group all the minor groups into an 'Other' ##
## category. This will be created for all spatial domains, but it is   ##
## only relevant for reef level data                                   ##
#########################################################################
lookup <- ltmp_group_lookup_pt(data)


#########################################################################
## Use the lookup to replace the minor groups with 'Other' and         ##
## generate a vector of group names This will always be calculated but ##
## is only really relevant for reef level data                         ##
#########################################################################
data <- ltmp_reduce_groups_pt(data)

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
data <- ltmp_process_points_pt(data, data.spatial)

#########################################################################
## Save and export the processed data                                  ##
## - ../data/processed/  (native RData format)                         ##
## - ../data/processed/  (csv format)                                  ##
## - <aws_path>/processed  (if data_from is AWS)                       ##
#########################################################################
ltmp_save_data_pt(data)


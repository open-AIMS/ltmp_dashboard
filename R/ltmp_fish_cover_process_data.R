source("ltmp_startup_functions.R")
source("ltmp_process_functions.R")
source("ltmp_export_functions.R")
if (ltmp_is_parent()) ltmp_start_matter(args)

status::status_set_stage(stage = 3, title = "Process data")

for (s in  str_subset(status_$status[[3]]$items, "_pt$|_manta$|_juv$"))
  status::remove_status_item(stage = 3, item = s)

#######################################################################
## Load the data in preparaton for data processing.                  ##
## There is two processing steps done at this stage.                 ##
## - rename AIMS_REEF_NAME to REEF (used as the reef id throughout)  ##
## - ensure there are no "INDETERMINATE" GROUP_DESC values           ##
#######################################################################
data <- ltmp_load_data_fish()

#######################################################################
## Fill in any gaps in the data.  These data gaps are primarily when ##
## there are no instances of a group in a particular year            ##
#######################################################################
## data <- ltmp_fill_gaps_fish(data)

##################################################################################
## Determine the spatial domains (Region, NRM_region, Bioregion) for each reef   ##
## - aggregate the lat/longs to reef level                                       ##
## - join to the spatial lookup to add Regions, Zones,  Bioregions etc           ##
###################################################################################
data_spatial <- ltmp_spatial_domain(data)

##################################################################################
## Incorporate trophic groups and calculate density and biomass                 ##
##################################################################################
data <- ltmp_calc_density_fish(data)


##################################################################################
## Determine whether each fish is a newly monitored taxa or a traditionally     ##
## monitored taxa (OLD_FISH: TRUE).  This is done based on the list of          ##
## FISH_CODE in:                                                                ##
## ../data/parameters/traditional_fish.csv                                      ##
##################################################################################
data <- ltmp_old_new_fish(data)

###################################################################################
## Generate a reference lookup to determine the major (top 6) groups (GENUS for  ##
## Pomacentridae/Damselfishes, otherwise FAMILY) and group all the minor groups  ##
## into an 'Other' category. This will be created for all spatial domains, but   ##
## it is only relevant for reef level data.  This is only relavent for those     ##
## Groups that involve length calculations.                                      ##
###################################################################################
lookup_sizes <- ltmp_lookup_sizes_fish(data)

###################################################################################
## Generate a reference lookup to determine the major (top 6) groups (FAMILY)    ##
## and group all the minor groups.                                               ##
## This is specific for:                                                         ##
## - Harvested: (Labridae, Lethrinidae, Lutjanidae, Serranidae)                  ##
## - Herbivores: (Acanthuridae, Scarinae, Siganidae)                             ##
## This will be created for all spatial domains, it is only relevant for reef    ##
## level data.                                                                   ##
###################################################################################
lookup_h <- ltmp_lookup_h_fish(data)

###################################################################################
## Sum the abundances for the Large fishes and Damselfishes                      ##
###################################################################################
data_sum <- ltmp_process_sizes_fish(data, lookup_sizes)

###################################################################################
## Sum the abundances for the Total fishes group and add this to the Harvested   ##
## and Herbivores                                                                ##
###################################################################################
data_sum <- ltmp_process_total_fish(data, data_sum)

###################################################################################
## Sum the abundances for the Harvested and Herbivore groups                     ##
###################################################################################
data_sum <- ltmp_process_h_fish(data, lookup_h, data_sum)

###################################################################################
## Sum the abundances for the Coral Trout and Secondary Targets:                 ##
##  - Abundance                                                                  ##
##  - Density                                                                    ##
##  - Biomass                                                                    ##
###################################################################################
data_sum <- ltmp_process_trout_fish(data, data_sum)

###################################################################################
## Tidy up the variables and add the spatial domains                             ##
###################################################################################
data <- ltmp_process_fish(data_sum, data_spatial)

#########################################################################
## Save and export the processed data                                  ##
## - ../data/processed/  (native RData format)                         ##
## - ../data/processed/  (csv format)                                  ##
## - <aws_path>/processed  (if data_from is AWS)                       ##
#########################################################################
ltmp_save_data_pt(data)

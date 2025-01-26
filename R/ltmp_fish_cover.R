source("ltmp_startup_functions.R")

if (ltmp_is_parent()) ltmp_start_matter(args)

#################################################################################
## Read in the csv data from '/data/primary' and write out in native R to same ##
## location                                                                    ##
## Input:     ../data/primary/<filename>.csv                                   ##
## Output:    ../data/primary/<filename>.RData                                 ##
#################################################################################
source("ltmp_fish_cover_load_data.R")

##########################################################################
## Process the data and place the processed data in '../data/processed' ##
## Input:     ../data/primary/<filename>.RData                          ##
## Output:    ../data/processed/<filename>.RData                        ##
##            ../data/processed/<filename>.csv                          ##
##########################################################################
source("ltmp_fish_cover_process_data.R")

#########################################################################
## Fit models to the COVER data                                        ##
## Input:     ../data/processed/<filename>.RData                       ##
## output:    ../data/modelled/<filename>.csv                          ##
##            ../data/modelled/<filename>.json                         ##
##            ../data/modelled/<filename>.js                           ##
#########################################################################
source('ltmp_fish_cover_fit_models.R')

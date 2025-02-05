source("ltmp_startup_functions.R")
source("ltmp_model_functions.R")
source("ltmp_export_functions.R")
if (ltmp_is_parent()) ltmp_start_matter(args)

status::status_set_stage(stage = 4, title = "Fit models")

for (s in  str_subset(status_$status[[4]]$items, "_pt$"))
  status::remove_status_item(stage = 4, item = s)

#######################################################################
## Load the processed data in preparaton for model fitting.          ##
## There is two processing steps done at this stage.                 ##
#######################################################################
data <- ltmp_load_processed_data_pt()

#######################################################################
## Create the nested tibble                                          ##
#######################################################################
data <- 
  data |>
  group_by(VARIABLE) |>
  nest()

#######################################################################
## Make formula                                                      ##
#######################################################################
data <- data |> ltmp_get_formula_juv()
## data$form[[1]]

## Note, for juveniles (as of Oct 2024), it is necessary to use a
## cellmeans model for the formula. This is to allow the code to come
## up with strong priors for cases when all values are 0. Note, the
## treatment contrasts model formula will be converted just prior to
## fitting the model. So keep going as if it was a regular formula.

#######################################################################
## Create analysis splits                                            ##
#######################################################################
data <- data |> ltmp_analysis_splits()
## data$analysis_splits[[1]]

#######################################################################
## Apply analysis splits                                             ##
#######################################################################
data <- data |> ltmp_apply_splits()
## data1
## data1[1, 'data_group'][[1]][[1]]

#######################################################################
## Prepare nesting variables (ensure that all nested levels have     ##
## unique ids                                                        ##
#######################################################################
data <- data |> ltmp_prepare_nesting_variables()
## data1[1, 'data_group'][[1]][[1]] |> as.data.frame() |> head()

#######################################################################
## Update the formula (e.g. determine what fixed and random effects  ##
## to include in the formula based on the data_method and the number ##
## of levels of each factor                                          ##
#######################################################################
data <- data |> ltmp_update_formula()

#######################################################################
## Update the formula (e.g. determine what fixed and random effects  ##
## to include in the formula based on the data_method and the number ##
## of levels of each factor                                          ##
#######################################################################
data <- data |> ltmp_prepare_variables(VAR = "ABUNDANCE")

#######################################################################
## Generate the newdata (data containing the levels of the factors   ##
## for which predictions should be made                              ##
#######################################################################
data <- data |> ltmp_newdata(data_type = "juvenile")

#######################################################################
## Calculate raw hierarhical means and medians                       ##
#######################################################################
data <- data |> ltmp_raw_data_summaries_juv()
## data
## data$raw_sum[[1]]
## data$raw_sum[[4]]

#######################################################################
## Generate a label that concatenates:                               ##
## - VARIABLE                                                        ##
## - splits                                                          ##
## - data_method                                                     ##
## - data_scale                                                      ##
## - domain_name                                                     ##
## This is used to create file names                                 ##
#######################################################################
data <- data |> ltmp_make_modelling_label()
## data1$label[[5]]

null <- data |> ltmp_export_raw_data_pt()

#######################################################################
## Fit inla model(s)                                                 ##
## - binomial                                                        ##
## - beta-binomial                                                   ##
#######################################################################
data <- data |> ltmp_fit_inla_juv()

#######################################################################
## Calculate posteriors                                              ##
## - year_group_posteriors: (string pointer to file)                 ##
## - year_group_sum: (tibble)                                        ##
## - year_posteriors: (string pointer to file)                       ##
## - year_sum: (tibble)                                              ##
## - yearcomp_posteriors: (string pointer to file)                   ##
## - yearcomp_sum: (tibble)                                          ##
#######################################################################
data <- data |> ltmp_get_model_posteriors()

filenm <- str_replace(data$label[[1]], "([^_]*_[^_]*_[^_]*)_.*", "\\1")
saveRDS(data, file = paste0(DATA_PATH, "/modelled/", filenm, ".rds"))

#######################################################################
## Generate a plot that compares cellmeans from raw and each model   ##
## Figures saved in DATA_PATH/modelled/gg*.png                       ##
#######################################################################
data_compare <- data |> ltmp_compare_models()
## data_compare$gg[[1]]
## data_compare$gg[[5]]
## data_compare$gg[[6]]

#######################################################################
## Generate a plot that displays the raw trends for each transect    ##
## along with raw means and medians conditional on spatial           ##
## Figures saved in DATA_PATH/modelled/gg_raw_summ_*.png             ##
#######################################################################
raw_summary_plots <- data |> ltmp_raw_summary_plots()

source("ltmp_startup_functions.R")
source("ltmp_model_functions.R")
source("ltmp_export_functions.R")
if (ltmp_is_parent()) ltmp_start_matter(args)

status::status_set_stage(stage = 4, title = "Fit models")

for (s in  str_subset(status_$status[[4]]$items, "_pt$|_juv$|_fish$"))
  status::remove_status_item(stage = 4, item = s)

#######################################################################
## Load the processed data in preparaton for model fitting.          ##
## There is two processing steps done at this stage.                 ##
#######################################################################
data <- ltmp_load_processed_data_pt() |> mutate(sub_model = NA)

#######################################################################
## Create the nested tibble                                          ##
#######################################################################
model_lookup <- tribble(
  ~data_type, ~VARIABLE, ~model_type, ~model_response, ~sub_model, ~ylab, ~scale,
  "manta", NA, "Cover", "Cover", NA, "Percent cover", scales::label_percent(),
  ) |>
  dplyr::select(-VARIABLE) |> 
  crossing(VARIABLE = unique(data$VARIABLE)) |> 
  crossing(family_type = c("beta")) |>
  dplyr::select(-sub_model)

data <- data |> ltmp_nested_table(model_lookup)

#######################################################################
## Make formula                                                      ##
#######################################################################
data <- data |> ltmp_get_formula_manta()

#######################################################################
## Add group depth and reef_zone to the data                         ##
#######################################################################
data <- data |> ltmp_add_group_depth_zone_manta()

#######################################################################
## Create analysis splits                                            ##
#######################################################################
data <- data |> ltmp_analysis_splits()

#######################################################################
## Apply analysis splits                                             ##
#######################################################################
data <- data |> ltmp_apply_splits()

#######################################################################
## Prepare nesting variables (ensure that all nested levels have     ##
## unique ids                                                        ##
#######################################################################
data <- data |> ltmp_prepare_nesting_variables()

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
data <- data |> ltmp_prepare_variables()

#######################################################################
## Generate the newdata (data containing the levels of the factors   ##
## for which predictions should be made                              ##
#######################################################################
data <- data |> ltmp_newdata(data_type = "manta")

#######################################################################
## Calculate raw hierarhical means and medians                       ##
#######################################################################
data <- data |> ltmp_raw_data_summaries_manta()

#######################################################################
## Generate a label that concatenates:                               ##
## - VARIABLE                                                        ##
## - splits (<REEF_ZONE>_<fDEPTH>_<SHELF>                            ##
## - data_method                                                     ##
## - data_scale                                                      ##
## - domain_name                                                     ##
## This is used to create file names                                 ##
#######################################################################
data <- data |> ltmp_make_modelling_label()

null <- data |> ltmp_export_raw_data_pt()

#######################################################################
## Fit inla model(s)                                                 ##
## - binomial                                                        ##
## - beta-binomial                                                   ##
#######################################################################
data <- data |> ltmp_fit_inla_manta()

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

#######################################################################
## For each VARIABLE, select the "best" candidate model.  This could ##
## be done in a number of different ways.  The simplest is just to   ##
## determine which model has median values closest to the simple     ##
## raw means                                                         ##
#######################################################################
data <- data |> ltmp_choose_model()

filenm <- str_replace(data$label[[1]], "([^_]*_[^_]*_[^_]*)_.*", "\\1")
saveRDS(data, file = paste0(DATA_PATH, "/modelled/", filenm, ".rds"))

#######################################################################
## Generate a plot that compares cellmeans from raw and each model   ##
## Figures saved in DATA_PATH/modelled/gg*.png                       ##
#######################################################################
data_compare <- data |> ltmp_compare_models(model_lookup)
## data_group_compare <- data |> ltmp_group_compare_models()

#######################################################################
## Generate a plot that displays the raw trends for each transect    ##
## along with raw means and medians conditional on spatial           ##
## Figures saved in DATA_PATH/modelled/gg_raw_summ_*.png             ##
#######################################################################
raw_summary_plots <- data |> ltmp_raw_summary_plots(model_lookup)
## raw_group_summary_plots <- data |> ltmp_raw_group_summary_plots()

#######################################################################
## Prepare model summaries for export and then export to the AWS     ##
## bucket if it exists.                                              ##
#######################################################################
data_export <- data |> ltmp_prepare_export(model_lookup)
data_export |> ltmp_export_data()

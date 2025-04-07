source("ltmp_startup_functions.R")
source("ltmp_model_functions.R")
source("ltmp_export_functions.R")
if (ltmp_is_parent()) ltmp_start_matter(args)

status::status_set_stage(stage = 4, title = "Fit models")

for (s in  str_subset(status_$status[[4]]$items, "_juv$|_fish$|_manta$"))
  status::remove_status_item(stage = 4, item = s)

#######################################################################
## Load the processed data in preparaton for model fitting.          ##
## There is two processing steps done at this stage.                 ##
#######################################################################
data <- ltmp_load_processed_data_pt() |> mutate(sub_model = NA)

#######################################################################
## Create the nested tibble                                          ##
## 
#######################################################################
model_lookup <- tribble(
  ~data_type, ~VARIABLE, ~model_type, ~model_response, ~sub_model, ~ylab, ~scale,
  "photo-transect", NA, "Cover", "COUNT", NA, "Percent cover", scales::label_percent(),
  ) |>
  dplyr::select(-VARIABLE) |> 
  crossing(VARIABLE = unique(data$VARIABLE)) |> 
  crossing(family_type = c("binomial", "beta-binomial")) |>
  dplyr::select(-sub_model)

data <- data |> ltmp_nested_table(model_lookup)

#######################################################################
## Make formula                                                      ##
#######################################################################
data <- data |> ltmp_get_formula_pt()
## data1$form[[1]]

#######################################################################
## Create analysis splits                                            ##
#######################################################################
data <- data |> ltmp_analysis_splits()
## data1$analysis_splits[[1]]

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
## data1$form[[1]]

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
data <- data |> ltmp_newdata(data_type = "photo-transect")
## data1$newdata[[1]]

#######################################################################
## Calculate raw hierarhical means and medians                       ##
#######################################################################
data <- data |> ltmp_raw_data_summaries_pt()
## data1
## data1$raw_sum[[1]]
## data1$raw_sum[[5]]

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
## data1$label[[5]]

null <- data |> ltmp_export_raw_data_pt()

#######################################################################
## Fit inla model(s)                                                 ##
## - binomial                                                        ##
## - beta-binomial                                                   ##
#######################################################################
data <- data |> ltmp_fit_inla_pt()
## data1$model[[5]]
## summary(readRDS(data1$model[[5]]))

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
## data1$posteriors[[5]]
## readRDS(data1$posteriors[[5]]$year_posteriors)

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
data_group_compare <- data |> ltmp_group_compare_models(model_lookup)
## data_compare$gg[[1]]
## data_compare$gg[[5]]
## data_compare$gg[[6]]

#######################################################################
## Generate a plot that displays the raw trends for each transect    ##
## along with raw means and medians conditional on spatial           ##
## Figures saved in DATA_PATH/modelled/gg_raw_summ_*.png             ##
#######################################################################
raw_summary_plots <- data |> ltmp_raw_summary_plots(model_lookup)
raw_group_summary_plots <- data |> ltmp_raw_group_summary_plots(model_lookup)

#######################################################################
## Prepare model summaries for export and then export to the AWS     ##
## bucket if it exists.                                              ##
#######################################################################
data_export <- data |> ltmp_prepare_export(model_lookup)
data_export |> ltmp_export_data()


## i <- 9
## data$data_group[[i]] |>
##   ggplot(aes(x = REPORT_YEAR, y = PERC_COVER)) +
##   geom_line(aes(color = paste0(fDEPTH, TRANSECT_NO)), show.legend = FALSE) +
##   geom_line(data = data$raw_sum[[i]], aes(y = Mean, x = as.numeric(as.character(fYEAR)))) +
##   geom_line(data = data$raw_sum[[i]], aes(y = Median, x = as.numeric(as.character(fYEAR))),
##             linetype = "dashed") +
##   facet_wrap(~REEF) +
##   theme_bw()
## dev.off()



## data$data[[6]] |>  filter(REEF == "Geoffrey Bay", SITE_NO == 1, TRANSECT_NO == 1, fYEAR == 2024, fDEPTH == 2) |> as.data.frame() |> head()


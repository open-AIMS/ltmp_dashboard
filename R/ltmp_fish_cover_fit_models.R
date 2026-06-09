source("ltmp_startup_functions.R")
source("ltmp_model_functions.R")
## source("ltmp_write_functions.R")
source("ltmp_export_functions.R")
if (ltmp_is_parent()) ltmp_start_matter(args)

status::status_set_stage(stage = 4, title = "Fit models")

for (s in  str_subset(status_$status[[4]]$items, "_pt$|_manta$|_juv$"))
  status::remove_status_item(stage = 4, item = s)

#######################################################################
## Load the processed data in preparaton for model fitting.          ##
## There is two processing steps done at this stage.                 ##
#######################################################################
data <- ltmp_load_processed_data_pt()

data <- data |> mutate(sub_model = fish_sub)

#######################################################################
## Create the nested tibble                                          ##
#######################################################################
model_lookup <- tribble(
 ~data_type, ~VARIABLE, ~model_type, ~model_response, ~family_type, ~sub_model, ~ylab, ~scale,
 "fish", NA, "Density", "ABUNDANCE", "zeroinflatednbinomial1", NA, "Fish density per 250m²", scales::label_number(),
 "fish", NA, "Density", "ABUNDANCE", "nbinomial", NA, "Fish density per 250m²", scales::label_number(),
 "fish", NA, "Density", "ABUNDANCE", "zeroinflatedpoisson0", NA, "Fish density per 250m²", scales::label_number(),
 "fish", NA, "Density", "ABUNDANCE", "poisson", NA, "Fish density per 250m²", scales::label_number(),
 "fish", NA, "Biomass", "Biomass", "gamma", NA, "Fish biomass per 1000m²", scales::label_number(scale = 1)
 ## "fish", NA, "Biomass", "Biomass", "deltagamma", NA, "Fish biomass per 1000m²", scales::label_number(scale = 1)
 ## "fish", NA, "Biomass", "Biomass", "gamma", NA, "Fish biomass per 1000m²", scales::label_number(scale = 1),
 ## "fish", NA, "Biomass", "Biomass", "tweedie", NA, "Fish biomass per 1000m²", scales::label_number(scale = 1)
 ) |>
  dplyr::select(-VARIABLE) |>
  crossing(VARIABLE = unique(data$VARIABLE)) |>
  dplyr::filter(!(VARIABLE %in% c("Damselfishes", "Total fishes", "Harvested", "Herbivores") &
                  model_type == "Biomass")) |>
  dplyr::select(-sub_model) #|>
  ## full_join(data |>
  ##           dplyr::select(sub_model, VARIABLE) |>
  ##           distinct()
  ##           )

## model_lookup <- model_lookup |>
##   crossing(Zoning = c("Total", "Zoned"))


data <- data |>
  ## the fish data does not have fDEPTH, yet it needs to be there for
  ## the generality of some of the pre-modelling processing, so we
  ## will add a blank version
  mutate(fDEPTH = NA) |>
  ltmp_nested_table(model_lookup)

## data <-
##   data |>
##   ## the fish data does not have fDEPTH, yet it needs to be there for
##   ## the generality of some of the pre-modelling processing, so we
##   ## will add a blank version
##   mutate(fDEPTH = NA) |>
##   group_by(VARIABLE) |>
##   nest()

#######################################################################
## Make formula                                                      ##
#######################################################################
data <- data |> ltmp_get_formula_fish()

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

## Put the zoning stuff in here
## NEED TO DO THE PAIR CORRECTION FROM THE FILE MIKE SUPPLIED
rap_pair_lookup <- read_csv("../data/parameters/RAP Pair changes_ME.csv",
  show_col_types = FALSE)
## data1 <- data
data <-
  data |>
  mutate(data_group =  map(.x = data_group,
    .f = ~ {
      .x |>
        mutate(AIMS_REEF_NAME = str_replace(REEF, " _$", "")) |>
        left_join(rap_pair_lookup |>
                    dplyr::select(AIMS_REEF_NAME, NEW_RAP_PAIR, OPENORCLOSED_AFTER2004),
          by = c("AIMS_REEF_NAME")
        )
    })
  ) |>
  crossing(zoning = c("Total", "Zones")) |>
  mutate(analysis_splits = map2(analysis_splits, zoning,
    .f =  ~ {
      analysis_splits <- .x
      zoning <- .y
      analysis_splits[["ZONING"]] <- zoning
      analysis_splits
    })) |>
  mutate(data_group =  map2(data_group, zoning,
    .f = ~ {
      data_group <- .x
      zoning <- .y
      if (zoning == "Zones") {
        ## only include the years that have both open and closed reefs
        yrs <- data_group |>
          dplyr::filter(!is.na(OPENORCLOSED_AFTER2004)) |>
          droplevels() |>
          dplyr::select(OPENORCLOSED_AFTER2004, fYEAR) |>
          distinct() |>
          ungroup() |>
          group_by(fYEAR) |>
          count() |>
          filter(n == 2) |>
          pull(fYEAR) |>
          as.character()
        data_group <- data_group |>
          dplyr::filter(!is.na(OPENORCLOSED_AFTER2004)) |>
          droplevels() |>
          mutate(fZONE = factor(OPENORCLOSED_AFTER2004),
            fPAIR = factor(NEW_RAP_PAIR)) |>
          dplyr::filter(!is.na(fZONE), fYEAR %in% yrs, REPORT_YEAR > 2004) |>
          droplevels()
      } else {
        data_group <- data_group |>
          mutate(fZONE = "Total",
            fPAIR = NA)
      }
      data_group
    }))

#######################################################################
## Update the formula (e.g. determine what fixed and random effects  ##
## to include in the formula based on the data_method and the number ##
## of levels of each factor                                          ##
## Zoning added here                                                 ##
#######################################################################
data <- data |> ltmp_update_formula()

#######################################################################
## Update the formula (e.g. determine what fixed and random effects  ##
## to include in the formula based on the data_method and the number ##
## of levels of each factor.  Although some VARIABLES have both      ##
## ABUNDANCE and BIOMASS, only one of these is require for this      ##
## function.                                                         ##
#######################################################################
data <- data |> ltmp_prepare_variables()

#######################################################################
## Generate the newdata (data containing the levels of the factors   ##
## for which predictions should be made                              ##
#######################################################################
data <- data |> ltmp_newdata(data_type = "fish")

#######################################################################
## Calculate raw hierarhical means and medians                       ##
#######################################################################
data <- data |> ltmp_raw_data_summaries_fish()

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
## - gamma (Biomass)                                                 ##
## - zeroinflatednbinomial1 (Abundance)                              ##
## - nbinomial (Abundance)                                           ##
## - zeroinflatedpoisson0 (Abundance)                                ##
## - poisson (Abundance)                                             ##
#######################################################################
data <- data |> ltmp_fit_inla_fish()
gc(verbose = FALSE, full = TRUE)

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
## data <- readRDS(file = paste0(DATA_PATH, "/modelled/", filenm, ".rds"))

#######################################################################
## Generate a plot that compares cellmeans from raw and each model   ##
## Figures saved in DATA_PATH/modelled/gg*.png                       ##
#######################################################################
data_compare <- data |> ltmp_compare_models(model_lookup)
data_group_compare <- data |> ltmp_group_compare_models(model_lookup)

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

#######################################################################
## Delete all the non-selected model candidates                      ##
#######################################################################
data |> ltmp_delete_non_selected_models()

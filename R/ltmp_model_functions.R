
## Common functions ===================================================
ltmp_load_processed_data_pt <- function() {
  status::status_try_catch(
  {
    load(file = paste0(DATA_PATH, "processed/", RDATA_FILE))
    ## print(head(as.data.frame(data)))
  },
  stage_ = 4,
  order_ = 1,
  name_ = "Load data",
  item_ = "load_data"
  )
  return(data)  
}
ltmp_nested_table <- function(data, model_lookup) {
  status::status_try_catch(
  {
    data <- data |>
      group_by(VARIABLE, sub_model) |>
      nest() |>
      full_join(model_lookup |>
                dplyr::select(VARIABLE, model_type, family_type, model_response),
                by = "VARIABLE") |>
      dplyr::select(VARIABLE, model_type, family_type, model_response, everything())
  },
  stage_ = 4,
  order_ = 1,
  name_ = "Nest data",
  item_ = "nest_data"
  )
  return(data)  
}

## Photo-transect specific functions
ltmp_get_formula_pt <- function(data) {
  status::status_try_catch(
  {
    model.hierarchy = list(
      GBR=formula(~f(nNRM_YEAR, model='iid')+f(nREEF_YEAR, model='iid')+
                    f(SITE_NO, model='iid')+f(TRANSECT_NO, model='iid')),
      Zones=formula(~f(REEF, model='iid')+f(SITE_NO, model='iid')+f(TRANSECT_NO, model='iid')),
      nrm=formula(~f(REEF, model='iid')+f(SITE_NO, model='iid')+f(TRANSECT_NO, model='iid')),
      Bioregions=formula(~f(REEF, model='iid')+f(SITE_NO, model='iid')+f(TRANSECT_NO, model='iid')),
      Sectors=formula(~f(REEF, model='iid')+f(SITE_NO, model='iid')+f(TRANSECT_NO, model='iid')),
      reef=formula(~f(SITE_NO, model='iid')+f(TRANSECT_NO, model='iid'))
    )
    data <- data |> 
      mutate(form =  map(model_response,
                         .f = ~ {
                           data_scale <- status::get_setting(element = "data_scale")
                           form <- model.hierarchy[[data_scale]]
                           ## update.formula(form, COUNT ~.+fYEAR)
                           update.formula(form, paste0(.x, "~.+fYEAR"))
                         }
                         ))
    ## data <- data |> 
    ##   mutate(form =  map(status::get_setting(element = "data_scale"),
    ##                      .f = ~ {
    ##                        form <- model.hierarchy[[.x]]
    ##                        update.formula(form, COUNT ~.+fYEAR)
    ##                      }
    ##                      ))
    data
  },
  stage_ = 4,
  order_ = 2,
  name_ = "Make photo-transect formula",
  item_ = "make_formula_pt"
  )
  return(data)
}


## Common functions

## MODEL_GROUPS      - include fYEAR*fGROUP in models
## SPLIT_REEF_ZONE   - fit separate models per reef zone
## MODEL_REEF_ZONE   - append REEF_NAME with REEF_ZONE (and have
##                     this flow through SITE and TRANSECT) so that
##                     each ZONE is a separate reef
## SPLIT_DEPTH       - fit separate models per depth
## REPORT_DEPTH      - include separate depths in the output
## MODEL_DEPTH       - include f(fDEPTH, model='iid') in models
## SPLIT_SHELF       - fit separate models per shelf
ltmp_analysis_splits <- function(data) {
  status::status_try_catch(
  {
    data <- data |>
      mutate(analysis_splits = pmap(.l = list(status::get_setting(element = "data_scale"),
                                              data, VARIABLE),
                                    .f = ~ {
                                      data_scale <- ..1
                                      data <- ..2
                                      VARIABLE <- ..3
                                      if (data_scale == 'reef') {
                                        splits <- list(
                                          VARIABLE = VARIABLE,
                                          MODEL_GROUPS = TRUE,
                                          SPLIT_REEF_ZONE = TRUE,
                                          MODEL_REEF_ZONE = FALSE,
                                          SPLIT_DEPTH = TRUE,
                                          REPORT_DEPTH = TRUE,
                                          MODEL_DEPTH = FALSE,
                                          SPLIT_SHELF = FALSE
                                        )
                                      } else {
                                        splits <- list(
                                          VARIABLE = VARIABLE,
                                          MODEL_GROUPS = FALSE,
                                          SPLIT_REEF_ZONE = FALSE,
                                          MODEL_REEF_ZONE = FALSE,
                                          SPLIT_DEPTH = FALSE,
                                          REPORT_DEPTH = TRUE,
                                          MODEL_DEPTH = TRUE,
                                          SPLIT_SHELF = TRUE
                                        )
                                      }
                                      if (data_scale %in% c("GBR", "Sectors")) splits$SPLIT_SHELF <- FALSE
                                      if (data |> pull(fGROUP) |> unique() |> length() == 1) splits$MODEL_GROUPS=FALSE
                                      if (data |> pull(REEF_ZONE) |> unique() |> length() == 1) splits$MODEL_REEF_ZONE=FALSE
                                      if (data |> pull(fDEPTH) |> unique() |> length() == 1) splits$MODEL_DEPTH=FALSE
                                      splits
                                    }
                                    ))
    data
  },
  stage_ = 4,
  order_ = 3,
  name_ = "Make splits",
  item_ = "analysis_splits"
  )
  return(data)
}

ltmp_apply_splits <- function(dat) {
  status::status_try_catch(
  {
    dat <- dat |>
      mutate(data_group =
               map2(.x = data, .y = analysis_splits,
                    .f = ~ {
                      data <- .x
                      analysis_splits <- .y
                      data |>
                        mutate(splits = 
                                 paste0(if(analysis_splits$SPLIT_REEF_ZONE)
                                          str_replace(str_replace_na(REEF_ZONE, " "), "_", " ")
                                        else " ",
                                        "_",
                                        if(analysis_splits$SPLIT_DEPTH)
                                          str_replace(str_replace_na(data$fDEPTH, " "), "_", " ")
                                        else " ",
                                        "_",
                                        if(analysis_splits$SPLIT_SHELF)
                                          str_replace(str_replace_na(data$Shelf, " "), "_", " ")
                                        else " ",
                                        "_"
                                        )
                               ) |>
                        group_by(splits) |> 
                        nest() |>
                        dplyr::rename(data_group = data)
                    }
                    )) |>
      unnest(c("data_group"))

    dat <- dat |>
      mutate(data_group =
               map2(.x = data_group, .y = analysis_splits,
                    .f = ~ {
                      data_group <- .x
                      analysis_splits <- .y
                      if (analysis_splits$MODEL_GROUPS)
                        data_group <- data_group |>
                          mutate(fYEAR_GROUP = interaction(fYEAR, fGROUP))
                      data_group
                    }
                    ))
 
    dat
  },
  stage_ = 4,
  order_ = 4,
  name_ = "Apply splits",
  item_ = "apply_splits"
  )
  return(dat)
}

ltmp_prepare_nesting_variables <- function(dat) {
  status::status_try_catch(
  {
    dat <- 
      dat |>
      mutate(data_group = map2(.x = data_group, .y = analysis_splits,
                               .f = ~ {
                                 data_group <- .x
                                 analysis_splits <- .y
                                 data_group <-
                                   data_group |> 
                                   mutate(REEF = factor(if(!analysis_splits$MODEL_REEF_ZONE)
                                                          paste(REEF, REEF_ZONE)
                                                        else REEF),
                                          REEF_YEAR = factor(paste(REEF, REPORT_YEAR))
                                          )
                                 if ("SITE_NO" %in% names(data_group)) {
                                   data_group <- data_group |>
                                     mutate(SITE_NO = factor(if(!analysis_splits$MODEL_DEPTH) 
                                                               paste(REEF, SITE_NO, fDEPTH)
                                                             else paste(REEF, SITE_NO))
                                            )
                                 }
                                 if ("TRANSECT_NO" %in% names(data_group)) {
                                   data_group <- data_group |>
                                     mutate(TRANSECT_NO = factor(paste(SITE_NO, TRANSECT_NO)))
                                 }
                                 data_group |> droplevels()
                               }
                               )
             )
    dat
  },
  stage_ = 4,
  order_ = 5,
  name_ = "Prepare nesting vars",
  item_ = "prepare_nesting"
  )
  return(dat)
}

ltmp_update_formula <- function(dat) {
  status::status_try_catch(
  {
    dat <-
      dat |>
      mutate(form = pmap(.l = list(form, data_group, analysis_splits),
                         .f =  ~ {
                           form <- ..1
                           data_group <- ..2
                           analysis_splits <- ..3
                           if(analysis_splits$MODEL_DEPTH)
                             form <- update.formula(form, .~. + f(fDEPTH, model = "iid"))
                           if(analysis_splits$MODEL_GROUPS)
                             form <- update.formula(form, .~. + fYEAR*fGROUP)
                           if (length(unique(data_group$fYEAR)) < 2)
                             form <- remove_terms(form, "fYEAR")
                           if (length(unique(data_group$fGROUP)) < 2)
                             form <- remove_terms(form, "fGROUP")
                           if(status::get_setting(element = "data_scale") == "reef" &
                              status::get_setting(element = "data_method") == "photo-transect") {
                             form <- update(form, .~. + f(fYEAR_GROUP, model='iid'))
                           }
                           form
                         }
                         ))
    dat
  },
  stage_ = 4,
  order_ = 6,
  name_ = "Update formula",
  item_ = "update_formula"
  )
  return(dat)
}

remove_terms <- function(form, term) {
  fterms <- terms(form)
  fac <- attr(fterms, "factors")
  if (!term %in% colnames(fac)) return(form)
  idx <- which(as.logical(fac[term, ]))
  if (length(fac)>2) {
    new_fterms <- drop.terms(fterms, dropx = idx, keep.response = TRUE)
    return(formula(new_fterms))
  } else { # fYEAR is the only term and drop.terms must not be left without anything
    return(update(form, .~ 1))
  }
    
}

## ltmp_prepare_variables <- function(dat, VAR = "COUNT") {
ltmp_prepare_variables <- function(dat) {
  status::status_try_catch(
  {
    dat <- 
      dat |>
      mutate(data_group =
               map2(.x = data_group, .y = model_response,
               ## map(.x = data_group,
                   .f = ~ {
                     data_group <- .x
                     ## In addition to fitting the models, there is a
                     ## desire to have all the pairwise comparisons
                     ## between the final year and each previous year.
                     ## If we reorder the factor level such that the
                     ## final year is the first level, then we get this
                     ## for free...
                     data_group <- data_group |>
                       arrange(fYEAR) |>
                       mutate(fYEAR = fct_relevel(fYEAR, rev))
                     ## we also want to make sure that the most populus
                     ## group is the reference group_by
                     if (length(unique(data_group$fGROUP)) > 1) {
                       fgroup_levels <- data_group |>
                         group_by(fGROUP) |>
                         ## summarise(Sum=sum(.data[[VAR]], na.rm=TRUE)) |>
                         ## summarise(Sum=sum(!!sym(VAR), na.rm=TRUE)) |>
                         summarise(Sum=sum(get(.y), na.rm=TRUE)) |>
                         arrange(-Sum) |>
                         pull(fGROUP)
                       data_group <- data_group |>
                         mutate(fGROUP = fct_relevel(fGROUP, fgroup_levels))
                       data_group <- data_group |>
                         mutate(fYEAR_GROUP = interaction(fYEAR, fGROUP)) |>
                         droplevels()
                     }
                     data_group 
                   }
                   ))
    ## dat
  },
  stage_ = 4,
  order_ = 7,
  name_ = "Prepare vars",
  item_ = "prepare_variables"
  )
  return(dat)
}

ltmp_newdata <- function(dat, data_type = "photo-transect") {
  status::status_try_catch(
  {
    ## print(colnames(dat))
    ## print(head(as.data.frame(dat)))
    ## print(head(as.data.frame(dat$data_group[[1]])))
    dat <- dat |>
      mutate(newdata = map(.x = data_group,
                           .f = ~ {
                             data_group <- .x
                             if (data_type == "photo-transect") {
                             newdata <- data_group |>
                               tidyr::expand(fYEAR, fGROUP, REEF = NA,
                                             SITE = NA, SITE_NO = NA,
                                             TRANSECT_NO = NA, COUNT = NA,
                                             TOTAL = NA)
                             }
                             if (data_type == "juvenile") {
                             newdata <- data_group |>
                               tidyr::expand(fYEAR, fGROUP, REEF = NA,
                                             SITE = NA, SITE_NO = NA,
                                             ABUNDANCE = NA,
                                             AVAILABLE_SUBSTRATE = 1)
                             }
                             if (data_type == "manta") {
                               newdata <- data_group |>
                               tidyr::expand(fYEAR, fGROUP, REEF = NA,
                                             Cover = NA,
                                             Tows = NA, fTOW = NA)
                             }
                             if (data_type == "fish") {
                               newdata <- data_group |>
                               tidyr::expand(fYEAR, fGROUP, REEF = NA,
                                             SITE = NA, SITE_NO = NA,
                                             TRANSECT_NO = NA,
                                             ABUNDANCE = NA, Biomass =  NA)
                             }
                             ## Add back on the mean survey date so that
                             ## the modelled values can be plotted on an
                             ## x-axis reflecting sampling time Will
                             ## need to think about what to do about
                             ## higher spatial aggregates (domains)
                             newdata <- newdata |>
                               mutate(REPORT_YEAR = as.numeric(as.character(fYEAR))) |>
                               left_join(data_group |>
                                         group_by(fYEAR) |>
                                         summarise(DATE = if_else(length(unique(REEF)) == 1,
                                                                  mean(DATE, na.rm = TRUE),
                                                                  as.Date(paste0(unique(fYEAR), '-01-01')))) |>
                                         dplyr::select(fYEAR, DATE) |>
                                         distinct()
                                         ## by = "fYEAR"
                                         )
                             newdata
                           }
                           ))
    dat
  },
  stage_ = 4,
  order_ = 8,
  name_ = "Create newdata",
  item_ = "create_newdata"
  )
  return(dat)
}

ltmp_raw_data_summaries_pt <- function(dat) {
  status::status_try_catch(
  {
    dat <- dat |>
      mutate(raw_sum = map(.x = data_group,
                           .f =  ~ {
                             data_group <- .x
                             data_group |> 
                               mutate(cover = COUNT/TOTAL) |> 
                               group_by(REEF, REEF_ZONE, fDEPTH, SITE_NO, TRANSECT_NO, fYEAR) |>
                               summarise(cover = sum(cover),
                                         TOTAL = sum(TOTAL)) |>
                               ungroup() |> 
                               group_by(REEF, fYEAR) |> 
                               summarise(Mean = mean(cover),
                                         Median = median(cover),
                                         Total.count = sum(TOTAL)) |>
                               ungroup() |> 
                               group_by(fYEAR) |> 
                               summarise(Mean = mean(Mean),
                                         Median = median(Median),
                                         Total.count = sum(Total.count)
                                         ) |>
                               as.data.frame()
                           }
                           ))
    dat
  },
  stage_ = 4,
  order_ = 9,
  name_ = "Raw summaries",
  item_ = "raw_sum_pt"
  )
  return(dat)
}

ltmp_make_modelling_label <- function(dat) {
  status::status_try_catch(
  {
    dat <- dat |>
      mutate(label = pmap(.l = list(VARIABLE, family_type, splits, model_type, sub_model),
                          .f =  ~ paste(
                                status::get_setting(element = "data_method"),
                                status::get_setting(element = "data_scale"),
                                status::get_setting(element = "domain_name"),
                                ..1,  #VARIABLE
                                ..2,  #family_type
                                gsub("(.*)_$", "\\1", ..3),  #splits (take the last delimiter off)
                                ..4,  #model_type
                                ifelse(is.na(..5), " ", ..5),  #sub_model
                                sep = "_")))
    dat
  },
  stage_ = 4,
  order_ = 10,
  name_ = "Make model labels",
  item_ = "model_label"
  )
  return(dat)
}

ltmp_export_raw_data_pt <- function(dat) {
  status::status_try_catch(
  {
    dat <-
    pwalk(.l = list(dat$label, dat$raw_sum, dat$data_group),
          .f =  ~ {
            label <- ..1
            raw <- ..2
            dat <- ..3
            label <- paste0(DATA_PATH, "modelled/", label, ".rds")
            saveRDS(raw, file = gsub(".rds$", "_raw_sums.rds", label))
            saveRDS(dat, file = gsub(".rds$", "_raw_data.rds", label))
            }
          )
  },
  stage_ = 4,
  order_ = 10.5,
  name_ = "Export raw data",
  item_ = "export_raw"
  )
  return(NULL)
}

ltmp_fit_inla_pt <- function(dat) {
  status::status_try_catch(
  {
    dat <- dat |>
      ## crossing(model_type = c("binomial", "beta-binomial")) |> 
      mutate(model = pmap(.l = list(data_group, newdata, form, label, family_type),
                          .f = ~ ltmp__fit_inla_pt(..1, ..2, ..3, ..4, type = ..5)
                          ))
    dat
  },
  stage_ = 4,
  order_ = 11,
  name_ = "Fit models",
  item_ = "fit_models_pt"
  )
  return(dat)
}

ltmp__fit_inla_pt <- function(dat, newdata, form, label, type = "binomial") {
  ## label <- paste0(DATA_PATH, "modelled/", label, "_", type, ".rds")
  label <- paste0(DATA_PATH, "modelled/", label, ".rds")
  print(label)
  environment(form) <- new.env()
  data_pred <- dat |>
    bind_rows(newdata) 
  if (type == "binomial") {
    set.seed(123)
    mod.inla <- try({
      inla(form,
           data=data_pred,
           Ntrials=TOTAL,
           family='binomial',
           control.family=list(link='logit'),
           control.predictor=list(link=1, compute=TRUE),
           control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
           )
    }, silent = TRUE
    )
  }
  if (type == "beta-binomial") {
    set.seed(123)
    mod.inla <- try({
      inla(form,
           data=data_pred,
           Ntrials=TOTAL,
           family='betabinomial',
           control.family=list(link='logit'),
           control.predictor=list(link=1, compute=TRUE),
           control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
           )
    }, silent = TRUE
    )
  }
  if (inherits(mod.inla, "try-error")) {
    return("")
  }else {
    saveRDS(mod.inla, file = label)
    return(label)
  }
}

ltmp_get_model_posteriors <- function(dat) {
  status::status_try_catch(
  {
    plan(multicore, workers = parallelly::availableCores() - 1)
    ## future:set.seed(TRUE)
    dat <- dat |>
      mutate(posteriors = future_map2(.x = data_group, .y = model,
                                      .f = ~ get_model_posteriors(.y, .x)
                                      ))
    dat
  },
  stage_ = 4,
  order_ = 12,
  name_ = "Get posteriors",
  item_ = "get_posteriors"
  )
  return(dat)
}

get_model_posteriors <- function(mod_str, data.group) {
  print(mod_str)
  if (mod_str == "") return(NULL) 
  mod <- readRDS(mod_str)
  nd <- nrow(data.group)
  nt <- nrow(mod$.args$data)
  newdata <- mod$.args$data[(nd + 1):nt, ]
  ## For cells with corresponding input sums that equal 0 (e.g. not
  ## present at all in the cell) replace the mean, median and
  ## lower/upper with 0
  if ("COUNT" %in% names(data.group)) {  ## Photo-transect only
  replace_0 <- data.group %>%
    group_by(fYEAR, fGROUP, .drop = FALSE) %>%
    summarise(Sum = sum(COUNT))
  } else {  # Juveniles have a different way of dealing with this
    replace_0 <- tribble(~fYEAR, ~fGROUP, ~Sum)
  }

  draws <- inla.posterior.sample(n = 1000, mod, seed = 123)

  ## get the inverse link from the model
  link_fun <- mod$.args$control.family[[1]]$link
  inv_link <- function(x) get(paste0("inla.link.", link_fun))(x, inverse = TRUE)
  
  cellmeans <- sapply(draws, function(x) x[[2]][(nd + 1):nt])
  if(is.null(dim(cellmeans))) cellmeans <- t(cellmeans)
  year_group_posteriors <- get_year_group_posteriors(newdata, cellmeans, replace_0, inv_link)
  year_group_posteriors_label <- gsub(".rds$", "_year_group_posteriors.rds", mod_str)
  saveRDS(year_group_posteriors, file = year_group_posteriors_label)
  write_csv(year_group_posteriors |> dplyr::select(-fYEAR),
            file = gsub(".rds", ".csv", year_group_posteriors_label))
  ## write_aws(filenm = basename(gsub(".rds$", ".csv", year_group_posteriors_label)),
  ##           catalog_file = TRUE)

  year_group_sum <- year_group_posteriors |>
    group_by(fYEAR, fGROUP, REPORT_YEAR, DATE) %>%
    mean_median_hdci(value) %>%
    ungroup()
  saveRDS(year_group_sum, file = gsub("posteriors", "sum", year_group_posteriors_label))

  ## marginalise over fGROUP
  year_posteriors <- get_year_posteriors(newdata, cellmeans, replace_0, inv_link)
  year_posteriors_label <- gsub(".rds$", "_year_posteriors.rds", mod_str)
  saveRDS(year_posteriors, file = year_posteriors_label)
  write_csv(year_posteriors |> dplyr::select(-fYEAR),
            file = gsub(".rds$", ".csv", gsub("_year_", "_annual_", year_posteriors_label)))
  ## write_aws(filenm = basename(gsub(".rds$", ".csv",
  ##                                  gsub("_year_", "_annual_", year_posteriors_label))),
  ##           catalog_file = TRUE)

  year_sum <- year_posteriors %>%
    group_by(fYEAR, REPORT_YEAR, DATE) %>%
    mean_median_hdci(value)
  ## for binomial models, make sure the upper limit maxes out a 1
  if (link_fun == "logit") {
    year_sum <- year_sum |> 
      mutate(upper = ifelse(upper > 1, 1, upper))
  }
  saveRDS(year_sum, file = gsub("posteriors", "sum", year_posteriors_label))
  
  ## year comparisons
  if (length(unique(year_posteriors$fYEAR)) > 1) {  ## no point if there is only one year
    yearcomp_posteriors <- get_yearcomp_posteriors(newdata, year_posteriors)
    yearcomp_posteriors_label <- gsub(".rds$", "_yearcomp_posteriors.rds", mod_str)
    saveRDS(yearcomp_posteriors, file = yearcomp_posteriors_label)

    yearcomp_sum <- yearcomp_posteriors |>
      ungroup() |>
      group_by(YearComp) |>
      dplyr::select(-.draw) |>
      summarise_draws(mean, median, HDInterval::hdi,
                      pl1 = ~ mean(.x < 0),
                      pl2 = ~ mean(.x < 1),
                      pg1 = ~ mean(.x > 0),
                      pg2 = ~ mean(.x > 1)
                      ) |>
      ungroup() |>
      mutate(Pl = ifelse(variable == "value", pl1, pl2)) |>
      mutate(Pg = ifelse(variable == "value", pg1, pg2)) |>
      dplyr::select(-pl1, -pl2, -pg1, -pg2) |>
      arrange(desc(YearComp))
    saveRDS(yearcomp_sum, file = gsub("posteriors", "sum", yearcomp_posteriors_label))
  } else {
    yearcomp_posteriors_label <- ""
    yearcomp_sum <- NULL
  }

  ## all year comparisons
  if (length(unique(year_posteriors$fYEAR)) > 1) {  ## no point if there is only one year
    all_yearcomp_posteriors <- get_all_yearcomp_posteriors(newdata, year_posteriors)
    all_yearcomp_posteriors_label <- gsub(".rds$", "_all_yearcomp_posteriors.rds", mod_str)
    saveRDS(all_yearcomp_posteriors, file = all_yearcomp_posteriors_label)

    all_yearcomp_sum <- all_yearcomp_posteriors |>
      ungroup() |>
      group_by(YearComp) |>
      dplyr::select(-.draw) |>
      summarise_draws(mean, median, HDInterval::hdi,
                      pl1 = ~ mean(.x < 0),
                      pl2 = ~ mean(.x < 1),
                      pg1 = ~ mean(.x > 0),
                      pg2 = ~ mean(.x > 1)
                      ) |>
      ungroup() |>
      mutate(Pl = ifelse(variable == "value", pl1, pl2)) |>
      mutate(Pg = ifelse(variable == "value", pg1, pg2)) |>
      dplyr::select(-pl1, -pl2, -pg1, -pg2) |>
      arrange(desc(YearComp))
    saveRDS(all_yearcomp_sum, file = gsub("posteriors", "sum", all_yearcomp_posteriors_label))
  } else {
    all_yearcomp_posteriors_label <- ""
    all_yearcomp_sum <- NULL
  }


  return(list(year_group_posteriors = year_group_posteriors_label, #year_group_posteriors,
         year_group_sum = year_group_sum,
         year_posteriors = year_posteriors_label, #year_posteriors,
         year_sum = year_sum,
         yearcomp_posteriors = yearcomp_posteriors_label, #yearcomp_posteriors,
         yearcomp_sum = yearcomp_sum,
         all_yearcomp_posteriors = all_yearcomp_posteriors_label, #all_yearcomp_posteriors,
         all_yearcomp_sum = all_yearcomp_sum
         )
  )
}
get_year_group_posteriors <- function(newdata, cellmeans, replace_0, inv_link = plogis) {
  newdata <- newdata |>
    dplyr::select(fYEAR, fGROUP, REPORT_YEAR, DATE) |>
    ## cbind(plogis(cellmeans)) %>%
    cbind(inv_link(cellmeans)) %>%
    rename_with(function(x) x <- 1, matches("plogis")) %>%
    pivot_longer(cols = matches("[0-9]"), names_to = ".draw")
  if (nrow(replace_0) > 0) {
    newdata <- newdata |>
      full_join(replace_0) %>%
      mutate(across(value, function(x) ifelse(.$Sum == 0, 0, x))) %>%
      dplyr::select(-Sum)
  }
  newdata <- newdata |>
    droplevels() %>%
    group_by(fYEAR, fGROUP, REPORT_YEAR, DATE, .draw) %>%
    summarise(value = sum(value)) %>%
    ungroup() |>
    ## if there are still NAs, replace with 0.  these NA values are for fGROUPs
    ## that are not present in the data in that year
    mutate(value = ifelse(is.na(value), 0, value))  
  newdata
}
get_year_posteriors <- function(newdata, cellmeans, replace_0, inv_link) {
  newdata <- newdata |>
    dplyr::select(fYEAR,fGROUP, REPORT_YEAR, DATE) %>%
    ## cbind(plogis(cellmeans)) %>%
    cbind(inv_link(cellmeans)) %>%
    pivot_longer(cols = matches("[0-9]"), names_to = ".draw") 
  if (nrow(replace_0) > 0) {
    newdata <- newdata |>
      full_join(replace_0) %>%
      mutate(across(value, function(x) ifelse(.$Sum == 0,0,x))) %>%
      dplyr::select(-Sum) %>%  droplevels()
  }
  newdata <- newdata |>
    group_by(fYEAR,REPORT_YEAR, DATE, .draw) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()
  newdata 
}
get_yearcomp_posteriors <- function(newdata, year_posteriors) {
  yearcomp <- newdata %>%
    tidyr::expand(fYEAR) %>%
    mutate(fYEAR = paste0(first(fYEAR), "-" ,fYEAR)) %>%
    slice(-1) %>%
    mutate(fYEAR = factor(fYEAR, levels = fYEAR))
  if (nrow(yearcomp) == 0) {
    yearcomp_posteriors <- newdata %>%
      tidyr::expand(fYEAR) %>%
      rename(YearComp = fYEAR) %>%
      mutate(variable = c("value","frac"), mean = NA, median = NA,
             lower = NA, upper = NA, Pl = NA, Pg = NA)
  } else {
    yearcomp_posteriors <-  year_posteriors |>
      group_by(.draw) |>
      ## reframe(
      summarise(
        frac = exp(as.vector(as.vector(log(value)) %*%
                             t(cbind(1, -1 * model.matrix(~fYEAR)[-1, -1])))),
        value = as.vector(as.vector(value) %*%
                          t(cbind(1, -1 * model.matrix(~fYEAR)[-1, -1]))),
        YearComp = paste0(first(fYEAR), "-", fYEAR[-1]),
        ) |>
      dplyr::select(YearComp, .draw, value, frac) 
  }
  return(yearcomp_posteriors)
}
get_all_yearcomp_posteriors <- function(newdata, year_posteriors) {
  years <- levels(newdata$fYEAR)
  xmat <- emmeans:::tukey.emmc(years)
  if (length(years) == 0) {
    all_yearcomp_posteriors <- newdata %>%
      tidyr::expand(fYEAR) %>%
      rename(YearComp = fYEAR) %>%
      mutate(variable = c("value","frac"), mean = NA, median = NA,
             lower = NA, upper = NA, Pl = NA, Pg = NA)
  } else {
    all_yearcomp_posteriors <-  year_posteriors |>
      group_by(.draw) |>
      ## reframe(
      summarise(
        frac = exp(as.vector(as.vector(log(value)) %*% as.matrix(xmat))),
        value = as.vector(as.vector(value) %*% as.matrix(xmat)),
        YearComp = names(xmat)
        ) |>
      dplyr::select(YearComp, .draw, value, frac) 
  }
  return(all_yearcomp_posteriors)
}
mean_median_hdci <- function(.data, ...) {
  ## groups = group_vars(.data)
  ## print(class(groups))
  col_exprs <- quos(..., .named=TRUE)
  col_expr = col_exprs[[1]]
  col_name = names(col_exprs)
  x1 <- .data %>% mean_hdci(!!sym(col_name))
  x2 <- .data %>% median_hdci(!!sym(col_name))
  x1 %>% dplyr::rename(mean=value, lower=.lower, upper=.upper) %>%
    dplyr::select(-.point,-.width,-.interval) %>%
    bind_cols(x2 %>% ungroup %>% dplyr::select(median=value))
}

ltmp_compare_models <- function(dat, model_lookup) {
  status::status_try_catch(
  {
    plan(multicore, workers = parallelly::availableCores() - 1)
    ## future:set.seed(TRUE)
    dat_compare <-
      dat |>
      left_join(model_lookup |>
                dplyr::select(VARIABLE, model_type, family_type, ylab, scale),
                by = c("VARIABLE", "model_type", "family_type")) |> 
      mutate(compare_models =
               future_map2(.x = posteriors, .y = family_type,
                    .f = ~ {
                      if (is.null(.x)) return(NULL)
                      .x$year_sum |>
                        mutate(family_type = .y)
                    }
                    )) |>
      ## ## this effects fish only.  It alters the label to distinguish between ABUNDANCE and Biomass models
      ## (\(.x) if ("sub_variable" %in% names(.x))
      ##         .x |> mutate(label = paste0(label, "_", model_type))
      ##       else .x
      ## )() |> 
      mutate(label = str_replace(label, paste0(family_type, "_"), "")) |> 
      dplyr::select(VARIABLE, sub_model, splits, label, raw_sum, compare_models, ylab, scale, selected) |>
      unnest(c(compare_models)) |> 
      ## group_by(VARIABLE, sub_model, splits, label, raw_sum, ylab, scale) |>
      ## group_by(VARIABLE, sub_model, splits, ylab, scale) |>
      group_by(VARIABLE, sub_model, splits, ylab) |> 
      nest() |>
      ## mutate(gg = future_pmap(.l = list(data, raw_sum, label, ylab, scale),
      mutate(gg = future_pmap(.l = list(data, ylab),
                       .f =  ~ {
                         data <- ..1
                         raw_sum <- data$raw_sum[[1]] #unique(data$raw_sum) #..2
                         lab <- unique(data$label)
                         ylab <- ..2 #..4
                         yscale <- unique(data$scale)[[1]] #..5

                         data <- data |>
                           mutate(family_type = ifelse(selected,
                                                       paste0("*", family_type, "*"),
                                                       family_type))
                         ## lookup <- tribble(~data_type, ~VARIABLE, ~sub_variable, ~ylab, ~scale,
                         ##                   "photo-transect", NA, NA, "Percent cover", scales::label_percent(),
                         ##                   "manta", NA, NA, "Percent cover", scales::label_percent(),
                         ##                   "juvenile", NA, NA, "Juveniles per m²", scales::label_percent(),
                         ##                   "fish", NA, "ABUNDANCE", "Fish density per 250m²", scales::label_number(),
                         ##                   "fish", NA, "Biomass", "Fish biomass per 1000m²", scales::label_number(scale = 4)
                         ##                   )
                                           
                         ## st <- strsplit(lab, "_")[[1]]
                         ## lookup <- lookup |> filter(data_type == st[[1]])
                         ## if (length(st) == 9) 
                         ##   lookup <- lookup |> filter(sub_variable == st[[9]])
                         ## ylab <- lookup$ylab
                         ## yscale <- lookup$scale[[1]]
                         gg <-
                           data |> ggplot(aes(x = REPORT_YEAR, y = median)) +
                           geom_ribbon(aes(ymin = lower, ymax = upper),
                                       fill = "orange", alpha = 0.3) +
                           geom_line(aes(group = family_type, colour = family_type)) +
                           geom_point()  +
                           geom_line(data = raw_sum,
                                     aes(y = Mean, x = as.numeric(as.character(fYEAR)),
                                         colour = "Raw mean")) +
                           geom_line(data = raw_sum,
                                     aes(y = Median, x = as.numeric(as.character(fYEAR)),
                                         colour = "Raw median")) +
                           facet_wrap(~family_type) +
                           ggtitle(str_replace_all(lab, "_", " ")) +
                           scale_y_continuous(ylab, label = yscale) +
                               ## scale_y_continuous(ylab, label = scales::label_percent())
                           theme_bw() +
                           theme(axis.title.x = element_blank(),
                                 strip.background = element_rect(fill = "lightblue")
                                 )
                         filenm <- paste0(FIGURES_PATH, "", "gg_", lab, ".png")
                         ggsave(filename = filenm, plot = gg, width = 12, height = 7)
                         filenm
                       }
                       ))
    dat_compare  
  },
  stage_ = 4,
  order_ = 13,
  name_ = "Compare models",
  item_ = "compare_models"
  )
  return(dat_compare)
}

ltmp_group_compare_models <- function(dat, model_lookup) {
  status::status_try_catch(
  {
    plan(multicore, workers = parallelly::availableCores() - 1)
    if (status::get_setting(element = "data_scale") == "reef") {
    dat_group_compare <-
      dat |>
      left_join(model_lookup |>
                dplyr::select(VARIABLE, model_type, family_type, ylab, scale),
                by = c("VARIABLE", "model_type", "family_type")) |> 
      dplyr::filter(model_type != "Biomass") |>
      droplevels() |> 
      mutate(compare_models =
               future_map2(.x = posteriors, .y = family_type,
                    .f = ~ {
                      if (is.null(.x)) return(NULL)
                      .x$year_group_sum |>
                        mutate(family_type = .y)
                    }
                    )) |>
      ## ## this effects fish only.  It alters the label to distinguish between ABUNDANCE and Biomass models
      ## (\(.x) if ("sub_variable" %in% names(.x))
      ##         .x |> mutate(label = paste0(label, "_", sub_variable))
      ##       else .x
      ## )() |> 
      mutate(label = str_replace(label, paste0(family_type, "_"), "")) |> 
      dplyr::filter(selected) |> 
      dplyr::select(VARIABLE, sub_model, splits, label, data_group, compare_models, ylab, scale) |>
      unnest(c(compare_models)) |> 
      group_by(VARIABLE, sub_model, splits, label, data_group, ylab, scale) |>
      nest() |>
      mutate(gg = future_pmap(.l = list(data, data_group, label, ylab, scale),
                       .f =  ~ {
                         data <- ..1
                         data_group <- ..2
                         lab <- ..3
                         ylab <- ..4
                         yscale <- ..5
                         ## lookup <- tribble(~data_type, ~VARIABLE, ~sub_variable, ~ylab, ~scale,
                         ##                   "photo-transect", NA, NA, "Percent cover", scales::label_percent(),
                         ##                   "manta", NA, NA, "Percent cover", scales::label_percent(),
                         ##                   "juvenile", NA, NA, "Juveniles per m²", scales::label_percent(),
                         ##                   "fish", NA, "ABUNDANCE", "Fish density per 250m²", scales::label_number(),
                         ##                   "fish", NA, "Biomass", "Fish biomass per 1000m²", scales::label_number(scale = 4)
                         ##                   )
                         ## st <- strsplit(lab, "_")[[1]]
                         ## lookup <- lookup |> filter(data_type == st[[1]])
                         ## if (length(st) == 9) 
                         ##   lookup <- lookup |> filter(sub_variable == st[[9]])
                         ## ylab <- lookup$ylab
                         ## yscale <- lookup$scale[[1]]

                         if (length(unique(data$fGROUP))<2) return(NULL)
                         ## data |>
                         ##   group_by(fGROUP) |>
                         ##   summarise(cover = sum(median)) |>
                         ##   ungroup() |>
                         ##   arrange(cover) |>
                         ##   pull(fGROUP)
                         gg <-
                           data |>
                           mutate(fGROUP = forcats::fct_reorder(fGROUP, median)) |> 
                           ggplot(aes(x = REPORT_YEAR, y = median)) +
                           geom_bar(aes(fill = fGROUP), stat = "Identity", position = "stack") +
                           facet_grid(~family_type) +
                           ggtitle(str_replace_all(lab, "_", " ")) +
                           scale_y_continuous(ylab, label = yscale) +
                           ## scale_y_continuous("Cover", label = scales::label_percent()) +
                           scale_fill_discrete("", labels = function(x) rev(x)) +
                           theme_bw() +
                           theme(axis.title.x = element_blank(),
                                 strip.background = element_rect(fill = "lightblue")
                                 )
                         filenm <- paste0(FIGURES_PATH, "", "gg_group_", lab, ".png")
                         ggsave(filename = filenm, plot = gg, width = 12, height = 7)
                         filenm
                       }
                       ))
    dat_group_compare  
    } else {
      dat_group_compare <- dat |> mutate(gg = "")
    }
  },
  stage_ = 4,
  order_ = 13,
  name_ = "Compare group models",
  item_ = "compare_group_models"
  )
  return(dat_group_compare)
}


ltmp_raw_summary_plots <- function(dat, model_lookup) {
  status::status_try_catch(
  {
    if (status::get_setting("data_from") == "AWS") return(NULL)
    plan(multicore, workers = parallelly::availableCores() - 1)
    ## future:set.seed(TRUE)
    raw_plots <-
      dat  |>
      left_join(model_lookup |>
                dplyr::select(VARIABLE, model_type, family_type, ylab, scale),
                by = c("VARIABLE", "model_type", "family_type")) |> 
      ## ## this effects fish only.  It alters the label to distinguish between ABUNDANCE and Biomass models
      ## (\(.x) if ("sub_variable" %in% names(.x))
      ##         .x |> mutate(label = paste0(label, "_", sub_variable))
      ##       else .x
      ## )() |> 
      mutate(label = str_replace(label, paste0(family_type, "_"), "")) |> 
      mutate(gg =
               future_pmap(.l = list(data_group, raw_sum, label, ylab, scale, model_response),
                    .f = ~ {
                      data_group <- ..1
                      raw_sum <- ..2
                      lab <- ..3
                         ylab <- ..4
                         yscale <- ..5
                      resp <- ..6
                      ## lookup <- tribble(~data_type, ~resp, ~sub_variable, ~ylab, ~scale,
                      ##                   "photo-transect", "PERC_COVER", NA, "Percent cover", scales::label_percent(),
                      ##                   "manta", "Cover", NA, "Percent cover", scales::label_percent(),
                      ##                   "juvenile", "PERC_COVER", NA, "Juveniles per m²", scales::label_percent(),
                      ##                   "fish", "ABUNDANCE", "ABUNDANCE", "Fish density per 250m²", scales::label_number(),
                      ##                   "fish", "Biomass", "Biomass", "Fish biomass per 1000m²", scales::label_number(scale = 4)
                      ##                   )
                      ## st <- strsplit(lab, "_")[[1]]
                      ## lookup <- lookup |> filter(data_type == st[[1]])
                      ## if (length(st) == 9) 
                      ##   lookup <- lookup |> filter(sub_variable == st[[9]])
                      ## ylab <- lookup$ylab
                      ## yscale <- lookup$scale[[1]]
                      ## resp <- lookup$resp[[1]]
                      if (status::get_setting(element = "data_method") %in%
                          c("photo-transect", "juvenile", "fish")) {
                        if (resp == "COUNT") {
                          dg <- 
                            data_group |> 
                            group_by(REPORT_YEAR, REEF, fDEPTH, TRANSECT_NO) |>
                            summarise(value = sum(PERC_COVER)) |>
                            ungroup() 
                        } else if (resp == "ABUNDANCE") {
                          dg <- 
                            data_group |> 
                            group_by(REPORT_YEAR, REEF, fDEPTH, TRANSECT_NO) |>
                            summarise(value = sum(ABUNDANCE)) |>
                            ungroup() 
                        } else if (resp == "Biomass") {
                          dg <- 
                            data_group |> 
                            group_by(REPORT_YEAR, REEF, fDEPTH, TRANSECT_NO) |>
                            summarise(value = sum(Biomass)) |>
                            ungroup() 
                        }

                      } else {  ## manta
                        dg <- 
                          data_group |> 
                          group_by(REPORT_YEAR, REEF, fDEPTH) |>
                          summarise(value = mean(Cover)) |>
                          mutate(TRANSECT_NO = NA) |> 
                          ungroup()
                      }
                      gg <- 
                        dg |> 
                        ggplot(aes(x = REPORT_YEAR, y = value)) +
                        geom_line(aes(color = paste0(fDEPTH, TRANSECT_NO)),
                                  show.legend = FALSE) +
                        geom_line(data = raw_sum,
                                  aes(y = Mean,
                                      x = as.numeric(as.character(fYEAR)))) +
                        geom_line(data = raw_sum,
                                  aes(y = Median,
                                      x = as.numeric(as.character(fYEAR))),
                                  linetype = "dashed") +
                        scale_y_continuous(ylab, label = yscale) +
                        scale_x_continuous("") +
                        facet_wrap(~REEF) +
                        theme_bw()
                      filenm <- paste0(FIGURES_PATH, "", "gg_raw_sum_", lab, ".png")
                      saveRDS(gg, file = paste0(FIGURES_PATH, "", "gg_raw_sum_", lab, ".rds"))
                      ggsave(filename = filenm, plot = gg, width = 12, height = 7)
                      filenm
                    }
                    ))
    raw_plots
  },
  stage_ = 4,
  order_ = 14,
  name_ = "Raw summary plots",
  item_ = "raw_summary_plots"
  )
  return(raw_plots)
}



ltmp_raw_group_summary_plots <- function(dat, model_lookup) {
  status::status_try_catch(
  {
    if (status::get_setting("data_from") == "AWS") return(NULL)
    plan(multicore, workers = parallelly::availableCores() - 1)
    ## future:set.seed(TRUE)
    if (status::get_setting(element = "data_scale") == "reef" &
        status::get_setting(element = "data_method") != "manta") {
      raw_group_plots <-
        dat |>
        left_join(model_lookup |>
                  dplyr::select(VARIABLE, model_type, family_type, ylab, scale),
                  by = c("VARIABLE", "model_type", "family_type")) |> 
        ## ## this effects fish only.  It alters the label to distinguish between ABUNDANCE and Biomass models
        ## (\(.x) if ("sub_variable" %in% names(.x))
        ##          .x |> mutate(label = paste0(label, "_", sub_variable))
        ##        else .x
        ## )() |> 
      mutate(label = str_replace(label, paste0(family_type, "_"), "")) |> 
        mutate(gg =
                 future_pmap(.l = list(data_group, raw_sum, label, ylab, scale, model_response),
                      .f = ~ {
                        data_group <- ..1
                        raw_sum <- ..2
                        lab <- ..3
                         ylab <- ..4
                         yscale <- ..5
                      resp <- ..6

                        ## lookup <- tribble(~data_type, ~resp, ~sub_variable, ~ylab, ~scale,
                        ##                   "photo-transect", "PERC_COVER", NA, "Percent cover", scales::label_percent(),
                        ##                   "manta", "Cover", NA, "Percent cover", scales::label_percent(),
                        ##                   "juvenile", "PERC_COVER", NA, "Juveniles per m²", scales::label_percent(),
                        ##                   "fish", "ABUNDANCE", "ABUNDANCE", "Fish density per 250m²", scales::label_number(),
                        ##                   "fish", "Biomass", "Biomass", "Fish biomass per 1000m²", scales::label_number(scale = 4)
                        ##                   )
                        ## st <- strsplit(lab, "_")[[1]]
                        ## lookup <- lookup |> filter(data_type == st[[1]])
                        ## if (length(st) == 9) 
                        ##   lookup <- lookup |> filter(sub_variable == st[[9]])
                        ## ylab <- lookup$ylab
                        ## yscale <- lookup$scale[[1]]
                        ## resp <- lookup$resp[[1]]

                        ## if (status::get_setting(element = "data_method") %in%
                        ##     c("photo-transect", "juvenile", "fish")) {
                        if (status::get_setting(element = "data_method") %in%
                            c("photo-transect", "juvenile", "fish")) {
                          if (resp == "COUNT") {
                            dg <- data_group |> mutate(value = PERC_COVER)
                          } else if (resp == "ABUNDANCE") {
                            dg <- data_group |> mutate(value = ABUNDANCE)
                          } else if (resp == "Biomass") {
                            dg <- data_group |> mutate(value = Biomass)
                          }
                        } else {  ## manta
                          dg <- data_group |> mutate(value = Cover)
                        }
                        gg <- 
                          dg |> 
                          ggplot(aes(x = REPORT_YEAR, y = value)) +
                          geom_line(aes(color = paste0(fDEPTH, TRANSECT_NO)),
                                    show.legend = FALSE) +
                          geom_line(data = raw_sum,
                                    aes(y = Mean,
                                        x = as.numeric(as.character(fYEAR)))) +
                          geom_line(data = raw_sum,
                                    aes(y = Median,
                                        x = as.numeric(as.character(fYEAR))),
                                    linetype = "dashed") +
                          scale_y_continuous(ylab, label = yscale) +
                          scale_x_continuous("") +
                          facet_wrap(~fGROUP) +
                          theme_bw()
                        ## }
                        filenm <- paste0(FIGURES_PATH, "", "gg_raw_group_sum_", lab, ".png")
                        saveRDS(gg, file = paste0(FIGURES_PATH, "", "gg_raw_group_sum_", lab, ".rds"))
                        ggsave(filename = filenm, plot = gg, width = 12, height = 7)
                        filenm
                      }
                      ))
    } else {
      raw_group_plots <- dat |> mutate(gg = "")
    }
    raw_group_plots
  },
  stage_ = 4,
  order_ = 14,
  name_ = "Raw group summary plots",
  item_ = "raw_group_summary_plots"
  )
  return(raw_group_plots)
}


ltmp_choose_model <- function(dat) {
  status::status_try_catch(
  {
    is_first_min <- function(x) {
      if (all(is.na(x))) return(FALSE)
      seq_along(x) == which.min(x)
    }
    comp <- dat |>
      group_by(model_type, .add = TRUE) |> 
      mutate(SS = pmap(.l = list(raw_sum, posteriors, model_type),
                       .f = ~ {
                         model_type <- ..3
                         posts <- ..2$year_sum
                         if (is.null(posts)) return(NA)
                         raw <- ..1 ## |> 
                         ## mutate(Mean = ifelse(model_type == "Biomass", Mean_biomass, Mean))
                         raw |>
                           full_join(posts, by = "fYEAR") |>
                           summarise(SS = sum((Mean-median)^2, na.rm = TRUE))
                       }
                       )) |>
      unnest(SS) |>
      group_by(splits, .add = TRUE) |> 
      mutate(selected = is_first_min(SS)) |> 
      ungroup(model_type, splits)
    comp
  },
  stage_ = 4,
  order_ = 12,
  name_ = "Select best model",
  item_ = "select_model"
  )
  return(comp)
}

ltmp_delete_non_selected_models <- function(data) {
  status::status_try_catch(
  {
    file_str <- data |>
      ungroup() |> 
      dplyr::filter(!selected) |>
      dplyr::select(label) |>
      unnest(label) |>
      pull(label)
    ## Delete any unselected models and thier derivatives
    if (length(file_str) > 0) {
      file_str <- paste(paste0(file_str, ".*"), collapse =  "|")
      files <- list.files(
        path = paste0(
          status::get_setting("data_path"),
          "modelled/"
        ),
        pattern = file_str,
        )
      file.remove(
        paste0(
          status::get_setting("data_path"),
          "modelled/", files
        )
      )
    }
    ## Delete the selected model (not derivatives) as well
    files <- data |>
      ungroup() |>
      filter(selected) |>
      dplyr::select(model) |>
      unnest(model) |>
      pull(model)
    file.remove(files)
  },
  stage_ = 4,
  order_ = 20,
  name_ = "Delete excess models",
  item_ = "delete_models"
  )
  return(invisible(NULL))
}

## Juvenile specific functions

ltmp_get_formula_juv <- function(data) {
  status::status_try_catch(
  {
    model.hierarchy = list(
      GBR = formula(~f(nNRM_YEAR, model='iid') + f(nREEF_YEAR, model='iid')+
                    f(SITE_NO, model='iid') + offset(log(AVAILABLE_SUBSTRATE))),
      Zones = formula(~f(REEF, model='iid') + f(SITE_NO, model='iid') + offset(log(AVAILABLE_SUBSTRATE))),
      nrm = formula(~f(REEF, model='iid') + f(SITE_NO, model='iid') + offset(log(AVAILABLE_SUBSTRATE))),
      Bioregions = formula(~f(REEF, model='iid') + f(SITE_NO, model='iid') + offset(log(AVAILABLE_SUBSTRATE))),
      Sectors = formula(~f(REEF, model='iid') + f(SITE_NO, model='iid') + offset(log(AVAILABLE_SUBSTRATE))),
      reef = formula(~f(SITE_NO, model='iid') + offset(log(AVAILABLE_SUBSTRATE)))
    )
    data <- data |> 
      mutate(form =  map(status::get_setting(element = "data_scale"),
                         .f = ~ {
                           form <- model.hierarchy[[.x]]
                           update.formula(form, ABUNDANCE ~.+fYEAR)
                         }
                         ))
    data
  },
  stage_ = 4,
  order_ = 2,
  name_ = "Make juvenile formula",
  item_ = "make_formula_juv"
  )
  return(data)
}

ltmp_raw_data_summaries_juv <- function(dat) {
  status::status_try_catch(
  {
    dat <- dat |>
      mutate(raw_sum = map(.x = data_group,
                           .f =  ~ {
                             data_group <- .x
                             data_group |> 
                               mutate(cover = ABUNDANCE / AVAILABLE_SUBSTRATE) |> 
                               group_by(REEF, REEF_ZONE, fDEPTH, SITE_NO, fYEAR) |>
                               summarise(cover = sum(cover)) |> 
                               ungroup() |> 
                               group_by(REEF, fYEAR) |> 
                               summarise(Mean = mean(cover),
                                         Median = median(cover)) |> 
                               ungroup() |> 
                               group_by(fYEAR) |> 
                               summarise(Mean = mean(Mean),
                                         Median = median(Median)) |> 
                               as.data.frame() |>
                               suppressMessages() |>
                               suppressWarnings()
                           }
                           ))
    dat
  },
  stage_ = 4,
  order_ = 9,
  name_ = "Raw summaries",
  item_ = "raw_sum_juv"
  )
  return(dat)
}

ltmp_fit_inla_juv <- function(dat) {
  status::status_try_catch(
  {
    dat <- dat |>
      crossing(model_type = c("ZIP", "ZINB")) |> 
      mutate(model = pmap(.l = list(data_group, newdata, form, label, model_type),
                          .f = ~ ltmp__fit_inla_juv(..1, ..2, ..3, ..4, type = ..5)
                          ))
    dat
  },
  stage_ = 4,
  order_ = 11,
  name_ = "Fit models",
  item_ = "fit_models_juv"
  )
  return(dat)
}

ltmp__fit_inla_juv <- function(dat, newdata, form, label, type = "binomial") {
  label <- paste0(DATA_PATH, "modelled/", label, "_", type, ".rds")
  print(label)
  environment(form) <- new.env()
  data_pred <- dat |>
    bind_rows(newdata) 
  ## There are numerous instances where juvenile corals were not observed at
  ## all in any transects within the spatial domain for a year (missing cells)
  ## This leads to modelling instability.  The original solution was to
  ## remove these combinations prior to modelling and then add them back
  ## in as zeros during the predition stage.  This does however cause issues
  ## when comparing juvenile densities to such years (division by zero).
  ## 
  ## So, the current solution is to define strong priors for the offending
  ## combinations, and vague priors on all others. Unfortunately, this adds
  ## a complication that the model needs to be fit using a cellmeans
  ## parameterisation rather than with treatment contrasts.  Hence, it is
  ## necessary to convert the formula to a cellmeans formula and work
  ## out which combinations to apply the stronger priors...
  form <- convert_to_cellmeans(form)
  priors <- make_strong_priors(dat)
  
  if (type == "ZIP") {
    set.seed(123)
    mod.inla <- try({
      inla(form,
           data=data_pred,
           family='zeroinflatedpoisson1',
           control.family=list(link='log'),
           control.predictor=list(link=1, compute=TRUE),
           control.fixed = list(mean = priors$prior_mean, prec = priors$prior_prec),
           control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
           )
    }, silent = TRUE
    )
  }
  if (type == "ZINB") {
    set.seed(123)
    mod.inla <- try({
      inla(form,
           data=data_pred,
           family='zeroinflatednbinomial1',
           control.family=list(link='log'),
           control.predictor=list(link=1, compute=TRUE),
           control.fixed = list(mean = priors$prior_mean, prec = priors$prior_prec),
           control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
           )
    }, silent = TRUE
    )
  }
  if (inherits(mod.inla, "try-error")) {
    return("")
  }else {
    saveRDS(mod.inla, file = label)
    return(label)
  }
}

convert_to_cellmeans <- function(formula) {
  terms <- terms(formula)
  orders <- attr(terms, "order")
  term.labels <- attr(terms, "term.labels")
  wch <- str_detect(term.labels, "^f\\(")
  terms <- terms[!wch]
  orders <- orders[!wch]
  term.labels <- term.labels[!wch]
  if (max(orders) > 1) {
    formula <- update.formula(
      formula,
      paste(".~.-", paste0(term.labels[orders == 1], collapse = "-"), "+ 0")
    )
  } else {
    formula <- update.formula(formula, paste(".~.-1", collapse = ""))
  }
  return(formula)
}

make_strong_priors <- function(dat, VAR = "ABUNDANCE") {
  zero_abundances <- dat |> 
    group_by(fYEAR, fGROUP, .drop=FALSE) |> 
    ## summarise(Sum = sum(ABUNDANCE, na.rm=TRUE)) |> 
    summarise(Sum = sum(!!sym(VAR), na.rm=TRUE)) |> 
    filter(Sum == 0) |> 
    ungroup() |>
    suppressWarnings() |>
    suppressMessages()
  if (length(unique(dat$fGROUP)) == 1) {
    prior_terms <- zero_abundances |>
      mutate(term = paste0("fYEAR", fYEAR)) |>
      pull(term)
  } else {
    prior_terms <- zero_abundances |>
      mutate(term = paste0("fYEAR", fYEAR, ":fGROUP", fGROUP)) |>
      pull(term)
  }
  prior_mean <- lapply(prior_terms, function(x) log(0.001))
  prior_mean <- append(prior_mean, 0)
  names(prior_mean) <- c(prior_terms, "default")
  prior_prec <- lapply(prior_terms, function(x) 1)
  prior_prec <- append(prior_prec, 0.000001)
  names(prior_prec) <- c(prior_terms, "default")

  list(prior_mean = prior_mean, prior_prec = prior_prec)
}

## Manta specific functions

ltmp_get_formula_manta <- function(data) {
  status::status_try_catch(
  {
    model.hierarchy = list(
      GBR = formula(~f(NRM_region, model = 'iid') +
                      f(REEF, model = 'iid') +
                      f(REEF_YEAR, model = 'iid')),
      Zones = formula(~f(REEF, model = 'iid') +
                        f(REEF_YEAR, model = 'iid')),
      nrm = formula(~f(REEF, model = 'iid') +
                      f(REEF_YEAR, model = 'iid')),
      Bioregions = formula(~f(REEF, model = 'iid') +
                             f(REEF_YEAR, model = 'iid')),
      Sectors = formula(~f(REEF, model = 'iid') +
                          f(REEF_YEAR, model = 'iid')),
      reef = formula(~1)
    )
    data <- data |> 
      mutate(form =  map(model_response,
                         .f = ~ {
                           data_scale <- status::get_setting(element = "data_scale")
                           form <- model.hierarchy[[data_scale]]
                           update.formula(form, paste0(.x, "~.+fYEAR"))
                         }
                         ))
    ## data <- data |> 
    ##   mutate(form =  map(status::get_setting(element = "data_scale"),
    ##                      .f = ~ {
    ##                        form <- model.hierarchy[[.x]]
    ##                        update.formula(form, Cover ~.+fYEAR)
    ##                      }
    ##                      ))
    data
  },
  stage_ = 4,
  order_ = 2,
  name_ = "Make manta formula",
  item_ = "make_formula_manta"
  )
  return(data)
}

ltmp_add_group_depth_zone_manta <- function(data) {
  status::status_try_catch(
  {
    depth_col <- c(fGROUP = "HC", fDEPTH=NA, REEF_ZONE=NA)
    data <-
      data |>
      mutate(data = map(.x = data,
                        .f =  ~ {
                          dat <- .x
                          dat |> 
                            ## add_column(!!!depth_col[!names(depth_col) %in% names(dat)]) 
                            mutate(fGROUP = "HC", fDEPTH = NA, REEF_ZONE = NA)
                          }
                        )
             )
    data
  },
  stage_ = 4,
  order_ = 3,
  name_ = "Add group, depth, zone",
  item_ = "add_group_depth_zone_manta"
  )
  return(data)
}

ltmp_raw_data_summaries_manta <- function(dat) {
  status::status_try_catch(
  {
    dat <- dat |>
      mutate(raw_sum = map(.x = data_group,
                           .f =  ~ {
                             data_group <- .x
                             data_group |> 
                               group_by(REEF, fYEAR) |> 
                               summarise(Mean = mean(Cover),
                                         Median = median(Cover)) |> 
                               ungroup() |> 
                               group_by(fYEAR) |> 
                               summarise(Mean = mean(Mean),
                                         Median = median(Median)) |> 
                               as.data.frame() |>
                               suppressMessages() |>
                               suppressWarnings()
                           }
                           ))
    dat
  },
  stage_ = 4,
  order_ = 9,
  name_ = "Raw summaries",
  item_ = "raw_sum_manta"
  )
  return(dat)
}

ltmp_fit_inla_manta <- function(dat) {
  status::status_try_catch(
  {
    dat <- dat |>
      ## crossing(model_type = c("beta")) |> 
      mutate(model = pmap(.l = list(data_group, newdata, form, label, family_type),
                          .f = ~ {
                            data_group <- ..1 |>
                              mutate(Cover = ifelse(Cover == 0, 0.001, ifelse(Cover == 1, 0.999, Cover)))
                            ltmp__fit_inla_manta(data_group, ..2, ..3, ..4, type = ..5)
                          }
                          ))
    dat
  },
  stage_ = 4,
  order_ = 11,
  name_ = "Fit models",
  item_ = "fit_models_manta"
  )
  return(dat)
}

ltmp__fit_inla_manta <- function(dat, newdata, form, label, type = "beta") {
  ## label <- paste0(DATA_PATH, "modelled/", label, "_", type, ".rds")
  label <- paste0(DATA_PATH, "modelled/", label, ".rds")
  print(label)
  environment(form) <- new.env()
  data_pred <- dat |>
    bind_rows(newdata) 
  if (type == "beta") {
    set.seed(123)
    mod.inla <- try({
      inla(form,
           data=data_pred,
           family='beta',
           control.family=list(link='logit'),
           control.predictor=list(link=1, compute=TRUE),
           control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config=TRUE)
           )
    }, silent = TRUE
    )
  }
  if (inherits(mod.inla, "try-error")) {
    return("")
  }else {
    saveRDS(mod.inla, file = label)
    return(label)
  }
}

## Fish specific functions

ltmp_get_formula_fish <- function(data) {
  status::status_try_catch(
  {
    model.hierarchy = list(
      GBR=formula(~f(nNRM_YEAR, model='iid')+f(nREEF_YEAR, model='iid')+
                    f(SITE_NO, model='iid')+f(TRANSECT_NO, model='iid')),
      Zones=formula(~f(REEF, model='iid')+f(SITE_NO, model='iid')+f(TRANSECT_NO, model='iid')),
      nrm=formula(~f(REEF, model='iid')+f(SITE_NO, model='iid')+f(TRANSECT_NO, model='iid')),
      Bioregions=formula(~f(REEF, model='iid')+f(SITE_NO, model='iid')+f(TRANSECT_NO, model='iid')),
      Sectors=formula(~f(REEF, model='iid')+f(SITE_NO, model='iid')+f(TRANSECT_NO, model='iid')),
      reef=formula(~f(SITE_NO, model='iid')+f(TRANSECT_NO, model='iid'))
    )
    data <- data |> 
      mutate(form =  map(model_response,
                         .f = ~ {
                           data_scale <- status::get_setting(element = "data_scale")
                           form <- model.hierarchy[[data_scale]]
                           ## update.formula(form, COUNT ~.+fYEAR)
                           update.formula(form, paste0(.x, "~.+fYEAR"))
                         }
                         ))
    ## data <- data |> 
    ##   mutate(form =  map(status::get_setting(element = "data_scale"),
    ##                      .f = ~ {
    ##                        form <- model.hierarchy[[.x]]
    ##                        update.formula(form, ABUNDANCE ~.+fYEAR)
    ##                      }
    ##                      ))
    data
  },
  stage_ = 4,
  order_ = 2,
  name_ = "Make fish formula",
  item_ = "make_formula_fish"
  )
  return(data)
}

ltmp_raw_data_summaries_fish <- function(dat) {
  status::status_try_catch(
  {
    dat <- dat |>
      mutate(raw_sum = map2(.x = data_group, .y = model_response,
                            .f =  ~ {
                              data_group <- .x
                              model_response <- .y
                              data_group |> 
                                mutate(value = get(model_response)) |> 
                                group_by(REEF, REEF_ZONE, fDEPTH, SITE_NO, TRANSECT_NO, fYEAR) |>
                                summarise(value = sum(value)) |> 
                                ungroup() |> 
                                group_by(REEF, fYEAR) |> 
                                summarise(Mean = mean(value),
                                          Median = median(value)
                                          ) |>
                                ungroup() |> 
                                group_by(fYEAR) |> 
                                summarise(Mean = mean(Mean),
                                          Median = median(Median)
                                          ) |>
                                as.data.frame()
                            }
                            ))
    dat
  },
  stage_ = 4,
  order_ = 9,
  name_ = "Raw summaries",
  item_ = "raw_sum_fish"
  )
  return(dat)
}

ltmp_fit_inla_fish <- function(dat) {
  status::status_try_catch(
  {
    ## lookup <- tribble(~VARIABLE, ~sub_variable,
    ##                   "Coral Trout", "ABUNDANCE",
    ##                   "Coral Trout", "Biomass",
    ##                   "Secondary targets", "ABUNDANCE",
    ##                   "Secondary targets", "Biomass",
    ##                   "Large fishes", "ABUNDANCE",
    ##                   "Damselfishes", "ABUNDANCE",
    ##                   "Harvested", "ABUNDANCE",
    ##                   "Herbivores", "ABUNDANCE",
    ##                   "Total fishes", "ABUNDANCE"
    ##                   ) |>
    ##   full_join(
    ##     tribble(~sub_variable, ~model_type,
    ##             "Biomass", "tweedie",
    ##             "ABUNDANCE", "zeroinflatednbinomial1",
    ##             "ABUNDANCE", "nbinomial",
    ##             "ABUNDANCE", "zeroinflatedpoisson0",
    ##             "ABUNDANCE", "poisson"
    ##             )
    ##     )
    dat <- dat |>
      ## full_join(lookup) |> 
      ## crossing(model_type = c("zeroinflatednbinomial1", "nbinomial",
      ##                         "zeroinflatedpoisson0", "poisson")) |> 
      mutate(model = pmap(.l = list(data_group, newdata, form, label, family_type),
                          .f = ~ ltmp__fit_inla_fish(..1, ..2, ..3, ..4, family_type = ..5)
                          ))
    dat
  },
  stage_ = 4,
  order_ = 11,
  name_ = "Fit models",
  item_ = "fit_models_fish"
  )
  return(dat)
}

ltmp__fit_inla_fish <- function(dat, newdata, form, label, family_type = "deltagamma") {
  ## ensure that there are non-na response values in the data
  resp <- form[[2]]
  if (length(na.omit(dat[[resp]])) == 0) return("")

  ## label <- paste0(DATA_PATH, "modelled/", label, "_", type, "_", sub_variable, ".rds")
  label <- paste0(DATA_PATH, "modelled/", label, ".rds")
  
  print(label)
  environment(form) <- new.env()
  data_pred <- dat |>
    bind_rows(newdata) 
  ## There are numerous instances where juvenile corals were not observed at
  ## all in any transects within the spatial domain for a year (missing cells)
  ## This leads to modelling instability.  The original solution was to
  ## remove these combinations prior to modelling and then add them back
  ## in as zeros during the predition stage.  This does however cause issues
  ## when comparing juvenile densities to such years (division by zero).
  ## 
  ## So, the current solution is to define strong priors for the offending
  ## combinations, and vague priors on all others. Unfortunately, this adds
  ## a complication that the model needs to be fit using a cellmeans
  ## parameterisation rather than with treatment contrasts.  Hence, it is
  ## necessary to convert the formula to a cellmeans formula and work
  ## out which combinations to apply the stronger priors...
  form <- convert_to_cellmeans(form)
  priors <- make_strong_priors(dat)
  if (family_type == "gamma") { # Biomass models
    priors <- make_strong_priors(dat, VAR = "Biomass")
    data_pred <- dat |>
      mutate(Biomass = Biomass + min(Biomass[Biomass>0])/10) |> 
      bind_rows(newdata) 
  }
  ## if (type == "binomial") {
  ## if (family_type == "tweedie") inla.setOption(inla.timeout = 60)

  ## if (family_type != "deltagamma") {  # Abundance/density models
    ## Start 
    set.seed(123)
    mod.inla <- try({
      inla(form,
           data = data_pred,
           family = family_type,
           control.family = list(link="log"),
           control.predictor = list(link = 1, compute = TRUE),
           control.fixed = list(mean = priors$prior_mean, prec = priors$prior_prec),
           control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE),
           ## silent = TRUE
           silent = 1L,
           verbose = FALSE
           ) 
    }, silent = TRUE
    )
    ## inla.setOption(inla.timeout = 0)
    ## }
    if (inherits(mod.inla, "try-error")) {
      return("")
    }else {
      saveRDS(mod.inla, file = label)
      return(label)
    }
  }

  ## if (family_type == "deltagamma") {  # Biomass models
  ##   ## Biomass is the product of two processes 1) a binomial process that
  ##   ## determines whether the fish are observed and 2) a positive
  ##   ## continuous process based on density and fish size. This makes it
  ##   ## awkward to model.extract. The traditional approach is called the
  ##   ## Delta-gamma model in which separate binomial and gamma models are
  ##   ## respectively fitted to bernoulli and zero exclusion version of the
  ##   ## response and the posteriors are combined.

  ##   ## Start with binomial 
  ##   data_pred  <- dat |> mutate(Biomass = as.numeric(Biomass > 0)) |> 
  ##     bind_rows(newdata) 
  ##   priors <- make_strong_priors(dat |> mutate(Biomass = as.numeric(Biomass > 0)), VAR = "Biomass")
  ##   set.seed(123)
  ##   mod.inla.b <- try({
  ##     inla(form,
  ##          data = data_pred,
  ##          family = "binomial",
  ##          control.family = list(link="logit"),
  ##          control.predictor = list(link = NA, compute = TRUE),
  ##          ## control.predictor = list(link = 1, compute = TRUE),
  ##          ## control.fixed = list(mean = priors$prior_mean, prec = priors$prior_prec),
  ##          control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE),
  ##          ## silent = TRUE
  ##          silent = 1L,
  ##          verbose = FALSE
  ##          ) 
  ##   }, silent = TRUE
  ##   )
  ##   ## inla.setOption(inla.timeout = 0)
  ##   ## }
  ##   if (inherits(mod.inla.b, "try-error")) {
  ##     return("")
  ##   }else {
  ##     ## Start with binomial 
  ##     set.seed(123)
  ##     data_pred <- dat |> filter(Biomass != 0) |> bind_rows(newdata)
  ##     priors <- make_strong_priors(dat |> filter(Biomass != 0), VAR = "Biomass") 
  ##     mod.inla.g <- try({
  ##       inla(form,
  ##            data = data_pred, 
  ##            family = "gamma",
  ##            control.family = list(link="log"),
  ##            control.predictor = list(link = 1, compute = TRUE),
  ##            control.fixed = list(mean = priors$prior_mean, prec = priors$prior_prec),
  ##            control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE),
  ##            ## silent = TRUE
  ##            silent = 1L,
  ##            verbose = FALSE
  ##            ) 
  ##     }, silent = TRUE
  ##     )

  ##     saveRDS(mod.inla.b, file = label)
  ##     return(label)
  ##   }
  ## }

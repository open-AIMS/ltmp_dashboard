ltmp_load_processed_data_pt <- function() {
  ## Common functions
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
      mutate(form =  map(status::get_setting(element = "data_scale"),
                         .f = ~ {
                           form <- model.hierarchy[[.x]]
                           update.formula(form, COUNT ~.+fYEAR)
                         }
                         ))
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
                                      data_type <- status::get_setting(element = "data_method")
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
                                      if (data_scale == "GBR") splits$SPLIT_SHELF <- FALSE
                                      if (data_scale == "Sectors" & data_type == "manta") splits$SPLIT_SHELF <- FALSE
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
  new_fterms <- drop.terms(fterms, dropx = idx, keep.response = TRUE)
  return(formula(new_fterms))
}

ltmp_prepare_variables <- function(dat, VAR = "COUNT") {
  status::status_try_catch(
  {
    dat <- 
      dat |>
      mutate(data_group =
               map(.x = data_group,
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
                         summarise(Sum=sum(!!sym(VAR), na.rm=TRUE)) |>
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
      mutate(label = pmap(.l = list(VARIABLE, splits),
                          .f =  ~ paste(
                                status::get_setting(element = "data_method"),
                                status::get_setting(element = "data_scale"),
                                status::get_setting(element = "domain_name"),
                                ..1,
                                ..2,
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
      crossing(model_type = c("binomial", "beta-binomial")) |> 
      mutate(model = pmap(.l = list(data_group, newdata, form, label, model_type),
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
  label <- paste0(DATA_PATH, "modelled/", label, "_", type, ".rds")
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
  dat <- dat |>
    mutate(posteriors = map2(.x = data_group, .y = model,
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

  cellmeans <- sapply(draws, function(x) x[[2]][(nd + 1):nt])
  year_group_posteriors <- get_year_group_posteriors(newdata, cellmeans, replace_0)
  year_group_posteriors_label <- gsub(".rds$", "_year_group_posteriors.rds", mod_str)
  saveRDS(year_group_posteriors, file = year_group_posteriors_label)

  year_group_sum <- year_group_posteriors |>
    group_by(fYEAR, fGROUP, REPORT_YEAR, DATE) %>%
    mean_median_hdci(value) %>%
    ungroup()
  saveRDS(year_group_sum, file = gsub("posteriors", "sum", year_group_posteriors_label))

  ## marginalise over fGROUP
  year_posteriors <- get_year_posteriors(newdata, cellmeans, replace_0)
  year_posteriors_label <- gsub(".rds$", "_year_posteriors.rds", mod_str)
  saveRDS(year_posteriors, file = year_posteriors_label)
  write_csv(year_posteriors |> dplyr::select(-fYEAR),
            file = gsub(".rds", ".csv", gsub("_year_", "_annual_", year_posteriors_label)))

  year_sum <- year_posteriors %>%
    group_by(fYEAR, REPORT_YEAR, DATE) %>%
    mean_median_hdci(value) %>%
    mutate(upper = ifelse(upper > 1, 1, upper))
  saveRDS(year_sum, file = gsub("posteriors", "sum", year_posteriors_label))

  ## year comparisons
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

  return(list(year_group_posteriors = year_group_posteriors_label, #year_group_posteriors,
         year_group_sum = year_group_sum,
         year_posteriors = year_posteriors_label, #year_posteriors,
         year_sum = year_sum,
         yearcomp_posteriors = yearcomp_posteriors_label, #yearcomp_posteriors,
         yearcomp_sum = yearcomp_sum)
  )
}
get_year_group_posteriors <- function(newdata, cellmeans, replace_0) {
  newdata <- newdata |>
    dplyr::select(fYEAR, fGROUP, REPORT_YEAR, DATE) |>
    cbind(plogis(cellmeans)) %>%
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
get_year_posteriors <- function(newdata, cellmeans, replace_0) {
  newdata <- newdata |>
    dplyr::select(fYEAR,fGROUP, REPORT_YEAR, DATE) %>%
    cbind(plogis(cellmeans)) %>%
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

ltmp_compare_models <- function(dat) {
  status::status_try_catch(
  {
    dat_compare <-
      dat |>
      mutate(compare_models =
               map2(.x = posteriors, .y = model_type,
                    .f = ~ {
                      if (is.null(.x)) return(NULL)
                      .x$year_sum |>
                        mutate(model_type = .y)
                    }
                    )) |>
      dplyr::select(VARIABLE, splits, label, raw_sum, compare_models) |>
      unnest(c(compare_models)) |> 
      group_by(VARIABLE, splits, label, raw_sum) |>
      nest() |>
      mutate(gg = pmap(.l = list(data, raw_sum, label),
                       .f =  ~ {
                         data <- ..1
                         raw_sum <- ..2
                         lab <- ..3
                         gg <-
                           data |> ggplot(aes(x = REPORT_YEAR, y = median)) +
                           geom_ribbon(aes(ymin = lower, ymax = upper),
                                       fill = "orange", alpha = 0.3) +
                           geom_line(aes(group = model_type, colour = model_type)) +
                           geom_point()  +
                           geom_line(data = raw_sum,
                                     aes(y = Mean, x = as.numeric(as.character(fYEAR)),
                                         colour = "Raw mean")) +
                           geom_line(data = raw_sum,
                                     aes(y = Median, x = as.numeric(as.character(fYEAR)),
                                         colour = "Raw median")) +
                           facet_grid(~model_type) +
                           ggtitle(str_replace_all(lab, "_", " ")) +
                           scale_y_continuous("Cover", label = scales::label_percent()) +
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

ltmp_group_compare_models <- function(dat) {
  status::status_try_catch(
  {
    if (status::get_setting(element = "data_scale") == "reef") {
    dat_group_compare <-
      dat |>
      mutate(compare_models =
               map2(.x = posteriors, .y = model_type,
                    .f = ~ {
                      if (is.null(.x)) return(NULL)
                      .x$year_group_sum |>
                        mutate(model_type = .y)
                    }
                    )) |>
      dplyr::select(VARIABLE, splits, label, data_group, compare_models) |>
      unnest(c(compare_models)) |> 
      group_by(VARIABLE, splits, label, data_group) |>
      nest() |>
      mutate(gg = pmap(.l = list(data, data_group, label),
                       .f =  ~ {
                         data <- ..1
                         data_group <- ..2
                         lab <- ..3
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
                           facet_grid(~model_type) +
                           ggtitle(str_replace_all(lab, "_", " ")) +
                           scale_y_continuous("Cover", label = scales::label_percent()) +
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

ltmp_raw_summary_plots <- function(dat) {
  status::status_try_catch(
  {
    raw_plots <-
      dat |>
      mutate(gg =
               pmap(.l = list(data_group, raw_sum, label),
                    .f = ~ {
                      data_group <- ..1
                      raw_sum <- ..2
                      lab <- ..3
                      if (status::get_setting(element = "data_method") %in%
                          c("photo-transect", "juvenile")) {
                        gg <- 
                          data_group |> 
                          group_by(REPORT_YEAR, REEF, fDEPTH, TRANSECT_NO) |>
                          summarise(PERC_COVER = sum(PERC_COVER)) |>
                          ungroup() |> 
                          ggplot(aes(x = REPORT_YEAR, y = PERC_COVER)) +
                          geom_line(aes(color = paste0(fDEPTH, TRANSECT_NO)),
                                    show.legend = FALSE) +
                          geom_line(data = raw_sum,
                                    aes(y = Mean,
                                        x = as.numeric(as.character(fYEAR)))) +
                          geom_line(data = raw_sum,
                                    aes(y = Median,
                                        x = as.numeric(as.character(fYEAR))),
                                    linetype = "dashed") +
                          facet_wrap(~REEF) +
                          theme_bw()
                      } else {
                        gg <- 
                          data_group |> 
                          group_by(REPORT_YEAR, REEF, fDEPTH) |>
                          summarise(PERC_COVER = mean(Cover)) |>
                          ungroup() |> 
                          ggplot(aes(x = REPORT_YEAR, y = PERC_COVER)) +
                          geom_line(aes(color = paste0(fDEPTH)),
                                    show.legend = FALSE) +
                          geom_line(data = raw_sum,
                                    aes(y = Mean,
                                        x = as.numeric(as.character(fYEAR)))) +
                          geom_line(data = raw_sum,
                                    aes(y = Median,
                                        x = as.numeric(as.character(fYEAR))),
                                    linetype = "dashed") +
                          facet_wrap(~REEF) +
                          theme_bw()
                      }
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

ltmp_raw_group_summary_plots <- function(dat) {
  status::status_try_catch(
  {
    if (status::get_setting(element = "data_scale") == "reef") {
      raw_group_plots <-
        dat |>
        mutate(gg =
                 pmap(.l = list(data_group, raw_sum, label),
                      .f = ~ {
                        data_group <- ..1
                        raw_sum <- ..2
                        lab <- ..3
                        gg <- 
                          data_group |> 
                          ggplot(aes(x = REPORT_YEAR, y = PERC_COVER)) +
                          geom_line(aes(color = paste0(fDEPTH, TRANSECT_NO)),
                                    show.legend = FALSE) +
                          geom_line(data = raw_sum,
                                    aes(y = Mean,
                                        x = as.numeric(as.character(fYEAR)))) +
                          geom_line(data = raw_sum,
                                    aes(y = Median,
                                        x = as.numeric(as.character(fYEAR))),
                                    linetype = "dashed") +
                          facet_wrap(~fGROUP) +
                          theme_bw()
                        filenm <- paste0(FIGURES_PATH, "", "gg_raw_group_sum_", lab, ".png")
                        saveRDS(gg, file = paste0(FIGURES_PATH, "", "gg_raw_group_sum_", lab, ".rds"))
                        ggsave(filename = filenm, plot = gg, width = 12, height = 7)
                        filenm
                      }
                      ))
    } else {
      raw_group_plots <- dat |> mutate(gg = "")
    }
  },
  stage_ = 4,
  order_ = 14,
  name_ = "Raw group summary plots",
  item_ = "raw_group_summary_plots"
  )
  return(raw_group_plots)
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

make_strong_priors <- function(dat) {
  zero_abundances <- dat |> 
    group_by(fYEAR, fGROUP, .drop=FALSE) |> 
    summarise(Sum = sum(ABUNDANCE, na.rm=TRUE)) |> 
    filter(Sum == 0) |> 
    ungroup()
  
  prior_terms <- zero_abundances |>
    mutate(term = paste0("fYEAR", fYEAR, ":fGROUP", fGROUP)) |>
    pull(term)
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
      mutate(form =  map(status::get_setting(element = "data_scale"),
                         .f = ~ {
                           form <- model.hierarchy[[.x]]
                           update.formula(form, Cover ~.+fYEAR)
                         }
                         ))
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
      crossing(model_type = c("beta")) |> 
      mutate(model = pmap(.l = list(data_group, newdata, form, label, model_type),
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
  label <- paste0(DATA_PATH, "modelled/", label, "_", type, ".rds")
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

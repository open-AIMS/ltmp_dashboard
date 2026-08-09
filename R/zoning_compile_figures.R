cat("\nInside zoning_compile_figures.R\n=============================================\n")

cat("The current working directory is: ", getwd(), "\n\n")

args <- commandArgs()

## Fake args for testing
if (length(args) < 3)  { ## clearly being run interactively
  args <- c(
    "--cwd=/home/Project/R/",
    "--domain=compile",
    "--method=fish",
    "--scale=sector",
    "--data_path=/data/", #/export/project/monitoring-dashboard/data/",
    "--output_path=/output/",
    "--path=/data/",
    "--refit=TRUE",
    "--log=/export/project/monitoring-dashboard/data/log/"
  )


  args <- c(
    "--cwd=/home/Project/R/",
    "--domain=contrasts",
    "--method=fish",
    "--scale=sector",
    "--data_path=/data/", #/export/project/monitoring-dashboard/data/",
    "--output_path=/output/",
    "--path=/data/",
    "--refit=TRUE",
    "--log=/export/project/monitoring-dashboard/data/log/"
  )
}

cat(args)


has_cwd_argument <- any(grepl("--cwd=.*", args, perl = TRUE))
if(has_cwd_argument) {
  arg <- args[grep("--cwd=.*", args)]
  cwd <- gsub("--cwd=(.)", "\\1", arg)
  setwd(cwd)
} else stop("no --cwd= component provided")
cat("\nNew working directory is: ", getwd(), "\n")

source("zoning_compile_figures_functions.R")

zoning_parse_args(args)

config_ <- list(
  dashboard_log = log_file,
  model_path = paste0(data_path, "modelled")
  )

cat("Load libraries\n")
library(tidyverse)
library(ggdist)


## compile the posterior summaries
cat("Get the names of the relevant files")
## fls <- list.files(path = paste0(data_path, "modelled/"),
##   pattern = "*._Sectors_[^_]*.rds", full.names = FALSE)
fls <- list.files(path = paste0(data_path, "modelled/"),
  pattern = "*._Sectors_.*Zones_year_sum.rds", full.names = FALSE)
## cat(paste("-", fls), sep = "\n")



data <- data.frame(fls = fls,
  file = paste0(data_path, "modelled/",fls)) |>
  separate_wider_delim(fls,
    delim = "_",
    ## names_sep = "_"#,
    names = c(
      "data_method",
      "data_type",
      "domain",
      "group",
      "family",
      "junk_1",
      "junk_2",
      "junk_3",
      ## "junk_4",
      "response",
      "sub_model",
      "zoning",
      "extra_1",
      "extra_2"
    ),
    cols_remove = FALSE) |>
  dplyr::select(-starts_with("junk"), -starts_with("extra")) |>
  mutate(
    year_sum = map(file, .f = ~ readRDS(.x))
  )

## cat(print(data))


##Time series #########################################################
if (domain == "compile") {
  MPA_time_series_plots_prepare_data__ <- function(
                                                   data,
                                                   data_method = "fish",
                                                   group = "Coral Trout",
                                                   response = "Density",
                                                   sub_model = "restricted") {
    data_method_ <- data_method
    group_ <- group
    response_ <- response
    sub_model_ <- sub_model
    dat_ts <-
      data |>
      dplyr::filter(data_method == data_method_,
        group == group_,
        response == response_,
        sub_model == sub_model_) |>
      droplevels() |>
      dplyr::select(domain, year_sum) |>
      mutate(sector = factor(domain,
        levels = c("CG", "PC", "CA", "IN", "TO", "WH", "PO", "SW", "CB"),
        labels =  c("Cape Grenville", "Princess Charlotte Bay", "Cairns",
          "Innisfail", "Townsville", "Whitsundays", "Pompeys", "Swains",
          "Cap-Bunkers"))) |>
      unnest("year_sum")
    return(dat_ts)
  }


  MPA_time_series_plots_gg__ <- function(dat_ts, ytitle) {
    gg <- dat_ts |> ggplot(aes(y = mean, x = REPORT_YEAR,
      shape = fZONE, fill = fZONE, linetype = fZONE)) +
      geom_line(linewidth = 0.5) +
      geom_linerange(aes(ymin = lower, ymax = upper), linetype = 1, show.legend = FALSE) +
      geom_point(size = 1.5) +
      scale_fill_manual("Zone",
        breaks = c("C", "O"),
        labels = c("Reserve", "Non-reserve"),
        values = c("black", "white")) +
      scale_shape_manual("Zone",
        breaks = c("C", "O"),
        labels = c("Reserve", "Non-reserve"),
        values = c(21, 21)) +
      scale_linetype_manual("Zone",
        breaks = c("C", "O"),
        labels = c("Reserve", "Non-reserve"),
        values = c(1, 2)) +
      ## scale_y_continuous(.y, labels = scales::comma) +
      scale_y_continuous(ytitle, labels = function(x) scales::comma(x * 1)) +
      facet_wrap(~sector, nrow = 2, scales = "free_x", strip.position = "bottom") +
      theme_classic()  +
      theme(legend.position = c(1, 0.1), legend.justification = c(1, 0),
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(vjust = 1.5),
        axis.title.x = element_blank(),
        axis.ticks.length.x = unit(2.75, units = "points"),
        axis.minor.ticks.length.x = rel(0.75),
        panel.spacing = unit(1, unit = "lines"),
        ## axis.text.x = element_text(size = 7, colour = c("black", rep(NA, 4))),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        strip.placement = "outside"
      ) +
      guides(x = guide_axis(minor.ticks = TRUE))

    if (diff(range(dat_ts$REPORT_YEAR)) > 5) {
      gg <- gg +
        scale_x_continuous('',
          breaks = scales::breaks_width(5),
          minor_breaks = scales::breaks_width(1),
          limits = range(dat_ts$REPORT_YEAR),
          expand = c(0, 0.5))
    } else {
      scale_x_continuous('',
        breaks = scales::breaks_width(1),
        minor_breaks = scales::breaks_width(1),
        limits = range(dat_ts$REPORT_YEAR),
        expand = c(0, 0.5))
    }
    return(gg)
  }

  MPA_time_series_plots <- function(data) {
    data_lookup <- data |>
      dplyr::select(data_method, group, response, sub_model) |>
      distinct() |>
      rowwise() |>
      ## mutate(ytitle = list(parse(
      ##   text = paste(group, response, "(per~1000~m^2)") |>
      ##     str_replace_all(" ", "~") |> str_to_title()))
      ## ) |>
      mutate(ytitle = case_when(
        data_method == "fish" ~
          list(parse(
            text = paste(str_to_title(paste(group, response)), "(per~1000~m^2)") |>
              str_replace_all(" ", "~"))),
        data_method == "juveniles" ~
          list(parse(
            text = paste(str_to_title(paste("Juvenile", response)), "(per~1000~m^2)") |>
              str_replace_all(" ", "~"))),
        data_method == "photo-transect" ~
          list(parse(
            text = paste(str_to_title(paste(group, response)), "(per~1000~m^2)") |>
              str_replace_all(" ", "~")))
      )) |>
      mutate(label = paste0(paste("ts", data_method, group, response, sub_model, sep = "_"), ".png")) |>
      ungroup() |>
      mutate(dat_ts = pmap(.l = list(data_method, group, response, sub_model),
        .f =  ~ {
          data_method_ <- ..1
          group_ <- ..2
          response_ <- ..3
          sub_model_ <- ..4

          MPA_time_series_plots_prepare_data__(
            data,
            data_method = data_method_,
            group = group_,
            response = response_,
            sub_model = sub_model_
          )
        })) |>
  mutate(gg = map2(dat_ts, ytitle,
    .f =  ~ {
      MPA_time_series_plots_gg__(.x, .y)
    })
  )

    walk2(data_lookup$gg, data_lookup$label,
      .f =  ~ {
        ggsave(filename = paste0(output_path, "/figures/", .y), plot = .x, width = 10, height = 5)
      })
    data_lookup <- data_lookup |> dplyr::select(-dat_ts, -gg, -ytitle)
    return(data_lookup)
  }

  data_ts <- MPA_time_series_plots(data)
  saveRDS(data_ts, file = paste0(data_path, "modelled/", "zoning_timeseries.rds"))

}

##Contrasts - Open vs Closed ###########################################
if (domain == "contrasts") {
  cat("\nCompile the contrasts data\n")
  data <- data |>
    mutate(
      mpa_sum = map(file, .f =  ~ readRDS(str_replace(.x, "year_sum", "mpa_sum"))),
      mpa_posteriors = map(file, .f =  ~ readRDS(str_replace(.x, "year_sum", "mpa_posteriors")))
    )
  MPA_contrast_plots_prepare_data_bar__ <- function(data,
                                                response = "Density",
                                                sub_model = "restricted"#,
                                                ## fYEAR =  2026
                                                ) {
    response_ <- response
    sub_model_ <- sub_model
    ## fYEAR_<- fYEAR

    data_cntr <-
      data |>
      dplyr::filter(response == response_,
        sub_model == sub_model_) |>
      droplevels() |>
      dplyr::select(domain, group, mpa_sum) |>
      mutate(sector = factor(domain,
        levels = c("CG", "PC", "CA", "IN", "TO", "WH", "PO", "SW", "CB"),
        labels =  c("Cape Grenville", "Princess Charlotte Bay", "Cairns",
          "Innisfail", "Townsville", "Whitsundays", "Pompeys", "Swains",
          "Cap-Bunkers"))) |>
      unnest("mpa_sum") #|>
    ## filter(fYEAR == fYEAR_) |>
    ## droplevels()
  }

  MPA_contrast_plots_prepare_data_slab__ <- function(data,
                                                response = "Density",
                                                sub_model = "restricted"#,
                                                ) {
    response_ <- response
    sub_model_ <- sub_model

    data_cntr <-
      data |>
      dplyr::filter(response == response_,
        sub_model == sub_model_) |>
      droplevels() |>
      dplyr::select(domain, group, mpa_posteriors) |>
      mutate(sector = factor(domain,
        levels = c("CG", "PC", "CA", "IN", "TO", "WH", "PO", "SW", "CB"),
        labels =  c("Cape Grenville", "Princess Charlotte Bay", "Cairns",
          "Innisfail", "Townsville", "Whitsundays", "Pompeys", "Swains",
          "Cap-Bunkers"))) |>
      unnest("mpa_posteriors") #|>
    ## filter(fYEAR == fYEAR_) |>
    ## droplevels()
  }
  pretty_major_breaks <- function(limits) {
    major_breaks <- case_when(
      max(limits) > 12 ~ list(c(0.1, 0.4, 1, 3, 11)),
      max(limits) > 9 ~ list(c(0.125, 0.25, 0.5, 1, 2, 4, 8)),
      max(limits) > 1 ~ list(c(0.25, 0.5, 1, 2, 4)),
      max(limits) <= 1 ~ list(c(0.8, 0.9, 1, 1.1, 1.25)),
      .default =  list(1)
    )
    return(major_breaks[[1]])
  }
  pretty_minor_breaks <- function(limits) {
    ## minor_breaks <- case_when(
    ##   max(limits) > 12 ~ list(c(0.2, 0.5, 1, 2, 6)),
    ##   max(limits) > 9 ~ list(c(0.125, 0.25, 0.5, 1, 2, 4, 8)),
    ##   max(limits) < 5 ~ list(c(0.25, 0.5, 1, 2, 4)),
    ##   .default =  list(1)
    ## )
    minor_breaks <- seq(0, round(max(limits)), by = 1)

    return(c(1/minor_breaks, minor_breaks))
  }

  MPA_contrast_plots_gg_bar__ <- function(dat_cntr) {
    dat_cntr <- dat_cntr |>
      filter(variable == "frac") |>
      filter(!is.na(group), group != "NA") |>
      droplevels() |>
      mutate(diff = case_when(
        Pl > 0.90 ~ "lower",
        Pg > 0.90 ~ "higher",
        .default = "no diff")) |>
      mutate(diff = factor(diff))
    max_diff <- dat_cntr |>
      summarise(max = max(upper, na.rm = TRUE),
        min = min(lower, na.rm = TRUE)) |>
      mutate(max_diff = max(max, 1/min)) |>
      pull(max_diff)
    if (max_diff > 20) max_diff <- 20
    custom_colours <- c("higher" = "blue", "lower" = "red", "no diff" = "gray")
    gg <-
      dat_cntr |>
      ggplot(aes(y = group, x = median)) +
      geom_vline(xintercept = 1, linetype = "dashed") +
      geom_pointrange(aes(xmin = lower, xmax = upper, color = diff), size = 0.5,
        show.legend = TRUE) +
      scale_y_discrete('') +
      scale_x_continuous("Percentage difference (Reserve vs Non-reserve)",
        trans = scales::log2_trans(),
        ## label = function(x) 100*(x - 1),
        label = \(x) sprintf("%0.1f", 100*(x - 1)),
        limit = c(1/max_diff, max_diff),
        ## breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8)) +
        breaks = pretty_major_breaks,
        minor_breaks = pretty_minor_breaks
      ) +
      scale_colour_manual(
        name = "",
        ## breaks = c("higher", "lower", "no diff"),
        labels = c("higher", "lower", "not different"),
        values = custom_colours,
        limits = names(custom_colours),
        drop = FALSE) +
      facet_wrap(~sector, scales = "free_x", strip.position = "top") +
      theme_classic() +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(vjust = 1.5),
        ## axis.title.x = element_blank(),
        axis.ticks.length.x = unit(2.75, units = "points"),
        axis.minor.ticks.length.x = rel(0.75),
        panel.spacing = unit(1, unit = "lines"),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        strip.placement = "outside"
      ) +
      guides(x = guide_axis(minor.ticks = TRUE))
    ## guides(x = guide_axis(minor.ticks = TRUE),
    ##   colour = guide_legend(override.aes =
    ##                           list(colour = c("higher" = "green", "lower" = "red", "do diff" = "gray"))))
    return(gg)
  }

  MPA_contrast_plots_gg_slab__ <- function(dat_cntr) {
    dat_cntr <- dat_cntr |>
      filter(!is.na(group), group != "NA") |>
      droplevels() |>
      group_by(group, sector) |>
      mutate(Pl = mean(frac < 1),
        Pg = mean(frac > 1),
        evidence = Pl >= 0.9 | Pg >= 0.9
      ) |>
      ungroup()
      ## mutate(diff = case_when(
      ##   Pl > 0.90 ~ "lower",
      ##   Pg > 0.90 ~ "higher",
      ##   .default = "no diff")) |>
      ## mutate(diff = factor(diff))
    max_diff <- dat_cntr |>
      summarise(max = max(frac, na.rm = TRUE),
        min = min(frac, na.rm = TRUE)) |>
      mutate(max_diff = max(max, 1/min)) |>
      pull(max_diff)
    if (max_diff > 20) max_diff <- 20
    ## custom_colours <- c("higher" = "blue", "lower" = "red", "no diff" = "gray")
    gg <-
      dat_cntr |>
      ggplot(aes(y = group, x = frac, evidence = evidence)) +
      geom_vline(xintercept = 1, linetype = "dashed") +
      ggdist::stat_slab(
        aes(fill = after_stat(interaction(x > 1, evidence)),
          alpha = after_stat(interaction(x > 1, evidence))
        ),
        colour = "black",
        linewidth = 0.5,
        trim = TRUE,
        ## slab_alpha = 0.5,
        p_limits = c(0.025, 0.95)) +
      scale_y_discrete('') +
      scale_x_continuous("Percentage difference (Reserve vs Non-reserve)",
        trans = scales::log2_trans(),
        ## label = function(x) 100*(x - 1),
        label = \(x) sprintf("%0.1f", 100*(x - 1)),
        limit = c(1/max_diff, max_diff),
        ## breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8)) +
        breaks = pretty_major_breaks,
        minor_breaks = pretty_minor_breaks
      ) +
      scale_fill_manual(
        values = c(
          "FALSE.TRUE" = "red",
          "TRUE.TRUE" = "blue",
          "FALSE.FALSE" = "red",
          "TRUE.FALSE" = "blue"
        ), guide = "none"
        ) +
      scale_alpha_manual(
        values = c(
          "FALSE.TRUE" = 0.6,
          "TRUE.TRUE" = 0.6,
          "FALSE.FALSE" = 0.1,
          "TRUE.FALSE" = 0.1
        ),
        guide = "none"
        ) +
      facet_wrap(~sector, scales = "free_x", strip.position = "top") +
      theme_classic() +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(vjust = 1.5),
        axis.ticks.length.x = unit(2.75, units = "points"),
        axis.minor.ticks.length.x = rel(0.75),
        panel.spacing = unit(1, unit = "lines"),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        strip.placement = "outside"
      ) +
      guides(x = guide_axis(minor.ticks = TRUE))
    return(gg)
  }

  MPA_contrast_plots <- function(data, interval_type = "bar") {
    data_lookup <- data |>
      dplyr::select(response, sub_model) |>
      distinct() |>
      mutate(dat_contr = pmap(.l = list(response, sub_model),
        .f =  ~ {
          response_ <- ..1
          sub_model_ <- ..2
          if (interval_type == "bar") {
            dt <- MPA_contrast_plots_prepare_data_bar__(
              data,
              response = response_,
              sub_model = sub_model_
            )
          } else {
            dt <- MPA_contrast_plots_prepare_data_slab__(
              data,
              response = response_,
              sub_model = sub_model_
            )
          }
          return(dt)
        })) |>
      unnest("dat_contr") |>
      group_by(response, sub_model, fYEAR) |>
      nest(.key = "dat_contr") |>
      rowwise() |>
      mutate(label = paste0(paste("cntrs", response, sub_model, fYEAR, interval_type, sep = "_"), ".png")) |>
      ungroup() |>
      ## exclude those dat_contr that do not have data (because of fYEAR)
      mutate(N = map(dat_contr, .f = ~nrow(.x))) |>
      filter(N > 0) |>
      mutate(gg = map(dat_contr,
        .f =  ~ {
          if (interval_type == "bar") {
            gg <- MPA_contrast_plots_gg_bar__(.x)
          } else {
            gg <- MPA_contrast_plots_gg_slab__(.x)
          }
          return(gg)
        })
      ) |>
      mutate(interval_type = interval_type)

    walk2(data_lookup$gg, data_lookup$label,
      .f =  ~ {
        cat(paste("\t- generate", .y, "\n"))
        ggsave(filename = paste0(output_path, "/figures/", .y), plot = .x, width = 10, height = 5)
      })
    data_lookup <- data_lookup |> dplyr::select(-dat_contr, -gg, -N)
    return(data_lookup)
  }

  data_cntr <- MPA_contrast_plots(data, interval_type = "bar") |>
    bind_rows(MPA_contrast_plots(data, interval_type = "slab"))
  saveRDS(data_cntr, file = paste0(data_path, "modelled/", "zoning_contrasts.rds"))

  cat(paste("Finished generating MPA Zoning contrast figures\n"))

}

if (domain == "contrasts_sector") {
  cat("\nCompile the sector contrasts data\n")
  data <- data |>
    mutate(
      mpa_sum = map(file, .f =  ~ readRDS(str_replace(.x, "year_sum", "mpa_sum"))),
      mpa_posteriors = map(file, .f =  ~ readRDS(str_replace(.x, "year_sum", "mpa_posteriors")))
    )
  cat("\nAggregate GBR wide posteriors\n")
  data_total <- data |>
    dplyr::select(-fls, -file, -year_sum, -mpa_sum) |>
    unnest(mpa_posteriors) |>
    group_by(data_method, data_type, response, group, sub_model, zoning) |>
    nest() |>
    mutate(mpa_posteriors = map(.x = data,
      .f = ~ {
        .x |>
          group_by(fYEAR, MPAComp, .draw) |>
          summarise(value = mean(value),
            frac = mean(frac),
            .groups = "drop"
          )
      }))  |>
    mutate(mpa_sum = map(.x = mpa_posteriors,
      .f = ~ {
        .x |>
          dplyr::select(-.draw) |>
          group_by(fYEAR, MPAComp) |>
          tidybayes::summarise_draws(mean, median, HDInterval::hdi,
            pl1 = ~ mean(.x < 0),
            pl2 = ~ mean(.x < 1),
            pg1 = ~ mean(.x > 0),
            pg2 = ~ mean(.x > 1)
          ) |>
          ungroup() |>
          mutate(Pl = ifelse(variable == "value", pl1, pl2)) |>
          mutate(Pg = ifelse(variable == "value", pg1, pg2)) |>
          dplyr::select(-pl1, -pl2, -pg1, -pg2) |>
          arrange(fYEAR, desc(MPAComp))
      })) |>
    mutate(domain = "GBR") |>
    dplyr::select(-data)

## cat(print(head(data_total$mpa_sum[[1]])))
  cat("\nBind Sector and GBR posteriors together\n")
  data <- data |> bind_rows(data_total)

  MPA_contrast_plots_prepare_data_bar__ <- function(data,
                                                response = "Density",
                                                sub_model = "restricted"#,
                                                ## fYEAR =  2026
                                                ) {
    response_ <- response
    sub_model_ <- sub_model
    ## fYEAR_<- fYEAR

    data_cntr <-
      data |>
      dplyr::filter(response == response_,
        sub_model == sub_model_) |>
      droplevels() |>
      dplyr::select(domain, group, mpa_sum) |>
      mutate(sector = factor(domain,
        levels = c("CG", "PC", "CA", "IN", "TO", "WH", "PO", "SW", "CB", "GBR"),
        labels =  c("Cape Grenville", "Princess Charlotte Bay", "Cairns",
          "Innisfail", "Townsville", "Whitsundays", "Pompeys", "Swains",
          "Cap-Bunkers", "Total GBR"))) |>
      unnest("mpa_sum") #|>
    ## filter(fYEAR == fYEAR_) |>
    ## droplevels()
  }

  MPA_contrast_plots_prepare_data_slab__ <- function(data,
                                                response = "Density",
                                                sub_model = "restricted"#,
                                                ) {
    response_ <- response
    sub_model_ <- sub_model

    data_cntr <-
      data |>
      dplyr::filter(response == response_,
        sub_model == sub_model_) |>
      droplevels() |>
      dplyr::select(domain, group, mpa_posteriors) |>
      mutate(sector = factor(domain,
        levels = c("CG", "PC", "CA", "IN", "TO", "WH", "PO", "SW", "CB", "GBR"),
        labels =  c("Cape Grenville", "Princess Charlotte Bay", "Cairns",
          "Innisfail", "Townsville", "Whitsundays", "Pompeys", "Swains",
          "Cap-Bunkers", "GBR wide"))) |>
      unnest("mpa_posteriors") #|>
    ## filter(fYEAR == fYEAR_) |>
    ## droplevels()
  }
  pretty_major_breaks <- function(limits) {
    major_breaks <- case_when(
      max(limits) > 12 ~ list(c(0.1, 0.4, 1, 3, 11)),
      max(limits) > 9 ~ list(c(0.125, 0.25, 0.5, 1, 2, 4, 8)),
      max(limits) > 1 ~ list(c(0.25, 0.5, 1, 2, 4)),
      max(limits) <= 1 ~ list(c(0.8, 0.9, 1, 1.1, 1.25)),
      .default =  list(1)
    )
    return(major_breaks[[1]])
  }
  pretty_minor_breaks <- function(limits) {
    ## minor_breaks <- case_when(
    ##   max(limits) > 12 ~ list(c(0.2, 0.5, 1, 2, 6)),
    ##   max(limits) > 9 ~ list(c(0.125, 0.25, 0.5, 1, 2, 4, 8)),
    ##   max(limits) < 5 ~ list(c(0.25, 0.5, 1, 2, 4)),
    ##   .default =  list(1)
    ## )
    minor_breaks <- seq(0, round(max(limits)), by = 1)

    return(c(1/minor_breaks, minor_breaks))
  }

  MPA_contrast_plots_gg_bar__ <- function(dat_cntr) {
    dat_cntr <- dat_cntr |>
      filter(variable == "frac") |>
      droplevels() |>
      mutate(diff = case_when(
        Pl > 0.90 ~ "lower",
        Pg > 0.90 ~ "higher",
        .default = "no diff")) |>
      mutate(diff = factor(diff))
    max_diff <- dat_cntr |>
      summarise(max = max(upper, na.rm = TRUE),
        min = min(lower, na.rm = TRUE)) |>
      mutate(max_diff = max(max, 1/min)) |>
      pull(max_diff)
    if (max_diff > 20) max_diff <- 20
    custom_colours <- c("higher" = "blue", "lower" = "red", "no diff" = "gray")
    gg <-
      dat_cntr |>
      ggplot(aes(y = fYEAR, x = median)) +
      geom_vline(xintercept = 1, linetype = "dashed") +
      geom_pointrange(aes(xmin = lower, xmax = upper, color = diff), size = 0.5,
        show.legend = TRUE) +
      scale_y_discrete('', limits = rev) +
      scale_x_continuous("Percentage difference (Reserve vs Non-reserve)",
        trans = scales::log2_trans(),
        ## label = function(x) 100*(x - 1),
        label = \(x) sprintf("%0.1f", 100*(x - 1)),
        limit = c(1/max_diff, max_diff),
        ## breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8)) +
        breaks = pretty_major_breaks,
        minor_breaks = pretty_minor_breaks
      ) +
      scale_colour_manual(
        name = "",
        ## breaks = c("higher", "lower", "no diff"),
        labels = c("higher", "lower", "not different"),
        values = custom_colours,
        limits = names(custom_colours),
        drop = FALSE) +
      theme_classic() +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(vjust = 1.5),
        ## axis.title.x = element_blank(),
        axis.ticks.length.x = unit(2.75, units = "points"),
        axis.minor.ticks.length.x = rel(0.75),
        panel.spacing = unit(1, unit = "lines"),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        strip.placement = "outside"
      ) +
      guides(x = guide_axis(minor.ticks = TRUE))
    ## guides(x = guide_axis(minor.ticks = TRUE),
    ##   colour = guide_legend(override.aes =
    ##                           list(colour = c("higher" = "green", "lower" = "red", "do diff" = "gray"))))
    return(gg)
  }
  MPA_contrast_plots_gg_slab__ <- function(dat_cntr) {
    dat_cntr <- dat_cntr |>
      ## filter(!is.na(group), group != "NA") |>
      ## droplevels() |>
      group_by(fYEAR) |>
      mutate(Pl = mean(frac < 1),
        Pg = mean(frac > 1),
        evidence = Pl >= 0.9 | Pg >= 0.9
      ) |>
      ungroup()
      ## mutate(diff = case_when(
      ##   Pl > 0.90 ~ "lower",
      ##   Pg > 0.90 ~ "higher",
      ##   .default = "no diff")) |>
      ## mutate(diff = factor(diff))
    max_diff <- dat_cntr |>
      summarise(max = max(frac, na.rm = TRUE),
        min = min(frac, na.rm = TRUE)) |>
      mutate(max_diff = max(max, 1/min)) |>
      pull(max_diff)
    if (max_diff > 20) max_diff <- 20
    ## custom_colours <- c("higher" = "blue", "lower" = "red", "no diff" = "gray")
    gg <-
      dat_cntr |>
      ggplot(aes(y = fYEAR, x = frac, evidence = evidence)) +
      geom_vline(xintercept = 1, linetype = "dashed") +
      ggdist::stat_slab(
        aes(fill = after_stat(interaction(x > 1, evidence)),
          alpha = after_stat(interaction(x > 1, evidence))
        ),
        colour = "black",
        linewidth = 0.5,
        trim = TRUE,
        ## slab_alpha = 0.5,
        p_limits = c(0.025, 0.95)) +
      scale_y_discrete('', limits = rev) +
      scale_x_continuous("Percentage difference (Reserve vs Non-reserve)",
        trans = scales::log2_trans(),
        ## label = function(x) 100*(x - 1),
        label = \(x) sprintf("%0.1f", 100*(x - 1)),
        limit = c(1/max_diff, max_diff),
        ## breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8)) +
        breaks = pretty_major_breaks,
        minor_breaks = pretty_minor_breaks
      ) +
      scale_fill_manual(
        values = c(
          "FALSE.TRUE" = "red",
          "TRUE.TRUE" = "blue",
          "FALSE.FALSE" = "red",
          "TRUE.FALSE" = "blue"
        ), guide = "none"
        ) +
      scale_alpha_manual(
        values = c(
          "FALSE.TRUE" = 0.6,
          "TRUE.TRUE" = 0.6,
          "FALSE.FALSE" = 0.1,
          "TRUE.FALSE" = 0.1
        ),
        guide = "none"
        ) +
      theme_classic() +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        axis.title.y = element_text(vjust = 1.5),
        axis.ticks.length.x = unit(2.75, units = "points"),
        axis.minor.ticks.length.x = rel(0.75),
        panel.spacing = unit(1, unit = "lines"),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        strip.placement = "outside"
      ) +
      guides(x = guide_axis(minor.ticks = TRUE))
    return(gg)
  }

  MPA_contrast_plots <- function(data, interval_type = "bar") {
    data_lookup <- data |>
      dplyr::select(data_method, response, sub_model) |>
      distinct() |>
      mutate(dat_contr = pmap(.l = list(response, sub_model),
        .f =  ~ {
          response_ <- ..1
          sub_model_ <- ..2
          if (interval_type == "bar") {
            dt <- MPA_contrast_plots_prepare_data_bar__(
              data,
              response = response_,
              sub_model = sub_model_
            )
          } else {
            dt <- MPA_contrast_plots_prepare_data_slab__(
              data,
              response = response_,
              sub_model = sub_model_
            )
          }
          return(dt)
        })) |>
      unnest("dat_contr") |>
      group_by(data_method, response, sub_model, domain, group) |>
      nest(.key = "dat_contr") |>
      rowwise() |>
      mutate(label = paste0(paste("cntrs", data_method, response, sub_model, domain, group, interval_type, sep = "_"), ".png")) |>
      ungroup() |>
      ## exclude those dat_contr that do not have data (because of fYEAR)
      mutate(N = map(dat_contr, .f = ~nrow(.x))) |>
      filter(N > 0) |>
      mutate(gg = map(dat_contr,
        .f =  ~ {
          if (interval_type == "bar") {
            gg <- MPA_contrast_plots_gg_bar__(.x)
          } else {
            gg <- MPA_contrast_plots_gg_slab__(.x)
          }
          return(gg)
        })
      ) |>
      mutate(interval_type = interval_type)

    walk2(data_lookup$gg, data_lookup$label,
      .f =  ~ {
        cat(paste("\t- generate", .y, "\n"))
        ggsave(filename = paste0(output_path, "/figures/", .y), plot = .x, width = 5, height = 5)
      })
    data_lookup <- data_lookup |> dplyr::select(-dat_contr, -gg, -N)
    return(data_lookup)
  }


  cat("\nMake the plots\n")
  data_cntr <- MPA_contrast_plots(data, interval_type = "bar") |>
    bind_rows(MPA_contrast_plots(data, interval_type = "slab"))
  saveRDS(data_cntr, file = paste0(data_path, "modelled/", "zoning_contrasts_sector.rds"))
  ## save the data behind the plots
  saveRDS(data, file = paste0(data_path, "modelled/", "zoning_contrasts_sector_data.rds"))

  ## Now calculate the GBR wide values

  cat(paste("Finished generating MPA Zoning contrast figures\n"))

}

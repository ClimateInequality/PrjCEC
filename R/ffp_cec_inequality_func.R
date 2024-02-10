#' Compute environmental exposure at group and geography intersection
#'
#' @description Given two data files, one with location (row) and date (column)
#' specific environmental exposure levels, and one with location (row) and
#' population-group (column) specific share of population, we compute
#' population-group-specific environmental exposure mean and percentiles,
#' overall mean and percentiles, and across-group mean and percentiles. Program
#' allows for aggregation at higher location and grouping levels.
#'
#' @param spt_path_data string path where various data and key files are stored
#' @param spt_path_out string path where data and image output files are stored
#' @param st_file_demo string name (A1) of file containing: SHARE(loc, pop),
#' loc are rows, pop are columns
#' @param st_file_envir string name (A2) of file containing: POLL(loc, day),
#' loc are rows, day are columns
#' @param st_file_key_popgrp string name (B1) of the population key file.
#' col1 = pop-key (pop col names), e.g., smallest intersectional group id;
#' col3toN = population group name, several group characteristics,
#' aggregate group characteristics.
#' @param st_file_key_loc string name (B2) of the loc key file. col1 is loc-key
#' (loc row names), e.g, county id, long-lat id ; col2 is higher-loc-code-key,
#' e.g., prefecture id, country-id; col3 etc are location name etc.
#' @param st_file_key_loc_agg string name (B3) of the higher - loc - key file.
#' col1 is higher-loc-ke, e.g., country-id; col2toN = location name, even higher
#' aggregation name, e.g., country name, region-id, region-name.
#' @param str_prefix_demo variable names (C1) for population groups,
#' column prefix for FILE_DEMO
#' @param stv_key_demo variable names (C2) for population groups,
#'  col1 name in population key file
#' @param stv_grp_demo variable names (C3) for population groups,
#' Population group for aggregation name in population key file
#' @param arv_label_demo variable names (C4) for population groups,
#' group attributes to consider include in output file.
#' @param str_prefix_loc variable names for (D1) location names,
#' column prefix for loc
#' @param stv_key_loc variable names for (D2) location names,
#' col1 name in location ke file with same value as loc rows
#' @param stv_key_loc_agg variable names for (D3) location names,
#' col2 name for higher level location key
#' @param stv_grp_loc variable names for (D4) location names,
#' Regional group for aggregation, jointly with population group,
#' average exposure shown at STV_GRP_LOC X STV_GRP_DEMO level
#' @param arv_label_loc variable names for (D5) location names,
#' group attributes labels to consider to include, can be empty
#' @param bl_save_img boolean store image
#' @param bl_save_eps boolean store eps image
#' @param bl_save_csv boolean store data
#' @param snm_new_file_name_prefix string file name prefix for csv and img
#' @param st_time_stats string for type of within year stats to compute, "mean" or "share"
#' @param fl_temp_bound float for temperature bound if `st_time_stats` is share of days
#' @param bl_greater boolean if to compute larger or smaller than `fl_temp_bound`
#' @param verbose boolean print progress and key results
#' @param verbose_debug boolean print detailed progress and detailed results
#' @author fan wang, \url{http://fanwangecon.github.io}
#'
#' @return an array of tax-liabilities for particular kids count and martial
#'   status along an array of income levels
#' @references
#' \url{https://fanwangecon.github.io/prjenvdemo/articles/fv_rda_simu_loc_demo.html}
#' @export
#' @import readr dplyr tidyr ggplot2 stringr
#'
ffp_demo_loc_env_inequality <- function(
    spt_path_data, spt_path_out,
    st_file_demo = "df_sedac_demo_global_2010_bydeg.csv",
    st_file_envir = "aod_merged_2010_1_365.csv",
    st_file_key_popgrp = "key_sedac.csv",
    st_file_key_loc = "key_loc.ccv",
    st_file_key_loc_agg = "key_country_code.csv",
    str_prefix_demo = "popgrp",
    stv_key_demo = "popgrp_key",
    stv_grp_demo = "gender", arv_label_demo = c("gender"),
    str_prefix_loc = "",
    stv_key_loc = "location_key", stv_key_loc_agg = "ISOCODE",
    stv_grp_loc = "region_name", arv_label_loc = c("region_name"),
    str_prefix_time = "day",
    snm_new_file_name_prefix = "ineq",
    st_time_stats = "share", fl_temp_bound = 35, bl_greater = TRUE,
    bl_save_img = TRUE, bl_save_eps = FALSE, bl_save_csv = TRUE,
    verbose = FALSE, verbose_debug = FALSE) {
  # spt_root <- "C:/Users/fan/Documents/Dropbox (UH-ECON)/"
  # spt_data <- "PIRE/team/angelo_santos/data/data_created/aod_sedac"
  # spt_path_data <- file.path(spt_root, spt_data,
  #                           fsep = .Platform$file.sep)

  # # SPT_PATH_OUT is the path for where files are stored.
  # spt_root <- "C:/Users/fan/Documents/Dropbox (UH-ECON)/"
  # spt_results <- "PIRE/team/angelo_santos/data/data_created/aod_sedac/ineq"
  # spt_path_out <- file.path(spt_root, spt_results,
  #                           fsep = .Platform$file.sep)

  # Common parameters
  bl_simu_data <- FALSE

  # E. Time variables
  # str_prefix_time <- "day"

  # new file new for saving
  snm_new_file_name <- paste(
    snm_new_file_name_prefix,
    stv_grp_demo, stv_grp_loc,
    sep = "_"
  )

  # Simulation parameters, unused now
  it_M_location <- 20
  it_N_pop_groups <- 100

  ar_fl_percentiles <- c(0.1, 0.2, 0.8, 0.9)
  ar_fl_ratio_upper <- c(0.8, 0.9)
  ar_fl_ratio_lower <- c(0.2, 0.1)


  # setwd(spt_path_data)

  # mt_pop_data_frac_sedac_load <- readr::read_csv(st_file_demo)
  # df_key_loc_country <- readr::read_csv(st_file_key_loc)
  # tb_loc_pollution_aod_load <- readr::read_csv(st_file_envir)

  # tb_loc_pollution_aod_load <- tb_loc_pollution_aod_load %>%
  #   left_join(
  #     df_key_loc_country %>% select(GBCounty, location_id),
  #     by="location_id") %>%
  #   select(location_id, GBCounty, everything())
  # # Average over higher location level by days
  # tb_loc_pollution_aod_load <- tb_loc_pollution_aod_load %>%
  #   group_by(GBCounty) %>%
  #   summarise(across(everything(), mean))
  # # Select columns
  # tb_loc_pollution_aod_load <- tb_loc_pollution_aod_load %>%
  #   select(-location_id) %>%
  #   rename(location_id = GBCounty)

  # # Change the ID column in key file
  # df_key_loc_country_alt <- tb_loc_pollution_aod_load %>%
  #   select(location_id)
  # df_key_loc_country_alt <- df_key_loc_country_alt %>%
  #   mutate(GBCounty = location_id)
  # df_key_loc_country <- df_key_loc_country_alt
  # # df_key_loc_country %>%
  # #   group_by(GBCounty)

  # # Rename column ID in population file as well
  # mt_pop_data_frac_sedac_load <- mt_pop_data_frac_sedac_load %>%
  #   rename(location_id = GBCounty)

  setwd(spt_path_data)

  mt_pop_data_frac_sedac_load <- readr::read_csv(st_file_demo)
  df_key_loc_country <- readr::read_csv(st_file_key_loc)
  tb_loc_pollution_aod_load <- readr::read_csv(st_file_envir)

  tb_loc_pollution_aod_load <- tb_loc_pollution_aod_load %>%
    left_join(
      df_key_loc_country %>% select(!!sym(stv_key_loc_agg), !!sym(stv_key_loc)),
      by = stv_key_loc
    ) %>%
    select(!!sym(stv_key_loc), !!sym(stv_key_loc_agg), everything())
  # Average over higher location level by days
  tb_loc_pollution_aod_load <- tb_loc_pollution_aod_load %>%
    group_by(!!sym(stv_key_loc_agg)) %>%
    summarise(across(everything(), mean))
  # Select columns
  tb_loc_pollution_aod_load <- tb_loc_pollution_aod_load %>%
    select(-!!sym(stv_key_loc)) %>%
    rename(!!sym(stv_key_loc) := !!sym(stv_key_loc_agg))

  # Change the ID column in key file
  df_key_loc_country_alt <- tb_loc_pollution_aod_load %>%
    select(!!sym(stv_key_loc))
  df_key_loc_country_alt <- df_key_loc_country_alt %>%
    mutate(!!sym(stv_key_loc_agg) := !!sym(stv_key_loc))
  df_key_loc_country <- df_key_loc_country_alt

  # Rename column ID in population file as well
  mt_pop_data_frac_sedac_load <- mt_pop_data_frac_sedac_load %>%
    rename(!!sym(stv_key_loc) := !!sym(stv_key_loc_agg))


  # Load population data ------------------
  if (!bl_simu_data) {
    setwd(spt_path_data)

    # 10. Load
    # mt_pop_data_frac_sedac_load <- readr::read_csv(st_file_demo)

    # 20. Wide frame to long frame, to get all combinations of
    # location and demographic groups as rows, and pop as mass
    # in the location for this population group.
    # drop places with NA
    mt_pop_sedac_long <- mt_pop_data_frac_sedac_load %>%
      pivot_longer(
        cols = starts_with(str_prefix_demo),
        names_to = c(str_prefix_demo),
        names_pattern = paste0(str_prefix_demo, "(.*)"),
        values_to = "pop"
      ) %>%
      rename(popgrp_key = popgrp) %>%
      mutate(popgrp_key = paste0(str_prefix_demo, popgrp_key)) %>%
      drop_na(pop)

    # 30. Load key demographics age_groups
    # map between all popgrp and aggregated popgroup basd on "age_group"
    # initial popgrp_id has all age groups x gender
    # age_group has only 4 age groups, no gender
    df_key_sedac <- readr::read_csv(st_file_key_popgrp)
    df_key_sedac <- df_key_sedac %>%
      # rename(popgrp_id = popgrp_id) %>%
      group_by(!!sym(stv_grp_demo)) %>%
      mutate(popgrp2_id = cur_group_id()) %>%
      select(
        popgrp_key, !!sym(stv_grp_demo), !!!syms(arv_label_demo),
        popgrp2_id
      ) %>%
      ungroup()

    # 31. match string to popgrp_age
    df_popgrp_age_key <- df_key_sedac %>%
      select(
        !!sym(stv_grp_demo), !!!syms(arv_label_demo),
        popgrp2_id
      ) %>%
      rename(popgrp_age = popgrp2_id) %>%
      distinct()

    # 40. Merge long demo dataframe with aggregate group keys
    mt_pop_sedac_long <- mt_pop_sedac_long %>%
      left_join(df_key_sedac %>%
        select(popgrp_key, popgrp2_id), by = stv_key_demo)

    # 45. Merge population within location id, by new the aggregated
    # popgrp2_id, which has only 4 groups
    # mutate first to check summation is correct
    mt_pop_sedac_long_agg <- mt_pop_sedac_long %>%
      arrange(!!sym(stv_key_loc), popgrp2_id, popgrp_key) %>%
      group_by(!!sym(stv_key_loc), popgrp2_id) %>%
      mutate(pop_grp_sum = sum(pop, na.rm = TRUE)) %>%
      select(!!sym(stv_key_loc), popgrp2_id, pop_grp_sum) %>%
      slice(1) %>%
      ungroup() %>%
      rename(popgrp_age = popgrp2_id, pop = pop_grp_sum)

    # # id column to string
    # df_pop_data_frac <- mt_pop_data_frac_sedac_long %>%
    #   rename(location_id = id) %>%
    #   mutate(location = paste0("location", location_id)) %>%
    #   select(location, location_id, popgrp2_id, pop)

    # 50. Load key country, region, continent information
    df_key_country_code <- readr::read_csv(st_file_key_loc_agg)
    df_key_country_code <- df_key_country_code %>%
      select(
        !!sym(stv_key_loc_agg), !!sym(stv_grp_loc),
        !!!syms(arv_label_loc)
      )

    # 55. Load country key and matching location id information
    # df_key_loc_country <- readr::read_csv(st_file_key_loc)
    df_key_loc_country <- df_key_loc_country %>%
      select(!!sym(stv_key_loc), !!sym(stv_key_loc_agg)) %>%
      drop_na(!!sym(stv_key_loc_agg)) %>%
      group_by(!!sym(stv_key_loc_agg)) %>%
      mutate(popgrp = cur_group_id()) %>%
      ungroup(!!sym(stv_key_loc_agg)) %>%
      arrange(popgrp)

    # 60, (a) location (geo-coord); (b) country-code;
    # (c) region-code
    df_key_loc <- df_key_loc_country %>%
      left_join(df_key_country_code, by = stv_key_loc_agg) %>%
      select(-popgrp) %>%
      drop_na(!!sym(stv_grp_loc)) %>%
      group_by(!!sym(stv_grp_loc)) %>%
      mutate(popgrp_region = cur_group_id()) %>%
      ungroup(!!sym(stv_grp_loc)) %>%
      arrange(popgrp_region)
    # 61. match string to popgrp_region
    df_popgrp_region_key <- df_key_loc %>%
      select(
        !!sym(stv_grp_loc), popgrp_region,
        !!!syms(arv_label_loc)
      ) %>%
      distinct()

    # 70. Population expansion by country/regions
    # total mass sums to 1 sum(mt_pop_sedac_long_agg$pop),
    # but not all population found a country code match
    df_pop_data_frac_long <- mt_pop_sedac_long_agg %>%
      left_join(
        df_key_loc %>%
          select(!!sym(stv_key_loc), popgrp_region),
        by = stv_key_loc
      ) %>%
      select(!!sym(stv_key_loc), popgrp_region, popgrp_age, pop) %>%
      drop_na(popgrp_region)
    # total mass sums to 94.8 percent, sum(df_pop_data_frac$pop)
    # Lost 5.2 percent due to either location not matching to
    # country or country not a part of a region group.

    # 73. Get location keys with pops
    df_location_with_pop <- df_pop_data_frac_long %>%
      rename(location_id = !!sym(stv_key_loc)) %>%
      select(location_id) %>%
      distinct(location_id) %>%
      mutate(has_pop = TRUE)

    # 75. Generate final popgrp joint key
    df_pop_data_frac_long <- df_pop_data_frac_long %>%
      group_by(popgrp_region, popgrp_age) %>%
      mutate(popgrp = cur_group_id()) %>%
      ungroup()

    # # Unique identifier for regions x demographics
    df_key_loc_unique <- df_pop_data_frac_long %>%
      distinct(popgrp_region, popgrp_age, popgrp) %>%
      arrange(popgrp)

    # Spread long to wide
    df_pop_data_frac <- df_pop_data_frac_long %>%
      select(!!sym(stv_key_loc), popgrp, pop) %>%
      arrange(popgrp, !!sym(stv_key_loc), pop) %>%
      pivot_wider(
        id_cols = c(stv_key_loc),
        names_from = "popgrp",
        names_prefix = "popgrp",
        values_from = pop
      ) %>%
      arrange(!!sym(stv_key_loc)) %>%
      mutate(location = paste0("location", !!sym(stv_key_loc))) %>%
      select(location, everything(), -!!sym(stv_key_loc))

    # Merge with country key
    fl_total_pop <- sum(df_pop_data_frac[, 2:(dim(df_pop_data_frac)[2])], na.rm = TRUE)
  }
  # key_loc_load <- readr::read_csv("key_loc.csv")

  # Load pollution data ------------------
  if (!bl_simu_data) {
    setwd(spt_path_data)

    # Load
    # tb_loc_pollution_aod_load <- readr::read_csv(st_file_envir)

    # Aggregate simple mean

    if (tolower(st_time_stats) == tolower("mean")) {
      tb_loc_pollution_all <- tb_loc_pollution_aod_load %>%
        mutate(
          avgdailypm10 = base::rowMeans(
            dplyr::pick(contains(str_prefix_time)),
            na.rm = TRUE
          )
        ) %>%
        select(!!sym(stv_key_loc), avgdailypm10) %>%
        rename(location_id = !!sym(stv_key_loc))
    } else if (tolower(st_time_stats) == tolower("share")) {
      if (bl_greater) {
        it_col_count <- dim(tb_loc_pollution_aod_load)[2]
        tb_loc_pollution_all <- tb_loc_pollution_aod_load %>%
          mutate(across(contains(str_prefix_time), function(x) ifelse(x < fl_temp_bound, 0, 1))) %>%
          mutate(
            avgdailypm10 = base::rowMeans(
              dplyr::pick(contains(str_prefix_time)),
              na.rm = TRUE
            )
          ) %>%
          select(!!sym(stv_key_loc), avgdailypm10) %>%
          rename(location_id = !!sym(stv_key_loc))
      } else {
        it_col_count <- dim(tb_loc_pollution_aod_load)[2]
        tb_loc_pollution_all <- tb_loc_pollution_aod_load %>%
          mutate(across(contains(str_prefix_time), function(x) ifelse(x >= fl_temp_bound, 0, 1))) %>%
          mutate(
            avgdailypm10 = base::rowMeans(
              dplyr::pick(contains(str_prefix_time)),
              na.rm = TRUE
            )
          ) %>%
          select(!!sym(stv_key_loc), avgdailypm10) %>%
          rename(location_id = !!sym(stv_key_loc))
      }
    }

    # Merge with population id to only include locations with population
    tb_loc_pollution <- tb_loc_pollution_all %>%
      left_join(df_location_with_pop, by = "location_id") %>%
      filter(has_pop == TRUE) %>%
      select(location_id, avgdailypm10)

    tb_loc_pollution <- tb_loc_pollution %>%
      mutate(location = paste0("location", location_id)) %>%
      select(location, everything(), -location_id)
  }


  # INPUTS Demographics -----------
  # mt_pop_data_frac <- 1
  if (bl_simu_data) {
    mt_pop_data_frac <- matrix(data = NA, nrow = it_M_location, ncol = it_N_pop_groups)
    colnames(mt_pop_data_frac) <- paste0("popgrp", seq(1, it_N_pop_groups))
    rownames(mt_pop_data_frac) <- paste0("location", seq(1, it_M_location))

    # Share of population per location
    set.seed(123)
    ar_p_loc <- dbinom(0:(3 * it_M_location - 1), 3 * it_M_location - 1, 0.5)
    it_start <- length(ar_p_loc) / 2 - it_M_location / 2
    ar_p_loc <- ar_p_loc[it_start:(it_start + it_M_location - 1)]
    ar_p_loc <- ar_p_loc / sum(ar_p_loc)

    # Different bernoulli "win" probability for each location
    set.seed(234)
    # ar_fl_unif_prob <- sort(runif(it_M_location)*(0.25)+0.4)
    ar_fl_unif_prob <- sort(runif(it_M_location))

    # Generate population proportion by locality
    for (it_loc in 1:it_M_location) {
      ar_p_pop_condi_loc <- dbinom(0:(it_N_pop_groups - 1), it_N_pop_groups - 1, ar_fl_unif_prob[it_loc])
      mt_pop_data_frac[it_loc, ] <- ar_p_pop_condi_loc * ar_p_loc[it_loc]
    }

    # Sum of cells, should equal to 1
    print(paste0("pop frac sum = ", sum(mt_pop_data_frac)))

    df_pop_data_frac <- as_tibble(mt_pop_data_frac, rownames = "location")

    # Display
    if (verbose) {
      st_caption <- paste(
        "Share of population in each location and demographic cell",
        st_popgrp_disp, st_loc_disp,
        sep = " "
      )
      round((mt_pop_data_frac[ar_it_loc_disp, ar_it_popgrp_disp]) * 100, 3) %>%
        kable(caption = st_caption) %>%
        kable_styling_fc()
    }
  }

  # INPUT Pollution -------------------
  # tb_loc_pollution <- 1
  if (bl_simu_data) {
    fl_meanlog <- 3.4
    fl_sdlog <- 0.35
    # hist(rlnorm(1000, meanlog = fl_meanlog, sdlog = fl_sdlog))

    # draw
    set.seed(123)
    ar_pollution_loc <- rlnorm(it_M_location, meanlog = fl_meanlog, sdlog = fl_sdlog)
    # pollution dataframe
    # 5 by 3 matrix

    # Column Names
    ar_st_varnames <- c("location", "avgdailypm10")

    # Combine to tibble, add name col1, col2, etc.
    tb_loc_pollution <- as_tibble(ar_pollution_loc) %>%
      rowid_to_column(var = stv_key_loc) %>%
      rename_all(~ c(ar_st_varnames)) %>%
      mutate(location = paste0("location", location))

    # Display
    if (verbose) {
      st_caption <- paste("Exposure across locations", st_loc_disp, sep = " ")
      tb_loc_pollution[ar_it_loc_disp, ] %>%
        kable(caption = st_caption) %>%
        kable_styling_fc()
    }
  }

  # 10. Reshape population data, so each observation is location/demo ----
  df_pop_data_frac_long <- df_pop_data_frac %>%
    pivot_longer(
      cols = starts_with("popgrp"),
      names_to = c("popgrp"),
      names_pattern = paste0("popgrp(.*)"),
      values_to = "pop_frac"
    )
  df_pop_data_frac_long <- df_pop_data_frac_long %>%
    mutate(pop_frac = pop_frac / fl_total_pop) %>%
    drop_na(pop_frac)
  sum(df_pop_data_frac_long$pop_frac, na.rm = TRUE)

  # 20. Combine population and pollution data -------------
  df_pop_pollution_long <- df_pop_data_frac_long %>%
    left_join(tb_loc_pollution, by = "location") %>%
    drop_na(avgdailypm10)

  sum(df_pop_data_frac_long$pop_frac, na.rm = TRUE)

  # display
  if (verbose) {
    st_caption <- paste("Population x Location Long Frame (15 rows shown)", sep = " ")
    df_pop_pollution_long[
      round(seq(1, dim(df_pop_pollution_long)[1], length.out = 15)),
    ] %>%
      kable(caption = st_caption) %>%
      kable_styling_fc()
  }


  ## 30. Group-specific CDF ----------------

  # Follow four steps above
  # nearest neighbor percentiles
  df_pop_pollution_by_popgrp_cdf <- df_pop_pollution_long %>%
    arrange(popgrp, avgdailypm10) %>%
    group_by(popgrp) %>%
    mutate(
      cdf_pop_condi_popgrp_sortpm10 = cumsum(pop_frac / sum(pop_frac)),
      pmf_pop_condi_popgrp_sortpm10 = (pop_frac / sum(pop_frac))
    )

  # Display
  if (verbose) {
    st_caption <- paste("Distribution within groups, sorted CDFs (15 rows shown)", sep = " ")
    df_pop_pollution_by_popgrp_cdf[
      round(seq(1, dim(df_pop_pollution_by_popgrp_cdf)[1], length.out = 15)),
    ] %>%
      kable(caption = st_caption) %>%
      kable_styling_fc_wide()
  }

  # 40. Stats 1 and 2: excess pollution burden and share of people below/above mean -----

  # Stats 1: excess pollution burden
  df_excess_pollution_burden <- df_pop_pollution_by_popgrp_cdf %>%
    ungroup() %>%
    mutate(
      popgrp_mass_total = sum(pop_frac, na.rm = TRUE)
    ) %>%
    mutate(
      pm10_overall_mean = (avgdailypm10 * pop_frac) / popgrp_mass_total
    ) %>%
    mutate(pm10_overall_mean = sum(pm10_overall_mean, na.rm = TRUE)) %>%
    group_by(popgrp) %>%
    mutate(
      # The share of population for this group
      popgrp_mass = sum(pop_frac, na.rm = TRUE)
    ) %>%
    mutate(
      # Pop-group mean, row by row
      pm10_grp_mean = (avgdailypm10 * pop_frac) / popgrp_mass
    ) %>%
    mutate(
      # Pop-group mean
      pm10_grp_mean = sum(pm10_grp_mean, na.rm = TRUE)
    ) %>%
    slice(1) %>%
    mutate(pm10_grp_exc_burden = pm10_grp_mean / pm10_overall_mean - 1) %>%
    select(
      popgrp, popgrp_mass,
      pm10_grp_mean, pm10_overall_mean, pm10_grp_exc_burden
    )

  fl_pm10_overall_mean <- mean(df_excess_pollution_burden %>% pull(pm10_overall_mean))

  # Stats 2: share of people within group below or above overall mean
  df_share_below_or_excess <- df_pop_pollution_by_popgrp_cdf %>%
    arrange(popgrp, avgdailypm10) %>%
    filter(avgdailypm10 < fl_pm10_overall_mean) %>%
    slice_tail() %>%
    mutate(pm10_grp_shr_exc = 1 - cdf_pop_condi_popgrp_sortpm10) %>%
    select(popgrp, pm10_grp_shr_exc)

  # merge stats 2 with stats 1
  df_excess_pollution_burden <- df_excess_pollution_burden %>%
    left_join(df_share_below_or_excess, by = "popgrp")

  # display
  if (verbose) {
    st_caption <- paste("Mean and Excess Burden by Population Groups",
      st_popgrp_disp,
      sep = " "
    )
    df_excess_pollution_burden[ar_it_popgrp_disp, ] %>%
      kable(caption = st_caption) %>%
      kable_styling_fc_wide()
  }

  # 50. Stats 3: within-population-group percentiles ----------

  # Stats 3: percentiles and ratios
  # Stats 3a: generate key within group percentiles
  # 1. 20th and 80th percentiles
  # 2. 10th and 90th percentiles
  # 3. 50th percentile
  # Generate pollution quantiles by population groups
  for (it_percentile_ctr in seq(1, length(ar_fl_percentiles))) {
    # Current within group percentile to compute
    fl_percentile <- ar_fl_percentiles[it_percentile_ctr]
    svr_percentile <- paste0("pm10_p", round(fl_percentile * 100))

    # Frame with specific percentile
    df_within_percentiles_cur <- df_pop_pollution_by_popgrp_cdf %>%
      group_by(popgrp) %>%
      filter(cdf_pop_condi_popgrp_sortpm10 >= fl_percentile) %>%
      slice(1) %>%
      mutate(!!sym(svr_percentile) := avgdailypm10) %>%
      select(popgrp, one_of(svr_percentile))

    # Merge percentile frames together
    if (it_percentile_ctr > 1) {
      df_within_percentiles <- df_within_percentiles %>%
        left_join(df_within_percentiles_cur, by = "popgrp")
    } else {
      df_within_percentiles <- df_within_percentiles_cur
    }
  }

  # display
  if (verbose) {
    st_caption <- paste("Exposure Distribution by Population Groups",
      st_popgrp_disp,
      sep = " "
    )
    df_within_percentiles[ar_it_popgrp_disp, ] %>%
      kable(caption = st_caption) %>%
      kable_styling_fc()
  }

  # 60. Stats 4: Overall distribution ----------
  df_location_mean <- df_pop_pollution_by_popgrp_cdf %>%
    ungroup() %>%
    group_by(location) %>%
    mutate(
      # The share of population for this group
      location_mass = sum(pop_frac),
      # Pop-group mean, don't need this, common within group
      pm10_mean = weighted.mean(avgdailypm10, pop_frac)
    ) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(pm10_mean) %>%
    mutate(cdf_sortpm10 = cumsum(location_mass)) %>%
    select(location, location_mass, cdf_sortpm10, pm10_mean) %>%
    mutate(popgrp = "overall")

  # display
  if (verbose) {
    st_caption <- paste("Exposure Distribution Overall", st_loc_disp, sep = " ")
    df_location_mean[ar_it_loc_disp, ] %>%
      kable(caption = st_caption) %>%
      kable_styling_fc_wide()
  }

  # 70. Stats 5: Across-group distribution ----------
  df_popgrp_mean <- df_excess_pollution_burden %>%
    ungroup() %>%
    arrange(pm10_grp_mean) %>%
    mutate(cdf_sortpm10 = cumsum(popgrp_mass)) %>%
    select(popgrp, popgrp_mass, cdf_sortpm10, pm10_grp_mean) %>%
    rename(pm10_mean = pm10_grp_mean) %>%
    mutate(popgrp = "across-group")

  # display
  if (verbose) {
    st_caption <- paste("Exposure Across Groups", st_popgrp_disp, sep = " ")
    df_popgrp_mean[ar_it_popgrp_disp, ] %>%
      kable(caption = st_caption) %>%
      kable_styling_fc_wide()
  }

  # 80. Stats 6: Overall and Across-Group Percentiles -----
  for (it_df in c(1, 2)) {
    for (it_percentile_ctr in seq(1, length(ar_fl_percentiles))) {
      # Load in data-frames
      if (it_df == 1) {
        # Overall distribution
        df_working_inputs <- df_location_mean
      } else if (it_df == 2) {
        # across-group-distribution
        df_working_inputs <- df_popgrp_mean
      }

      # Current within group percentile to compute
      fl_percentile <- ar_fl_percentiles[it_percentile_ctr]
      svr_percentile <- paste0("pm10_p", round(fl_percentile * 100))

      # Frame with specific percentile
      df_within_percentiles_cur <- df_working_inputs %>%
        filter(cdf_sortpm10 >= fl_percentile) %>%
        slice(1) %>%
        mutate(!!sym(svr_percentile) := pm10_mean) %>%
        select(popgrp, one_of(svr_percentile))

      # Merge percentile frames together
      if (it_percentile_ctr > 1) {
        df_percentiles <- df_percentiles %>%
          left_join(df_within_percentiles_cur, by = "popgrp")
      } else {
        df_percentiles <- df_within_percentiles_cur
      }
    }

    if (it_df == 1) {
      df_location_mean_perc <- df_percentiles
    } else if (it_df == 2) {
      df_popgrp_mean_perc <- df_percentiles
    }
  }

  # display
  if (verbose) {
    st_caption <- paste("Overall Exposure Distribution")
    df_location_mean_perc %>%
      kable(caption = st_caption) %>%
      kable_styling_fc()

    st_caption <- paste("Across Population Group Exposure Distribution")
    df_popgrp_mean_perc %>%
      kable(caption = st_caption) %>%
      kable_styling_fc()
  }

  # 90. Stats: combine all -----
  # Percentiles and excess burden data combined
  df_excburden_percentiles <- df_excess_pollution_burden %>%
    left_join(df_within_percentiles, by = "popgrp")
  # Overall and Across Group percentiles
  df_excburden_percentiles <- bind_rows(
    df_excburden_percentiles %>% mutate(stats_type = "within-group"), 
    df_location_mean_perc %>% mutate(stats_type = "overall"), 
    df_popgrp_mean_perc %>% mutate(stats_type = "across-group")
  )

  # 100. Stats: Relative percentiles ----
  # lower and upper bound or relative within group ratios
  # can only use values appearing in the percentiles list prior
  # Stats 4c: Ratios
  # Generate P80 to P20 ratio, and P90 to P10 standard inequality ratios
  for (it_ratio_ctr in seq(1, length(ar_fl_ratio_upper))) {
    # Upper and lower percentile bounds
    fl_ratio_upper <- ar_fl_ratio_upper[it_ratio_ctr]
    fl_ratio_lower <- ar_fl_ratio_lower[it_ratio_ctr]
    svr_ratio_upper_perc <- paste0("pm10_p", round(fl_ratio_upper * 100))
    svr_ratio_lower_perc <- paste0("pm10_p", round(fl_ratio_lower * 100))

    # New relative within group ratio variable name
    svr_ratio <- paste0("pm10_rat_p", round(fl_ratio_upper * 100), "_dvd_p", round(fl_ratio_lower * 100))

    # Generate P80 to P20 ratio, etc.
    df_excburden_percentiles <- df_excburden_percentiles %>%
      mutate(!!sym(svr_ratio) := !!sym(svr_ratio_upper_perc) / !!sym(svr_ratio_lower_perc))
  }

  # display
  if (verbose) {
    st_caption <- paste("Exposure within/across/overall Population Group P80-P20 Inequality",
      st_popgrp_disp,
      sep = " "
    )
    df_excburden_percentiles[ar_it_popgrp_disp_withoverall, ] %>%
      select(
        -pm10_overall_mean,
        -starts_with("pm10_grp_excbrd_p"),
        -starts_with("pm10_p")
      ) %>%
      kable(caption = st_caption) %>%
      kable_styling_fc_wide()
  }

  ## 110. Visualization ----------------------
  # Rounding excess burden s.d.
  df_scatter_main <- df_excburden_percentiles %>%
    filter(!popgrp %in% c("overall", "across-group"))
  fl_pm10_rat_p80_dvd_p20_overall <- mean(df_excburden_percentiles %>%
    filter(popgrp %in% c("overall")) %>% pull(pm10_rat_p80_dvd_p20))
  fl_pm10_rat_p80_dvd_p20_acrossgrp <- mean(df_excburden_percentiles %>%
    filter(popgrp %in% c("across-group")) %>% pull(pm10_rat_p80_dvd_p20))
  st_title <- paste0("Relative Percentile Ratios and Excess Burdens")
  # title_line1 <- paste0("Histogram shows the distribution of Relative Ratios")

  # Generate a Data Sample by Drawing from the Distribution
  it_sample_draws <- 1e6
  # ar_it_draws <- sample(1:it_N_pop_groups, it_sample_draws, replace=TRUE, prob=ar_data_grp_shares)
  # ar_sample_draws <- ar_data_grp_exc_burden[ar_it_draws]
  # Draw histogram
  pl_excess_burden <- df_scatter_main %>%
    rename(expo_grp_mean = pm10_grp_mean) %>%
    ggplot(aes(
      x = pm10_grp_exc_burden,
      y = pm10_rat_p80_dvd_p20
    )) +
    geom_jitter(aes(size = popgrp_mass, color = expo_grp_mean), width = 0.15, size = 2) +
    # geom_smooth(span = 0.50, se = TRUE) +
    theme_bw() +
    geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed", size = 2) +
    geom_hline(
      aes(
        yintercept = fl_pm10_rat_p80_dvd_p20_overall,
        linetype = "Overall"
      ),
      color = "red", size = 2
    ) +
    geom_hline(
      aes(
        yintercept = fl_pm10_rat_p80_dvd_p20_acrossgrp,
        linetype = "Across-groups"
      ),
      color = "green", size = 2
    ) +
    labs(
      title = st_title,
      # subtitle = paste0(title_line1),
      x = "Excess Exposure Burden  = (Exposure Share)/(Pop Share) - 1",
      y = "P80 to P20 Ratios",
      caption = "Based on simulated random data for testing."
    ) +
    scale_linetype_manual(
      name = "", values = c(2, 2),
      guide = guide_legend(override.aes = list(color = c("green", "red")))
    )

  # Print
  snm_img_name <- snm_new_file_name
  print(pl_excess_burden)
  setwd(spt_path_out)
  if (bl_save_img) {
    spn_output_fig_png <- file.path(
      spt_path_out,
      paste0(snm_img_name, ".png"),
      fsep = .Platform$file.sep
    )
    spn_output_fig_eps <- file.path(
      spt_path_out,
      paste0(snm_img_name, ".eps"),
      fsep = .Platform$file.sep
    )

    png(spn_output_fig_png,
      width = 160,
      height = 105, units = "mm",
      res = 150, pointsize = 7
    )

    if (bl_save_eps) {
      ggsave(
        spn_output_fig_eps,
        plot = last_plot(),
        device = "eps",
        path = NULL,
        scale = 1,
        width = 200,
        height = 100,
        units = c("mm"),
        dpi = 150,
        limitsize = TRUE
      )
    }
    print(pl_excess_burden)
    dev.off()
  }

  # if (bl_save_csv) {
  # Merge with file to update keys
  df_excburden_percentiles_keys <- df_excburden_percentiles %>%
    mutate(popgrp = as.numeric(popgrp)) %>%
    left_join(df_key_loc_unique, by = "popgrp") %>%
    arrange(popgrp) %>%
    select(popgrp, popgrp_region, popgrp_age, everything())

  # Merge with key strings
  df_excburden_percentiles_keys <- df_excburden_percentiles_keys %>%
    left_join(df_popgrp_region_key, by = "popgrp_region") %>%
    left_join(df_popgrp_age_key, by = "popgrp_age") %>%
    select(
      stats_type, popgrp,
      !!sym(stv_grp_loc), !!!syms(arv_label_loc),
      !!sym(stv_grp_demo), !!!syms(arv_label_demo),
      everything(), -popgrp_region, -popgrp_age
    )

  # Change variable names, replace out the word pm10 from file names
  df_excburden_percentiles_keys <- df_excburden_percentiles_keys %>%
    rename_at(
      vars(starts_with("pm10_")),
      funs(stringr::str_replace(., "pm10_", "expo_"))
    )

  # Store file with iage data
  spn_output_file <- file.path(
    spt_path_out,
    paste0(snm_new_file_name, ".csv"),
    fsep = .Platform$file.sep
  )
  readr::write_csv(df_excburden_percentiles_keys, spn_output_file)
  if (verbose_debug) {
    print(glue::glue(
      "File saved successfully: ", spn_output_file
    ))
  }

  return(df_excburden_percentiles_keys)
}

#' Compute double-thresholds, share of time and share of population, individuals at risk
#'
#' @description In each location, compute location-specific statistics along
#' increments of thresholds, including share of days/hours experiencing more
#' or less than some threshold of temperature or pollution. For each statistics
#' compute PMF and CDF across locations. Then Create dataframe where rows are share of
#' days for example exposed to some threshold of temperature or pollution, and
#' columns keep track of different threholds specified.
#' This runs after \input{ffp_demo_loc_env_inequality} has ran.
#'
#' The input data might be for share of days above some threshold. This function
#' will also compute the 3D (threshold=x, share-of-days=y, share-of-pop=z) stats
#' based on share of days below each temperature threshold. This is under the assumption
#' that the input \input{st_time_stats} is equal to share
#'
#'
#' @param spt_path_out string path where data and image output files are stored
#' @param str_prefix_demo variable names (C1) for population groups,
#' column prefix for FILE_DEMO
#' @param stv_grp_demo variable names (C3) for population groups,
#' Population group for aggregation name in population key file
#' @param stv_grp_loc variable names for (D4) location names,
#' Regional group for aggregation, jointly with population group,
#' average exposure shown at STV_GRP_LOC X STV_GRP_DEMO level
#' @param snm_in_file_name_prefix_base string file name prefix for input csv
#' @param snm_out_file_name_prefix_base string file prefix for output csv
#' @param st_time_stats string for type of within year stats to compute, "mean" or "share"
#' @param ar_temp_bound array for temperature bound if `st_time_stats` is share of days
#' @param bl_greater boolean if to compute larger or smaller than `fl_temp_bound`
#' @param st_demo_subgroup string value of demographic sub-group label to focus on,
#' only considers this subgroup, it is a category of the \input{stv_grp_demo} variable.
#' @param fl_round_gap_percent float for potential rounding to alternative aggregation.
#' percent interpretation comes when we consider share of days/hours.
#' @param bl_shave_inv boolean store 1-cdf when working with `st_time_stats="share"`
#' the base cdf is share of time exceeding some temperature threshold, 1-cdf would be
#' the share of time falling below some temperature threshold.
#' @param bl_save_img boolean store image
#' @param bl_save_csv boolean store data
#' @param verbose boolean print progress and key results
#' @param verbose_debug boolean print detailed progress and detailed results
#' @author fan wang, \url{http://fanwangecon.github.io}
#'
#' @return four csv files for \input{st_time_stats} equal to share, 3D output CSV files where
#' columns are value (e.g. temperature) thresholds, rows are share of days, and cells are share
#' of population for PMF (probability mass function), and share of population up to row for CDF
#' \itemize{
#'   \item le_pmf below value (e.g., temp) threshold 3D
#'   \item ge_pmf above value (e.g., temp) threshold 3D
#'   \item le_cdf below value (e.g., temp) threshold 3D
#'   \item ge_cdf above value (e.g., temp) threshold 3D
#' }
#' @references
#' \url{https://fanwangecon.github.io/prjenvdemo/articles/fv_rda_simu_loc_demo.html}
#' @export
#' @import readr dplyr tidyr ggplot2
#'
ffp_demo_loc_thresdouble_time_heatcold <- function(
    spt_path_in,
    spt_path_out,
    str_prefix_demo = "popgrp",
    stv_grp_demo = "gender",
    stv_grp_loc = "region_name",
    snm_in_file_name_prefix_base = "ineq",
    snm_out_file_name_prefix_base = "ineq",
    st_time_stats = "share", ar_temp_bound = seq(-20, 20, by = 1), bl_greater = TRUE,
    st_demo_subgroup = "0_14",
    fl_round_gap_percent = 2.5,
    bl_share_inv = FALSE,
    ar_st_cdf_pmf = c("cdf", "pmf"),
    bl_save_img = TRUE, bl_save_csv = TRUE,
    verbose = FALSE, verbose_debug = FALSE) {
  # 9. Stats to compute within year ----
  #   st_time_stats <- "share"
  #   ar_temp_bound <- seq(-20, 20, by=1)
  #   bl_greater <- TRUE
  # Sub_group of focus
  #   st_demo_subgroup <- "0_14"
  #   fl_round_gap_percent <- 2.5

  # 10 Loop through generating individual files and combining files ----
  # file counter
  it_file_ctr <- 0
  for (fl_temp_bound in ar_temp_bound) {
    it_file_ctr <- it_file_ctr + 1

    # 10. File names prefix, the rest of name from combining grp_demo and grp_loc
    # stv_grp_demo and stv_grp_loc
    ls_prefix_res <- ffp_demo_file_prefix(
      snm_new_file_name_prefix = snm_in_file_name_prefix_base,
      st_time_stats = st_time_stats,
      fl_temp_bound = fl_temp_bound,
      bl_greater = bl_greater,
      verbose = FALSE
    )
    snm_new_file_name_prefix <- ls_prefix_res$snm_new_file_name_prefix



    # if (tolower(st_time_stats) == tolower("mean")) {
    #   snm_new_file_name_prefix <- paste0("ineq_", st_time_stats)
    # } else if (tolower(st_time_stats) == tolower("share")) {
    #   if (bl_greater) {
    #     snm_new_file_name_prefix <- paste0("ineq_", st_time_stats, "_gr", fl_temp_bound)
    #   } else {
    #     snm_new_file_name_prefix <- paste0("ineq_", st_time_stats, "_ls", fl_temp_bound)
    #   }
    # }

    # 9. some additional parameters ------
    # bl_save_csv <- TRUE
    # verbose <- FALSE
    # verbose_debug <- TRUE

    # Combine existing outputs to aggregate file
    # File name
    snm_new_file_name <- paste(
      snm_new_file_name_prefix,
      stv_grp_demo, stv_grp_loc,
      sep = "_"
    )

    # Load file
    spn_results_file <- file.path(
      spt_path_in,
      paste0(snm_new_file_name, ".csv"),
      fsep = .Platform$file.sep
    )
    df_excburden_percentiles_keys <- readr::read_csv(spn_results_file)

    # Add column to file
    df_excburden_percentiles_keys <- df_excburden_percentiles_keys %>%
      mutate(
        st_time_stats = st_time_stats,
        fl_temp_bound = fl_temp_bound,
        bl_greater = bl_greater,
        compute_id = it_file_ctr
      )

    # Generate file of opposite direction, if original is above, this is below, vice versa
    if (st_time_stats == "share" & bl_share_inv) {
      df_excburden_percentiles_keys_bl_oppo <- df_excburden_percentiles_keys %>%
        mutate(
          st_time_stats = st_time_stats,
          fl_temp_bound = fl_temp_bound,
          bl_greater = !bl_greater,
          compute_id = it_file_ctr
        ) %>%
        mutate(
          expo_grp_mean = 1 - expo_grp_mean,
          expo_overall_mean = 1 - expo_overall_mean
        )
    }

    # Drop irrelevant aggregate rows and select relevant information
    df_excburden_percentiles_keys <- df_excburden_percentiles_keys %>%
      drop_na(popgrp) %>%
      select(
        !!sym(str_prefix_demo), !!sym(stv_grp_demo), !!sym(stv_grp_loc),
        popgrp_mass,
        expo_grp_mean,
        expo_overall_mean,
        fl_temp_bound
      )
    if (st_time_stats == "share" & bl_share_inv) {
      df_excburden_percentiles_keys_bl_oppo <- df_excburden_percentiles_keys_bl_oppo %>%
        drop_na(popgrp) %>%
        select(
          !!sym(str_prefix_demo), !!sym(stv_grp_demo), !!sym(stv_grp_loc),
          popgrp_mass,
          expo_grp_mean,
          expo_overall_mean,
          fl_temp_bound
        )
    }

    if (it_file_ctr == 1) {
      df_temp_cdf_full_jnt_main <- df_excburden_percentiles_keys
    } else {
      df_temp_cdf_full_jnt_main <- bind_rows(
        df_temp_cdf_full_jnt_main, df_excburden_percentiles_keys
      )
    }
    if (st_time_stats == "share" & bl_share_inv) {
      if (it_file_ctr == 1) {
        df_temp_cdf_full_jnt_oppo <- df_excburden_percentiles_keys_bl_oppo
      } else {
        df_temp_cdf_full_jnt_oppo <- bind_rows(
          df_temp_cdf_full_jnt_oppo, df_excburden_percentiles_keys_bl_oppo
        )
      }
    }
  }

  if (st_time_stats == "share" & bl_share_inv) {
    ar_bl_main_and_oppo <- c(TRUE, FALSE)
  } else {
    ar_bl_main_and_oppo <- c(TRUE)
  }

  for (bl_main_and_oppo in ar_bl_main_and_oppo) {
    if (bl_main_and_oppo) {
      df_temp_cdf_full_jnt <- df_temp_cdf_full_jnt_main
    } else {
      df_temp_cdf_full_jnt <- df_temp_cdf_full_jnt_oppo
    }
    if (verbose) {
      st_caption <- paste("df_temp_cdf_full_jnt")
      print(glue::glue("F-999138, S1: {st_caption}"))
      print(df_temp_cdf_full_jnt)
    }

    # 11. Reshape from long to wide ----
    # Each row is a different location
    # Each column is a different threshold level
    # Each cell contains share in this location for this threshold with above x temp
    # keep only child in youngest age group
    df_temp_cdf_full_jnt <- df_temp_cdf_full_jnt %>%
      filter(!!sym(stv_grp_demo) == st_demo_subgroup) %>%
      select(-popgrp, -expo_overall_mean, -!!sym(stv_grp_demo)) %>%
      rename(sharedays_above_temp = expo_grp_mean)

    # Long to wide
    df_temp_cdf_full_jnt_wide <- df_temp_cdf_full_jnt %>%
      arrange(!!sym(stv_grp_loc)) %>%
      pivot_wider(
        id_cols = c("popgrp_mass", !!sym(stv_grp_loc)),
        names_from = fl_temp_bound,
        names_prefix = "temp_ge_",
        values_from = sharedays_above_temp
      ) %>%
      ungroup() %>%
      mutate(popgrp_mass = popgrp_mass / sum(popgrp_mass)) %>%
      arrange(!!sym(stv_grp_loc))
    if (verbose) {
      st_caption <- paste("df_temp_cdf_full_jnt_wide")
      print(glue::glue("F-999138, S2: {st_caption}"))
      print(df_temp_cdf_full_jnt_wide)
    }

    # 12. Generate greater than threshold-specific CDFs ----
    it_file2_ctr <- 0
    for (fl_temp_bound in ar_temp_bound) {
      it_file2_ctr <- it_file2_ctr + 1

      # Threshold variable
      st_col_share <- paste0("temp_ge_", fl_temp_bound)

      # Select one threshold
      df_temp_cdf_full_jnt_wide_sel <- df_temp_cdf_full_jnt_wide %>%
        select(popgrp_mass, !!sym(st_col_share))

      # Sort and generate CDF
      fl_round_multiple <- 100 / fl_round_gap_percent
      df_temp_cdf_full_jnt_wide_sel <- df_temp_cdf_full_jnt_wide_sel %>%
        mutate(
          !!sym(st_col_share) :=
            # round(!!sym(st_col_share) * fl_round_multiple, 0) / fl_round_multiple
            ceiling(!!sym(st_col_share) * fl_round_multiple) / fl_round_multiple
        ) %>%
        arrange(!!sym(st_col_share)) %>%
        group_by(!!sym(st_col_share)) %>%
        summarize(popgrp_mass_sum = sum(popgrp_mass))

      # Add temp bound as variable, define PMF and CDF
      # PMF = probability mass function
      # iCDF = 1 - CDF
      df_temp_cdf_full_jnt_wide_sel <- df_temp_cdf_full_jnt_wide_sel %>%
        ungroup() %>%
        rename(
          share_days = !!sym(st_col_share),
          pop_pmf = popgrp_mass_sum
        ) %>%
        mutate(
          fl_temp_bound = fl_temp_bound
        )

      # File counting again
      if (it_file2_ctr == 1) {
        df_temp_cdf_full_long <- df_temp_cdf_full_jnt_wide_sel
      } else {
        df_temp_cdf_full_long <- bind_rows(
          df_temp_cdf_full_long, df_temp_cdf_full_jnt_wide_sel
        )
      }
      # sum(df_temp_cdf_full_jnt_wide_sel %>% pull(popgrp_mass_sum))
    }

    for (st_cdf_pmf in ar_st_cdf_pmf) {
      # for (bl_pmf_or_icdf in c(1, 2)) {
      if (verbose) {
        st_caption <- paste("df_temp_cdf_full_long")
        print(glue::glue("F-999138, S3: {st_caption}"))
        print(df_temp_cdf_full_long)
      }

      bl_greater_use <- bl_greater
      if (!bl_main_and_oppo) {
        # If not main, use the opposite of bl_greater
        bl_greater_use <- !bl_greater
      }

      # Variable names, greater or less then
      st_rela <- ls_prefix_res$st_rela
      st_names_prefix <- st_rela

      # 13. Final long to wide conversion, rows are share days, columns are fl_temp_bounds ----
      # Long to wide
      df_temp_cdf_full_wide <- df_temp_cdf_full_long %>%
        arrange(fl_temp_bound, share_days) %>%
        pivot_wider(
          id_cols = c("share_days"),
          names_from = fl_temp_bound,
          names_prefix = st_names_prefix,
          values_from = pop_pmf
        ) %>%
        arrange(share_days)
      if (verbose) {
        st_caption <- paste("df_temp_cdf_full_wide")
        print(glue::glue("F-999138, S4: {st_caption}"))
        print(df_temp_cdf_full_wide)
      }

      # Replace NA with 0, otherwise CDF does not compute
      df_temp_cdf_full_wide <- df_temp_cdf_full_wide %>%
        mutate(across(contains(st_names_prefix), function(x) replace_na(x, 0)))

      if (st_cdf_pmf == "cdf") {
        df_temp_cdf_full_wide <- df_temp_cdf_full_wide %>%
          mutate(across(contains(st_names_prefix), function(x) (cumsum(x))))
      }

      # rename share of time, not necessarily days
      df_temp_cdf_full_wide <- df_temp_cdf_full_wide %>% 
        rename(share_time = share_days)

      # File out
      if (bl_save_csv) {
        st_rela <- ls_prefix_res$st_rela
        snm_new_file_name_demo <- paste(
          stv_grp_demo, st_demo_subgroup,
          sep = "is"
        )
        snm_new_file_name <- paste(
          snm_out_file_name_prefix_base,
          st_rela,
          snm_new_file_name_demo,
          stv_grp_loc,
          sep = "_"
        )

        spn_output_file <- file.path(
          spt_path_out,
          paste0(snm_new_file_name, ".csv"),
          fsep = .Platform$file.sep
        )

        readr::write_csv(df_temp_cdf_full_wide, spn_output_file, na = "0")
        if (verbose_debug) {
          print(glue::glue("F-999138, S5"))
          print(glue::glue(
            "File saved successfully: ", spn_output_file
          ))
        }
      }
    }
  }

  return(df_temp_cdf_full_wide)
}

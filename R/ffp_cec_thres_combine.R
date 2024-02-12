#' Combine threshold-specific files for joint CDF/PMF files
#'
#' @description `ffp_demo_loc_env_inequality` generates threshold hold specific
#' files where each row is a different demo-location's average and distributional
#' exposure statistics. In this file, we combine group-mean statistics from selected
#' demo-location group from each threshold file, and combine them together.
#'
#' @param spt_path_in string path data input individual threshold files are
#' @param spt_path_out string path where data and image output files are stored
#' @param str_prefix_demo variable names (C1) for population groups,
#' column prefix for FILE_DEMO
#' @param stv_grp_demo variable names (C3) for population groups,
#' Population group for aggregation name in population key file
#' @param stv_grp_loc variable names for (D4) location names,
#' Regional group for aggregation, jointly with population group,
#' average exposure shown at STV_GRP_LOC X STV_GRP_DEMO level
#' @param bl_save_img boolean store image
#' @param bl_save_csv boolean store data
#' @param snm_in_file_name_prefix_base string file name prefix for input csv
#' @param snm_out_file_name_prefix_base string file prefix for output csv
#' @param st_time_stats string for type of within year stats to compute, "mean" or "share"
#' @param ar_temp_bound array for temperature bound if `st_time_stats` is share of days
#' @param bl_greater boolean if to compute larger or smaller than `fl_temp_bound`
#' @param st_demo_subgroup string value of demographic sub-group label to focus on,
#' only considers this subgroup, it is a category of the \input{stv_grp_demo} variable.
#' @param st_loc_subgroup string value of location sub-group label to focus on,
#' only considers this subgroup, it is a category of the \input{stv_grp_loc} variable.
#' @param verbose boolean print progress and key results
#' @param verbose_debug boolean print detailed progress and detailed results
#' @author fan wang, \url{http://fanwangecon.github.io}
#'
#' @return one file with all threshold info, potentially subsetted
#' @references
#' \url{https://fanwangecon.github.io/prjenvdemo/articles/fv_rda_simu_loc_demo.html}
#' @export
#' @import readr dplyr tidyr ggplot2
#'
ffp_demo_loc_thres_dist <- function(
    spt_path_in,
    spt_path_out,
    str_prefix_demo = "popgrp",
    stv_grp_demo = "gender",
    stv_grp_loc = "region_name",
    snm_in_file_name_prefix_base = "ineq",
    snm_out_file_name_prefix_base = "ineq",
    st_time_stats = "share", ar_temp_bound = seq(-20, 20, by = 1), bl_greater = TRUE,
    st_demo_subgroup = "0_14",
    st_loc_subgroup = NULL,
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

    # # Generate file of opposite direction, if original is above, this is below, vice versa
    # if (st_time_stats == "share") {
    #   df_excburden_percentiles_keys_bl_oppo <- df_excburden_percentiles_keys %>%
    #     mutate(
    #       st_time_stats = st_time_stats,
    #       fl_temp_bound = fl_temp_bound,
    #       bl_greater = !bl_greater,
    #       compute_id = it_file_ctr) %>%
    #     mutate(
    #       pm10_grp_mean = 1 - pm10_grp_mean,
    #       pm10_overall_mean = 1 - pm10_overall_mean)
    # }

    # Drop irrelevant aggregate rows and select relevant information
    df_excburden_percentiles_keys <- df_excburden_percentiles_keys %>%
      drop_na(popgrp) %>%
      select(
        stats_type,
        !!sym(str_prefix_demo), !!sym(stv_grp_demo), !!sym(stv_grp_loc),
        popgrp_mass,
        expo_grp_mean,
        expo_overall_mean,
        fl_temp_bound
      )
    # df_excburden_percentiles_keys_bl_oppo <- df_excburden_percentiles_keys_bl_oppo %>%
    #   drop_na(popgrp) %>%
    #   select(
    #     !!sym(str_prefix_demo), !!sym(stv_grp_demo), !!sym(stv_grp_loc),
    #     popgrp_mass,
    #     pm10_grp_mean,
    #     pm10_overall_mean,
    #     fl_temp_bound
    #   )

    if (it_file_ctr == 1) {
      df_temp_cdf_full_jnt_main <- df_excburden_percentiles_keys
    } else {
      df_temp_cdf_full_jnt_main <- bind_rows(
        df_temp_cdf_full_jnt_main, df_excburden_percentiles_keys
      )
    }
    # if (st_time_stats == "share") {
    #   if (it_file_ctr == 1) {
    #     df_temp_cdf_full_jnt_oppo <- df_excburden_percentiles_keys_bl_oppo
    #   } else {
    #     df_temp_cdf_full_jnt_oppo <- bind_rows(
    #       df_temp_cdf_full_jnt_oppo, df_excburden_percentiles_keys_bl_oppo)
    #   }
    # }
  }

  # Subsetting and saving a subset of file
  # File out if either loc or pop subgrouping
  if (!is.null(st_demo_subgroup) | !is.null(st_loc_subgroup)) {
    if (!is.null(st_demo_subgroup)) {
      df_temp_cdf_full_jnt_main <- df_temp_cdf_full_jnt_main %>%
        filter(!!sym(stv_grp_demo) == st_demo_subgroup)
    }
    if (!is.null(st_loc_subgroup)) {
      df_temp_cdf_full_jnt_main <- df_temp_cdf_full_jnt_main %>%
        filter(!!sym(stv_grp_loc) == st_loc_subgroup)
    }
    # Save two files, the f
    # st_rela is unrelated to sps
    st_rela <- ls_prefix_res$st_rela

    if (bl_save_csv) {
      # demo and loc string components
      if (!is.null(st_demo_subgroup)) {
        snm_new_file_name_demo <- paste(stv_grp_demo, st_demo_subgroup, sep = "is")
      } else {
        snm_new_file_name_demo <- stv_grp_demo
      }
      if (!is.null(st_loc_subgroup)) {
        snm_new_file_name_loc <- paste(stv_grp_loc, st_loc_subgroup, sep = "is")
      } else {
        snm_new_file_name_loc <- stv_grp_loc
      }

      snm_new_file_name <- paste(
        snm_out_file_name_prefix_base,
        st_rela,
        snm_new_file_name_demo,
        snm_new_file_name_loc,
        sep = "_"
      )

      spn_output_file_subset <- file.path(
        spt_path_out,
        paste0(snm_new_file_name, ".csv"),
        fsep = .Platform$file.sep
      )

      # store now conditioned file
      readr::write_csv(
        df_temp_cdf_full_jnt_main, spn_output_file_subset,
        na = "0"
      )
      if (verbose_debug) {
        print(glue::glue("F-105127, S1"))
        print(glue::glue(
          "All threshold subsetted file saved successfully: ",
          spn_output_file_subset
        ))
      }
    }
  } else {
    # Save full file
    if (bl_save_csv) {
      # Save two files, the f
      # st_rela is unrelated to sps
      st_rela <- ls_prefix_res$st_rela
      snm_new_file_name <- paste(
        snm_out_file_name_prefix_base,
        st_rela,
        stv_grp_demo,
        stv_grp_loc,
        sep = "_"
      )

      spn_output_file <- file.path(
        spt_path_out,
        paste0(snm_new_file_name, ".csv"),
        fsep = .Platform$file.sep
      )

      readr::write_csv(df_temp_cdf_full_jnt_main, spn_output_file, na = "0")
      if (verbose_debug) {
        print(glue::glue("F-105127, S2"))
        print(glue::glue(
          "All threshold joint file saved successfully: ", spn_output_file
        ))
      }
    }
  }


  return(df_temp_cdf_full_jnt_main)
}

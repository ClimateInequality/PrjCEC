# Implements https://github.com/ClimateInequality/PrjCEC/issues/39
# Developed based on `sandbox/R/run_full_dist/ffp_gpp_inequality_child_cdf_full_a1.R`.

# 0. Load local script support ------
source("R-script/ffs_pirecec_support.R")
ls_script_controls <- ffs_run_set_params()
it_nnodes <- ls_script_controls$it_nnodes
ls_run <- ls_script_controls$ls_run
ar_temp_bound <- ls_script_controls$ar_temp_bound
# 1 = parallel gen results over thresholds, 2 = combine parallel results across thresholds
# ls_run <- c(1, 2)
# ls_run <- c(2)

# Load data managment and statistics packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
# library(tidyverse)

# load parallel packages
if (1 %in% ls_run) {
  library(iterators)
  library(parallel)
  library(foreach)
  library(doParallel)

  # 2. Initiate cluster for parallelization ------
  # Get the number of cores
  it_n_cores_computer <- parallel::detectCores()
  glue::glue("Number of cores on computers:{it_n_cores_computer}")
  glue::glue("Using these number of cores:{it_nnodes}")
  # Start cluster
  ob_cluster <- parallel::makeCluster(
    it_nnodes,
    type = "PSOCK"
  )
  # Register cluster
  doParallel::registerDoParallel(cl = ob_cluster)
}

# 3. Get paths, constructed in `R-script/ffs_pirecec_support.R`
ls_paths <- ffs_pirecec_path_run()
spt_cec_sandbox_r_func <- ls_paths$spt_cec_sandbox_r_func
spt_pire_team_kf <- ls_paths$spt_pire_team_kf
# Data input folder
spt_path_data <- file.path(spt_pire_team_kf, "clean_data", fsep = .Platform$file.sep)
# Results/data output folder
spt_path_out_datares <- file.path("data-res", fsep = .Platform$file.sep)
spt_path_out_root <- file.path(spt_pire_team_kf,
  "ineq_results", "ineq_demo_risk_par",
  fsep = .Platform$file.sep
)

# 6. Variable names for population groups -----
str_prefix_demo <- "popgrp"
stv_key_demo <- "popgrp_key"
stv_grp_demo <- "age_group_m3"
arv_label_demo <- c("age_group_m3")

# 7. Variable names for location names -----
str_prefix_loc <- ""
stv_key_loc <- "location_id"
stv_key_loc_agg <- "GBCounty"
stv_grp_loc <- "GBCounty"
arv_label_loc <- c("GBCounty")

# 8. Time variables -----
str_prefix_time <- "day"

# 9. Stats to compute within year
st_time_stats <- "share"
# ar_temp_bound <- seq(1,2, by=1)
# ar_temp_bound <- seq(-40, 40, length.out = 81)
bl_greater <- TRUE

# Sub_group of focus
st_demo_subgroup <- "0_14"
fl_round_gap_percent <- 1

# 10. File names prefix, the rest of name from combining grp_demo and grp_loc
# stv_grp_demo and stv_grp_loc
snm_in_file_name_prefix_base <- "ineq"

# 11. some additional parameters ------
bl_save_img <- FALSE
bl_save_csv <- TRUE
verbose <- FALSE
verbose_debug <- TRUE
# IF bl_skip_gen_if_exist is TRUE, only generate new threshold specific files if file does not already exist.
bl_skip_gen_if_exist <- TRUE

# sets to run
ar_it_set <- c(1, 2)
for (it_set in ar_it_set) {
  # 2.C Results/data output folder
  if (it_set == 1) {
    st_yrfile_suffix_utci <- "1990_hour"
    st_yrfile_suffix <- "1990"
    # dm = demo, dbl = double thresholds, atrisk = individuals (children) at risk
    snm_out_file_name_prefix_base <- "dmdbl_90atrisk"
  } else if (it_set == 2) {
    st_yrfile_suffix_utci <- "2020_hour"
    st_yrfile_suffix <- "2020"
    # dm = demo, dbl = double thresholds, atrisk = individuals (children) at risk
    snm_out_file_name_prefix_base <- "dmdbl_20atrisk"
  }


  spt_results <- paste0("res", st_yrfile_suffix_utci)
  spt_path_out <- file.path(
    spt_path_out_root,
    spt_results,
    fsep = .Platform$file.sep
  )

  # 4. Data file names -----
  st_file_demo <- paste0("df_china_census_county_", st_yrfile_suffix, ".csv")
  st_file_envir <- paste0("df_era5_utci_china_", st_yrfile_suffix_utci, ".csv")

  # 5. Key file names -----
  # 5.1 Name of the population key file
  st_file_key_popgrp <- paste0("df_key_demo_china_census_", st_yrfile_suffix, ".csv")
  # 5.2 Name of the loc key file:
  st_file_key_loc <- paste0("df_key_loc_china_coord2county_", st_yrfile_suffix, ".csv")
  # 5.3 Name of the higher-loc-key file:
  st_file_key_loc_agg <- paste0("df_key_loc_china_county2province_", st_yrfile_suffix, ".csv")

  # 10 Loop through generating individual files and combining files ----
  for (it_run in ls_run) {
    if (it_run == 1) {
      # non-parallel loop
      # for (fl_temp_bound in ar_temp_bound) {
      # Parallel loop, note below we load in four packages.
      foreach(
        fl_temp_bound = ar_temp_bound,
        .packages = c("readr", "dplyr", "tidyr", "ggplot2")
      ) %dopar% {
        # fl_temp_bound <- 0

        # 10. File names prefix, the rest of name from combining grp_demo and grp_loc
        # stv_grp_demo and stv_grp_loc
        snm_new_file_name_prefix <- ffp_demo_file_prefix(
          snm_new_file_name_prefix = snm_in_file_name_prefix_base,
          st_time_stats = st_time_stats,
          fl_temp_bound = fl_temp_bound,
          bl_greater = bl_greater,
          verbose = FALSE
        )$snm_new_file_name_prefix
        snm_new_file_name <- paste(
          snm_new_file_name_prefix,
          stv_grp_demo, stv_grp_loc,
          sep = "_"
        )
        spn_output_file <- file.path(
          spt_path_out,
          paste0(snm_new_file_name, ".csv"),
          fsep = .Platform$file.sep
        )

        # Run function to generate new output tables
        df_excburden_percentiles_keys <- ffp_demo_loc_env_inequality(
          spt_path_data, spt_path_out,
          st_file_demo = st_file_demo,
          st_file_envir = st_file_envir,
          st_file_key_popgrp = st_file_key_popgrp,
          st_file_key_loc = st_file_key_loc,
          st_file_key_loc_agg = st_file_key_loc_agg,
          str_prefix_demo = str_prefix_demo,
          stv_key_demo = stv_key_demo,
          stv_grp_demo = stv_grp_demo, arv_label_demo = arv_label_demo,
          str_prefix_loc = str_prefix_loc,
          stv_key_loc = stv_key_loc, stv_key_loc_agg = stv_key_loc_agg,
          stv_grp_loc = stv_grp_loc, arv_label_loc = arv_label_loc,
          str_prefix_time = str_prefix_time,
          snm_new_file_name_prefix = snm_new_file_name_prefix,
          st_time_stats = st_time_stats, fl_temp_bound = fl_temp_bound, bl_greater = bl_greater,
          bl_save_img = bl_save_img, bl_save_csv = bl_save_csv,
          verbose = verbose, verbose_debug = verbose_debug
        )
      }
    }

    # 11. Run Threshold joint full dist results
    if (it_run == 2) {
      df_temp_cdf_full_wide <- ffp_demo_loc_thresdouble_time_heatcold(
        spt_path_in = spt_path_out,
        spt_path_out = spt_path_out_datares,
        str_prefix_demo = str_prefix_demo,
        stv_grp_demo = stv_grp_demo,
        stv_grp_loc = stv_grp_loc,
        snm_in_file_name_prefix_base = snm_in_file_name_prefix_base,
        snm_out_file_name_prefix_base = snm_out_file_name_prefix_base,
        st_time_stats = st_time_stats, ar_temp_bound = ar_temp_bound, bl_greater = bl_greater,
        st_demo_subgroup = st_demo_subgroup,
        fl_round_gap_percent = fl_round_gap_percent,
        bl_share_inv = FALSE,
        ar_st_cdf_pmf = c("cdf"),
        bl_save_img = FALSE, bl_save_csv = TRUE,
        verbose = TRUE, verbose_debug = FALSE
      )
    }
  }
}
# Shutdown cluster, after finishing both years' loop.
if (1 %in% ls_run) {
  parallel::stopCluster(cl = ob_cluster)
}
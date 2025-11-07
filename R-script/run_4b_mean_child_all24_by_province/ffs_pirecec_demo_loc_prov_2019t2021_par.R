# Implements Part 2 of https://github.com/ClimateInequality/PrjCEC/issues/30
# Follows from `cec_code/code_d_regional/ffp_cec_inequality_run_demo_loc_a2_parallel_region_2020.R`

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

# Load parallel packages
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

# 3. Get paths, constructed in `cec_code/ff_pirecec_support.R`
ls_paths <- ffs_pirecec_path_run()
spt_cec_sandbox_r_func <- ls_paths$spt_cec_sandbox_r_func
spt_pire_team_kf <- ls_paths$spt_pire_team_kf
# Data input folder
spt_path_data <- file.path(spt_pire_team_kf, "clean_data", fsep = .Platform$file.sep)
# Results/data output folder
spt_path_out_datares <- file.path(spt_pire_team_kf, "data-res", fsep = .Platform$file.sep)
spt_path_out <- file.path(spt_pire_team_kf,
  "ineq_results", "ineq_demo_loc_par", "prov_2019t2021",
  fsep = .Platform$file.sep
)

# 4. Data file names -----
st_file_demo <- "df_china_census_county_2020.csv"
st_file_envir <- "df_era5_utci_china_2019_2021_hour.csv"

# 5. Key file names -----
# 5.1 Name of the population key file
st_file_key_popgrp <- "df_key_demo_china_census_2020.csv"
# 5.2 Name of the loc key file:
st_file_key_loc <- "df_key_loc_china_coord2county_2020.csv"
# 5.3 Name of the higher-loc-key file:
st_file_key_loc_agg <- "ChSi1990_region_df_key_loc_china_county2province_2020.csv"

# 6. Variable names for population groups -----
str_prefix_demo <- "popgrp"
stv_key_demo <- "popgrp_key"
stv_grp_demo <- "age_group_m3"
arv_label_demo <- c("age_group_m3")

# 7. Variable names for location names -----
str_prefix_loc <- ""
stv_key_loc <- "location_id"
stv_key_loc_agg <- "GBCounty"
# Note using province names from 1990 (chongqing was a apart of sichuan)
stv_grp_loc <- "Prov_En_1990"
arv_label_loc <- c("Prov_En_1990")

# 8. Time variables -----
str_prefix_time <- "day"

# 9. Stats to compute within year
st_time_stats <- "share"
# ar_temp_bound <- seq(1,2, by=1)
# ar_temp_bound <- seq(-40, 40, length.out = 81)
bl_greater <- TRUE

# Sub_group of focus
st_demo_subgroup <- "0_14"
st_loc_subgroup <- NULL

# 10. File names prefix, the rest of name from combining grp_demo and grp_loc
# stv_grp_demo and stv_grp_loc
snm_in_file_name_prefix_base <- "ineq"
# dm = demo, 90h6t22, 1990, hours file, 6 to 22 day time hours only
snm_out_file_name_prefix_base <- "dmloc_19t21prov"

# 11. some additional parameters ------
bl_save_img <- TRUE
bl_save_csv <- TRUE
verbose <- FALSE
verbose_debug <- TRUE

# 12. Run parallel to generate raw files or to combine ----
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

      # Generate file prefix string
      snm_new_file_name_prefix <- ffp_demo_file_prefix(
        snm_new_file_name_prefix = snm_in_file_name_prefix_base,
        st_time_stats = st_time_stats,
        fl_temp_bound = fl_temp_bound,
        bl_greater = bl_greater,
        verbose = FALSE
      )$snm_new_file_name_prefix

      # Generate results
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
    parallel::stopCluster(cl = ob_cluster)
  }

  if (it_run == 2) {
    df_temp_cdf_full_jnt_main <- ffp_demo_loc_thres_dist(
      spt_path_in = spt_path_out,
      spt_path_out = spt_path_out_datares,
      str_prefix_demo = str_prefix_demo,
      stv_grp_demo = stv_grp_demo,
      stv_grp_loc = stv_grp_loc,
      snm_in_file_name_prefix_base = snm_in_file_name_prefix_base,
      snm_out_file_name_prefix_base = snm_out_file_name_prefix_base,
      st_time_stats = st_time_stats, ar_temp_bound = ar_temp_bound, bl_greater = bl_greater,
      st_demo_subgroup = st_demo_subgroup,
      st_loc_subgroup = st_loc_subgroup,
      bl_save_img = FALSE, bl_save_csv = TRUE,
      verbose = TRUE, verbose_debug = FALSE
    )
  }
}

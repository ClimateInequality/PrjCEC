# This file runs the ffp_evd_simu_loc_demo_main() function.
# Location only grouping

# 1. Load libraries ------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)
# library(tidyverse)

# 2. Specify path of (1) function (2) data input (3) data output -----
spt_root <- "C:/Users/fan/Documents/Dropbox (UH-ECON)/"
# 2.A Activate program
spt_gpp <- "repos/PrjCEC/sandbox/R/function/"
spt_path_func <- file.path(spt_root, spt_gpp, "ffp_dpd_aux.R", fsep = .Platform$file.sep)
source(spt_path_func)
spt_path_func <- file.path(spt_root, spt_gpp, "ffp_dpd_inequality_func.R", fsep = .Platform$file.sep)
source(spt_path_func)
spt_path_func <- file.path(spt_root, spt_gpp, "ffp_dpd_ineq_full_dist_func.R", fsep = .Platform$file.sep)
source(spt_path_func)

# 2.B Data input folder
spt_data <- "PIRE/team/kai_feng/clean_data"
spt_path_data <- file.path(spt_root, spt_data,
                          fsep = .Platform$file.sep)

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

# 9. Stats to compute within year ----
st_time_stats <- "share"
# fl_temp_bound <- 20
bl_greater <- TRUE
ar_temp_bound <- seq(-40, 40, by=1)

# Sub_group of focus
st_demo_subgroup <- "0_14"
fl_round_gap_percent <- 2

# 9. some additional parameters ------
bl_save_img <- FALSE
bl_save_csv <- TRUE
verbose <- FALSE
verbose_debug <- TRUE
bl_gen_input_files <- TRUE 
# IF bl_skip_gen_if_exist is TRUE, only generate new threshold specific files if file does not already exist. 
bl_skip_gen_if_exist <- TRUE

# sets to run
ar_it_set <- c(1,2,3,4) 
for (it_set in ar_it_set) {

  # 2.C Results/data output folder
  if (it_set == 1) {
    st_yrfile_suffix_utci <- "1990"
    st_yrfile_suffix <- st_yrfile_suffix_utci
  } else if (it_set == 2) {
    st_yrfile_suffix_utci <- "2020"
    st_yrfile_suffix <- st_yrfile_suffix_utci
  } else if (it_set == 3) {
    st_yrfile_suffix_utci <- "1990_hour"
    st_yrfile_suffix <- "1990"
  } else if (it_set == 4) {    
    st_yrfile_suffix_utci <- "2020_hour"
    st_yrfile_suffix <- "2020"
  }

  spt_results <- paste0("PIRE/team/kai_feng/ineq_results/ineq_cdf_full/res", st_yrfile_suffix_utci)
  spt_path_out <- file.path(spt_root, spt_results,
                            fsep = .Platform$file.sep)

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
  # file counter
  if (bl_gen_input_files) {
    for (fl_temp_bound in ar_temp_bound) {

      # 10. File names prefix, the rest of name from combining grp_demo and grp_loc
      # stv_grp_demo and stv_grp_loc
      snm_new_file_name_prefix <- ffp_demo_file_prefix(
            snm_new_file_name_prefix = snm_new_file_name_prefix,
            st_time_stats = st_time_stats, 
            fl_temp_bound = fl_temp_bound, 
            bl_greater = bl_greater,
            verbose = FALSE)$snm_new_file_name_prefix
      snm_new_file_name <- paste(
        snm_new_file_name_prefix,
        stv_grp_demo, stv_grp_loc, sep = "_")
      spn_output_file <- file.path(
        spt_path_out,
        paste0(snm_new_file_name, '.csv'),
        fsep = .Platform$file.sep)

        if (!file.exists(spn_output_file) & bl_skip_gen_if_exist) {
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
            verbose = verbose, verbose_debug = verbose_debug)
          # print output
          print(df_excburden_percentiles_keys)
      }

    }
  }

  # 11. Run Threshold joint full dist results
  df_temp_cdf_full_wide <- ffp_demo_loc_thres_dist(
      spt_path_out,
      str_prefix_demo = str_prefix_demo,
      stv_grp_demo = stv_grp_demo, 
      stv_grp_loc = stv_grp_loc, 
      snm_new_file_name_prefix = snm_new_file_name_prefix,
      st_time_stats = st_time_stats, ar_temp_bound = ar_temp_bound, bl_greater = bl_greater,
      st_demo_subgroup = st_demo_subgroup,
      fl_round_gap_percent = fl_round_gap_percent,
      bl_save_img = FALSE, bl_save_csv = TRUE,
      verbose = TRUE, verbose_debug = FALSE)
    
}
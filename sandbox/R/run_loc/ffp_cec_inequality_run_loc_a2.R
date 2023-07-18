# This file runs the ffp_evd_simu_loc_demo_main() function.
# Location only grouping


# 1. Load libraries ------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(tidyverse)

# 2. Specify path of (1) function (2) data input (3) data output -----
spt_root <- "C:/Users/fan/Documents/Dropbox (UH-ECON)/"
# 2.A Activate program
spt_gpp <- "repos/PrjCEC/sandbox/R/function/"
spt_path_func <- file.path(spt_root, spt_gpp, "ffp_dpd_inequality_func.R",
                           fsep = .Platform$file.sep)
source(spt_path_func)

# 2.B Data input folder
spt_data <- "PIRE/team/kai_feng/clean_data"
spt_path_data <- file.path(spt_root, spt_data,
                           fsep = .Platform$file.sep)

# 2.C Results/data output folder
spt_results <- "PIRE/team/kai_feng/ineq_results/ineq_loc"
spt_path_out <- file.path(spt_root, spt_results,
                           fsep = .Platform$file.sep)

# 4. Data file names -----
st_file_demo <- "df_china_census_county_2010.csv"
st_file_envir <- "df_era5_utci_china_2010.csv"

# 5. Key file names -----
# 5.1 Name of the population key file
st_file_key_popgrp <- "df_key_demo_china_census_2010.csv"
# 5.2 Name of the loc key file:
st_file_key_loc <- "df_key_loc_china_coord2county_2010.csv"
# 5.3 Name of the higher-loc-key file:
st_file_key_loc_agg <- "df_key_loc_china_county2province_2010.csv"

# 6. Variable names for population groups -----
str_prefix_demo <- "popgrp"
stv_key_demo <- "popgrp_key"
stv_grp_demo <- "all_groups"
arv_label_demo <- c()

# 7. Variable names for location names -----
str_prefix_loc <- ""
stv_key_loc <- "location_id"
stv_key_loc_agg <- "GBCounty"
stv_grp_loc <- "City_EN"
arv_label_loc <- c("City_EN", "Prov_En")

# 8. Time variables -----
str_prefix_time <- "day"

# 9. Stats to compute within year
st_time_stats <- "share"
fl_temp_bound <- 20
bl_greater <- TRUE

# 10. File names prefix, the rest of name from combining grp_demo and grp_loc
# stv_grp_demo and stv_grp_loc
if (tolower(st_time_stats) == tolower("mean")) {
  snm_new_file_name_prefix <- paste0("ineq_", st_time_stats)
} else if (tolower(st_time_stats) == tolower("share")) {
  if (bl_greater) {
    snm_new_file_name_prefix <- paste0("ineq_", st_time_stats, "_gr", fl_temp_bound)
  } else {
    snm_new_file_name_prefix <- paste0("ineq_", st_time_stats, "_ls", fl_temp_bound)
  }
}


# 9. some additional parameters ------
bl_save_img <- TRUE
bl_save_csv <- TRUE
verbose <- FALSE
verbose_debug <- TRUE

# Run function
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

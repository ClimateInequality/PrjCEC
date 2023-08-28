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
spt_results <- "PIRE/team/kai_feng/ineq_results/ineq_cdf_full"
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
ar_temp_bound <- seq(-20, 20, by=1)

# Sub_group of focus
st_demo_subgroup <- "0_14"
fl_round_gap_percent <- 2.5

# 10 Loop through generating individual files and combining files ----
ar_bl_generate <- c(FALSE)
# If generate is False, combines existing files, already generated
# ar_bl_generate <- c(FALSE)

for (bl_generate in ar_bl_generate) {

  # file counter
  it_file_ctr <- 0
  for (fl_temp_bound in ar_temp_bound) {
    it_file_ctr <- it_file_ctr + 1

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
    
    if (bl_generate) {
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

    } else {

      # Combine existing outputs to aggregate file
      # File name
      snm_new_file_name <- paste(
        snm_new_file_name_prefix,
        stv_grp_demo, stv_grp_loc, sep = "_")

      # Load file
      spn_results_file <- file.path(
        spt_path_out,
        paste0(snm_new_file_name, '.csv'),
        fsep = .Platform$file.sep)
      df_excburden_percentiles_keys <- readr::read_csv(spn_results_file)

      # Add column to file 
      df_excburden_percentiles_keys <- df_excburden_percentiles_keys %>% 
        mutate(
          st_time_stats = st_time_stats,
          fl_temp_bound = fl_temp_bound, 
          bl_greater = bl_greater, 
          compute_id = it_file_ctr)

      # Drop irrelevant aggregate rows and select relevant information
      df_excburden_percentiles_keys <- df_excburden_percentiles_keys %>%
        drop_na(popgrp) %>% 
        select(
          !!sym(str_prefix_demo), !!sym(stv_grp_demo), !!sym(stv_grp_loc), 
          popgrp_mass, 
          pm10_grp_mean, 
          pm10_overall_mean, 
          fl_temp_bound
          )

      if (it_file_ctr == 1) {
        df_temp_cdf_full_jnt <- df_excburden_percentiles_keys
      } else {
        df_temp_cdf_full_jnt <- bind_rows(
          df_temp_cdf_full_jnt, df_excburden_percentiles_keys)
      }

    }
  }
}

# 11. Reshape from long to wide ----
# Each row is a different location
# Each column is a different threshold level
# Each cell contains share in this location for this threshold with above x temp

# keep only child in youngest age group
df_temp_cdf_full_jnt <- df_temp_cdf_full_jnt %>% 
  filter(!!sym(stv_grp_demo) == st_demo_subgroup) %>% 
  select(-popgrp, -pm10_overall_mean, -!!sym(stv_grp_demo)) %>% 
  rename(sharedays_above_temp = pm10_grp_mean)

# Long to wide
df_temp_cdf_full_jnt_wide <- df_temp_cdf_full_jnt %>%
  arrange(!!sym(stv_grp_loc)) %>%
  pivot_wider(id_cols = c("popgrp_mass", !!sym(stv_grp_loc)),
              names_from = fl_temp_bound,
              names_prefix = "temp_ge_",
              values_from = sharedays_above_temp) %>%
  ungroup() %>%
  mutate(popgrp_mass = popgrp_mass/sum(popgrp_mass)) %>%
  arrange(!!sym(stv_grp_loc))

# 12. Generate greater than threshold-specific CDFs ----
it_file2_ctr <- 0
for (fl_temp_bound in ar_temp_bound) {
  it_file2_ctr <- it_file2_ctr + 1

  # Threshold variable
  st_col_share <- paste0("temp_ge_", fl_temp_bound)
  
  # Select one threshold
  df_temp_cdf_full_jnt_wide_sel <- df_temp_cdf_full_jnt_wide %>%
    select(popgrp_mass, st_col_share) 

  # Sort and generate CDF
  fl_round_multiple <- 100/fl_round_gap_percent
  df_temp_cdf_full_jnt_wide_sel <- df_temp_cdf_full_jnt_wide_sel %>%   
    mutate(
      !!sym(st_col_share) := 
      round(!!sym(st_col_share)*fl_round_multiple, 0)/fl_round_multiple) %>% 
    arrange(!!sym(st_col_share)) %>% 
    group_by(!!sym(st_col_share)) %>% 
    summarize(popgrp_mass_sum = sum(popgrp_mass))

  # Add temp bound as variable
  df_temp_cdf_full_jnt_wide_sel <- df_temp_cdf_full_jnt_wide_sel %>%
      rename(
        share_days =!!sym(st_col_share), 
        pop_cdf = popgrp_mass_sum
      ) %>%
      mutate(fl_temp_bound = fl_temp_bound)

  # File counting again
  if (it_file2_ctr == 1) {
    df_temp_cdf_full_long <- df_temp_cdf_full_jnt_wide_sel
  } else {
    df_temp_cdf_full_long <- bind_rows(
      df_temp_cdf_full_long, df_temp_cdf_full_jnt_wide_sel)
  }
  # sum(df_temp_cdf_full_jnt_wide_sel %>% pull(popgrp_mass_sum))
}

# 13. Final long to wide conversion, rows are share days, columns are fl_temp_bounds ----
# Long to wide
df_temp_cdf_full_wide <- df_temp_cdf_full_long %>%
  arrange(fl_temp_bound, share_days) %>%
  pivot_wider(id_cols = c("share_days"),
              names_from = fl_temp_bound,
              names_prefix = "temp_ge_",
              values_from = pop_cdf) %>%
  arrange(share_days)

# File out 
snm_new_file_name <- paste(
  paste0("ineq_", st_time_stats), 
  "cdf_full", 
  stv_grp_demo,
  st_demo_subgroup,
  sep = "_")
spn_output_file <- file.path(
  spt_path_out,
  paste0(snm_new_file_name, '.csv'),
  fsep = .Platform$file.sep)
readr::write_csv(df_temp_cdf_full_wide, spn_output_file, na="0")
if (verbose_debug) {
  print(glue::glue(
    "File saved successfully: ", spn_output_file))
}
# Part 2 of https://github.com/ClimateInequality/PrjCEC/issues/46
# _notes/issues/issues_20241214_county_grid_count.md

library(dplyr)
library(tidyr)
library(readr)

verbose <- TRUE
it_row_print <- 300
it_file_code <- 321782
bl_main_save <- TRUE

# Get paths, constructed in `cec_code/ff_pirecec_support.R`
spt_path_data <- file.path(
  "/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data",
  #here::here(), "data",
  fsep = .Platform$file.sep
)
# Results/data output folder
snm_file_name_out <- "aux_county_demo_share_grids_counts_bookends.csv"
spt_res_out <- file.path(
  "/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/",
  #here::here(),
  "data-res",
  fsep = .Platform$file.sep
)

# 1. Get data and key file names, loop over 1990 and 2020 ------------------------
ar_it_year <- c(1990, 2020)
ls_df_loc_demo_grids <- vector(mode = "list", length = length(ar_it_year))
it_year_ctr <- 0
for (it_year in ar_it_year) {
  it_year_ctr <- it_year_ctr + 1
  # File names
  if (it_year == 1990) {
    # 1990 names
    st_file_demo <- "df_china_census_county_1990.csv"
    st_file_envir <- "df_era5_utci_china_1989_1991_hour.csv"
    st_file_key_popgrp <- "df_key_demo_china_census_1990.csv"
    st_file_key_loc <- "df_key_loc_china_coord2county_1990.csv"
    st_file_key_loc_agg <- "df_key_loc_china_county2province_1990.csv"
  } else if (it_year == 2020) {
    # 2020 names
    st_file_demo <- "df_china_census_county_2020.csv"
    st_file_envir <- "df_era5_utci_china_2019_2021_hour.csv"
    st_file_key_popgrp <- "df_key_demo_china_census_2020.csv"
    st_file_key_loc <- "df_key_loc_china_coord2county_2020.csv"
    st_file_key_loc_agg <- "df_key_loc_china_county2province_2020.csv"
  }
  # Read in population files
  df_demo <- read_csv(file.path(spt_path_data, st_file_demo, fsep = .Platform$file.sep))
  # Read key files
  df_key_popgrp <- read_csv(file.path(spt_path_data, st_file_key_popgrp, fsep = .Platform$file.sep))
  df_key_loc <- read_csv(file.path(spt_path_data, st_file_key_loc, fsep = .Platform$file.sep))
  df_key_loc_agg <- read_csv(file.path(spt_path_data, st_file_key_loc_agg, fsep = .Platform$file.sep))
  df_key_loc_agg <- df_key_loc_agg %>% select(GBCounty, GBProv, Prov_En, region_name)
  #   print(colnames(df_key_loc_agg))
  # # [1] "...1"          "GBCounty"      "Prov_En"       "all_locations" "GBProv"        "region_name"
  # #  [1] "...1"          "GBProv"        "Prov_En"       "...4"          "GBCounty"      "County_CH"     "GbCity"        "City_CH"       "Prov_CH"
  # [10] "all_locations" "region_name"

  # 2. Get population data for children 0 to 14 --------------------------------
  # Get pop key for corresponding age group of interest, male and female 0 to 14
  df_key_popgrp_sel <- df_key_popgrp %>%
    filter(
      year == it_year, age_group_m3 == "0_14"
    ) %>%
    select(popgrp_key, gender, age_group)
  if (verbose) {
    print(glue::glue("F-{it_file_code}, 1a"))
    print(df_key_popgrp_sel, n = it_row_print)
  }
  # Get popgrp_keys
  ar_popgrp_key_sel <- df_key_popgrp_sel %>% pull(popgrp_key)
  # Subsetting population file
  df_demo_sel <- df_demo %>% select(GBCounty, one_of(ar_popgrp_key_sel))
  if (verbose) {
    print(glue::glue("F-{it_file_code}, 1b"))
    print(summary(df_demo_sel))
    print(glue::glue("rows/cols in demo file (rows=counties): {dim(df_demo_sel)}"))
    print(glue::glue("rows/cols in key file (rows=counties): {dim(df_key_loc %>% distinct(GBCounty))}"))
  }
  # Sum up selected population groups by each location: sum up total male and femela 0 to 4
  df_demo_agg <- df_demo_sel %>%
    pivot_longer(
      cols = starts_with("popgrp"),
      names_to = c("popgrp"),
      names_pattern = paste0("popgrp(.*)"),
      values_to = "population"
    ) %>%
    group_by(GBCounty) %>%
    summarize(
      population_agg_loc = sum(population)
    )

  # 3. Count grid points per county and merge with population with prov/region info ---------------------------------
  # Count grid points per county
  df_county_gridcount <- df_key_loc %>%
    group_by(GBCounty) %>%
    summarize(n_coord_GBCounty = n())
  # Merge in population data with grid count
  df_county_gridcount_pop <- df_county_gridcount %>%
    left_join(
      df_demo_agg,
      by = "GBCounty"
    )
  # Merge with regional and prov info
  df_county_gridcount_pop <- df_county_gridcount_pop %>%
    left_join(
      df_key_loc_agg,
      by = "GBCounty"
    )
  if (verbose) {
    print(glue::glue("F-{it_file_code}, c"))
    print(summary(df_county_gridcount_pop))
    print(df_county_gridcount_pop, n = it_row_print)
  }
  
  # 4. Store year-specific files and combine -----------------
  df_county_gridcount_pop <- df_county_gridcount_pop %>%
    mutate(year = it_year)
  ls_df_loc_demo_grids[[it_year_ctr]] <- df_county_gridcount_pop
}
# Combine files from different years into a single file
df_loc_demo_grids_jnt <- do.call(bind_rows, ls_df_loc_demo_grids)

# 5. Export joint year grid count and population by county file --------------
if (bl_main_save) {
    spn_path <- file.path(
        spt_res_out, snm_file_name_out,
        fsep = .Platform$file.sep
    )
    write_csv(df_loc_demo_grids_jnt, spn_path)
    print(glue::glue("F-812763, S3"))
    print(glue::glue("File saved: {spn_path}"))
}
# This script loads multiple .rda files containing hourly Universal Thermal Climate Index (UTCI)
# data for China, cleans and harmonizes them using a spatial key, then merges and reindexes
# them into a continuous dataset for multi-year analysis.

# Workflow Summary:
# 1. Load yearly ERA5 UTCI .rda files (e.g., 1989–1991, 2019–2021).
# 2. Read a spatial key (w/ longitude–latitude–location_id) and deduplicate coordinates.
# 3. Use `rename_ml_data()` to join UTCI data with location and remove unmatched rows.
# 4. Apply `combine_and_reindex()` to merge multiple yearly datasets while renaming day columns 
#    across years (e.g., day1 … dayN).
# 5. Export the merged dataset as a .csv file .

# How to Use:
# - Update the file paths for the input .rda files, key CSV, and output file.
# - Input key that includes columns: `Long`, `Lat`, and `location_id`.
# - Run the script for each time block (eg 1989–1991).
# - Check output dimensions and column names using the diagnostic commands at the end.

# Output:
# A merged CSV with unified `location_id` and continuous time columns

require(tidyverse)
require(haven)


#-------Load in 1989-1991-------
loader1 <- load("/Users/mlaghi/Dropbox/DAEO_bigdata_prc/w_c_era5/utci/cim_annual_1940t2020/utci_china_Y1989_hourly.rda")
loader2 <- load("/Users/mlaghi/Dropbox/DAEO_bigdata_prc/w_c_era5/utci/cim_annual_1940t2020/utci_china_Y1990_hourly.rda")
loader3 <- load("/Users/mlaghi/Dropbox/DAEO_bigdata_prc/w_c_era5/utci/cim_annual_1940t2020/utci_china_Y1991_hourly.rda")

ml1 <- get(loader1[1])
ml2 <- get(loader2[1])
ml3 <- get(loader3[1])

key <- readr::read_csv("/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/df_key_loc_china_coord2county_1990.csv")

# Deduplicate key
key_unique_longlat <- key %>% distinct(Long, Lat, location_id)

# Function to clean and join with key
rename_ml_data <- function(df, key, location_col = c("Long", "Lat"), keep_col = "location_id") {
  df %>%
    dplyr::left_join(key, by = location_col) %>%
    dplyr::filter(!is.na(.data[[keep_col]])) %>%
    dplyr::relocate(.data[[keep_col]]) %>%
    dplyr::select(-all_of(location_col))
}

# Apply function to each dataset
ml1 <- rename_ml_data(ml1, key_unique_longlat)
ml2 <- rename_ml_data(ml2, key_unique_longlat)
ml3 <- rename_ml_data(ml3, key_unique_longlat)

# Combine and reindex day columns continuously
combine_and_reindex <- function(...) {
  dfs <- list(...)
  # keep first col = location_id
  for (i in seq_along(dfs)) {
    df <- dfs[[i]]
    ncol_df <- ncol(df)
    if (i == 1) {
      start_idx <- 1
    } else {
      prev_cols <- sum(sapply(dfs[1:(i-1)], function(x) ncol(x) - 1))
      start_idx <- prev_cols + 1
    }
    new_names <- c(
      colnames(df)[1],
      paste0("day", seq(start_idx, length.out = ncol_df - 1))
    )
    colnames(df) <- new_names
    dfs[[i]] <- df
  }
  # merge by location_id
  Reduce(function(x, y) dplyr::full_join(x, y, by = "location_id"), dfs)
}

ml_all <- combine_and_reindex(ml1, ml2, ml3)
write_csv(ml_all, "/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/df_era5_utci_china_1989_1991_hour.csv")


#------ 2019-2021 -------

#-------Load in 2019-2021-------
loader1 <- load("/Users/mlaghi/Dropbox/DAEO_bigdata_prc/w_c_era5/utci/cim_annual_1940t2020/utci_china_Y2019_hourly.rda")
loader2 <- load("/Users/mlaghi/Dropbox/DAEO_bigdata_prc/w_c_era5/utci/cim_annual_1940t2020/utci_china_Y2020_hourly.rda")
loader3 <- load("/Users/mlaghi/Dropbox/DAEO_bigdata_prc/w_c_era5/utci/cim_annual_1940t2020/utci_china_Y2021_hourly.rda")

ml1 <- get(loader1[1])
ml2 <- get(loader2[1])
ml3 <- get(loader3[1])

key <- readr::read_csv("/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/df_key_loc_china_coord2county_2020.csv")

# Deduplicate key
key_unique_longlat <- key %>% distinct(Long, Lat, location_id)

# Function to clean and join with key
rename_ml_data <- function(df, key, location_col = c("Long", "Lat"), keep_col = "location_id") {
  df %>%
    dplyr::left_join(key, by = location_col) %>%
    dplyr::filter(!is.na(.data[[keep_col]])) %>%
    dplyr::relocate(.data[[keep_col]]) %>%
    dplyr::select(-all_of(location_col))
}

# Apply function to each dataset
ml1 <- rename_ml_data(ml1, key_unique_longlat)
ml2 <- rename_ml_data(ml2, key_unique_longlat)
ml3 <- rename_ml_data(ml3, key_unique_longlat)

# Combine and reindex day columns continuously
combine_and_reindex <- function(...) {
  dfs <- list(...)
  # keep first col = location_id
  for (i in seq_along(dfs)) {
    df <- dfs[[i]]
    ncol_df <- ncol(df)
    if (i == 1) {
      start_idx <- 1
    } else {
      prev_cols <- sum(sapply(dfs[1:(i-1)], function(x) ncol(x) - 1))
      start_idx <- prev_cols + 1
    }
    new_names <- c(
      colnames(df)[1],
      paste0("day", seq(start_idx, length.out = ncol_df - 1))
    )
    colnames(df) <- new_names
    dfs[[i]] <- df
  }
  # merge by location_id
  Reduce(function(x, y) dplyr::full_join(x, y, by = "location_id"), dfs)
}

ml_all <- combine_and_reindex(ml1, ml2, ml3)
write_csv(ml_all, "/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/df_era5_utci_china_2019_2021_hour.csv")



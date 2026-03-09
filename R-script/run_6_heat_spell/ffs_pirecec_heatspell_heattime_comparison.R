#' Heat-Spell (consecutive days with max temp exceed threshsold) and share of hours exceeding temperature threhsold Analysis
#'
#' @description
#' This script analyzes the relationship between hourly over temperature thresholds 
#' (share of hours in excess of temperature threhsold over some year-span) and heat spell
#' occurrences (share of days in heat-spell over some year-span) across Chinese counties. 
#' It processes ERA5 UTCI (Universal Thermal Climate Index) hourly data, computes heat spell 
#' statistics using multiple threshold definitions, and generates population-weighted table where we
#' have for each location under different heat spell definitions the share of days in heat spell, 
#' and share of hours experiencing heat. We use child-population information.
#' 
#' File Running instructions:
#' 
#' 1. All paths are collected at the top, modify `spt_root_pire`
#' 2. `bl_test` = True now, running the code uses a small test dataset `data/df_era5_utci_china_2019_2021_hour_test_l50d100.csv`, should just run and generate correlation and a figure at the end. The test dataset is very small, so some values are NaN
#' 3. `bl_test` = False, to run with full all hours + all location file
#' 4. Modify initial file names to run file for alternative years and hours + all location input files
#'
#' Suggested TODO, in keeping with structure in the rest of the codebase:
#' 
#' 1. the output `df_county_stats_long` using data should be stored into `data-res`, this is the "results" output from this file.
#' 2. this file at the end has correlation analysis and also a visualization, code for those should be migrated to a `tabfig_6_heat_spell` analysis folder.
#' 3. the code in `tabfig_6_heat_spell` should generate tables, correlations, and visualizations that will be used in the paper.
#' 
#' @section File Components:
#'
#' **Data Preparation (Lines 13-50):**
#' - Loads hourly UTCI temperature data from ERA5 dataset for China
#' - Converts hour-of-year format to UTC and Shanghai datetime formats
#' - Supports both test data (1000 rows) and full dataset (2019-2021)
#'
#' **Heat Spell Analysis Function (Lines 52-105):**
#' - `fn_heat_spell_analysis(df, x, y)`: Identifies heat spell events based on temperature
#'   and duration thresholds
#' - Outputs: proportion of days exceeding heat spell criteria per location
#'
#' **Temperature Exceed Analysis Function (Lines 107-130):**
#' - `fn_temp_exceed_analysis(df, x)`: Calculates share of hours exceeding temperature threshold
#' - Outputs: proportion of hours above threshold per location
#'
#' **Multi-Threshold Analysis (Lines 132-183):**
#' - Tests multiple temperature thresholds (26°C, 32°C, 38°C)
#' - Tests multiple consecutive-day thresholds (1, 3, 5 days)
#' - Combines results from grid-level (location) analysis
#'
#' **County Aggregation (Lines 185-230):**
#' - Maps grid locations to counties using coordinate-to-county mapping
#' - Aggregates grid-level statistics to county averages
#' - Reshapes data for easy comparison across thresholds
#'
#' **Population Weighting (Lines 232-273):**
#' - Incorporates census data for population weights by county
#' - Focuses on age group 0-14 population
#' - Computes population-weighted statistics
#'
#' **Correlation Analysis (Lines 275-304):**
#' - Computes population-weighted correlations between temperature exceed portions
#'   and heat spell day portions
#' - Outputs correlation values for all threshold combinations
#' - Includes optional visualization with faceted scatter plots (commented out)
#'
#' @section Key Inputs:
#' - `df_era5_utci_china_2020_hour.csv` (or 2019-2021 full dataset): Hourly UTCI temperatures by grid location
#' - `df_key_loc_china_coord2county.csv`: Grid location to county mapping
#' - `df_key_demo_china_census.csv`: Age group classification keys
#' - `df_china_census_county.csv`: County-level population census data
#'
#' @section Key Outputs:
#' - `df_correlations`: Correlation matrix showing relationship strength between
#'   temperature exceed portions and heat spell day portions, indexed by temperature
#'   and consecutive-day thresholds
#' - `df_county_stats_long`: County-level aggregated statistics with population weights
#' - `df_combined`: Grid-level combined statistics linking temperature exceeds to heat spells
#'
#' @section Main Achievements:
#' - Converts hourly grid data to daily and county-level aggregates
#' - Implements flexible heat spell definition via temperature (x) and duration (y) thresholds
#' - Incorporates population demographics to weight county-level correlations
#' - Enables comparative analysis: time-based (hours exceeding threshold) vs. event-based
#'   (consecutive hottest days) heat metrics
#'
#' @section Related Documentation:
#' Issue 51 of https://github.com/ClimateInequality/PrjCEC/issues/51

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)
library(ggplot2)

# Path roots and file paths -------------------------
# Path to dropbox large hourly temperature file
spt_root_pire <- "C:/Users/fan/UH-ECON Dropbox/Fan Wang/PIRE/team"
# relative path to the `PrjCEC/data` folder
spt_prj_data <- "data"

# Input data file paths - hourly UTCI temperature
# l50 = 50 locations, d100 = 100 days 
spn_era5_utci_hour_test <- file.path(
    spt_prj_data,
    "df_era5_utci_china_2019_2021_hour_test_l50d100.csv"
)
spn_era5_utci_hour_full <- file.path(
    spt_root_pire, "marco_laghi", "PrjCECReplicate", "clean_data",
    "df_era5_utci_china_2019_2021_hour.csv"
)
st_date_start <- "2019-01-01 00:00"

# Key and census data file paths
spn_key_loc_coord2county <- file.path(
    spt_prj_data, "df_key_loc_china_coord2county_2020.csv"
)
spn_key_demo_census <- file.path(
    spt_prj_data, "df_key_demo_china_census_2020.csv"
)
spn_census_county <- file.path(
    spt_prj_data, "df_china_census_county_2020.csv"
)

# Load data: either test (small sample) or full dataset -------------------------
bl_test <- TRUE
if (bl_test) {
    # If test file does not exist, generate it from source
    if (!file.exists(spn_era5_utci_hour_test)) {
        df_allhours <- read.csv(spn_era5_utci_hour_full, header = TRUE)
        dim(df_allhours)
        # Select 50 random rows from the dataframe, and keep all columns
        df_test <- df_allhours %>% sample_n(50)
        # Select the first 100 days, which is 100*24 hours
        # select the 100*24 + 2 columns of data
        df_test <- df_test %>% select(1:(100*24 + 2))
        write.csv(df_test, spn_era5_utci_hour_test, row.names = FALSE)
        cat("Test file generated at:", spn_era5_utci_hour_test, "\n")
    }
    # Load the test file
    df_allhours <- read.csv(spn_era5_utci_hour_test, header = TRUE)
} else {
    # Load all actual data
    df_allhours <- read.csv(spn_era5_utci_hour_full, header = TRUE)
}

# Converting data to temperate by hours/dates ---------------------
df_allhours_longer <- df_allhours %>%
    pivot_longer(
        cols = starts_with("day"),
        names_to = c("day"),
        names_pattern = paste0("day(.*)"),
        values_to = "temp"
    ) %>%
    rename(hour_of_year = day) %>%
    mutate(hour_of_year = as.numeric(hour_of_year) - 1)

# 6. Convert hours to utc time format
# Define origin time in UTC
de_utc_origin <- as.POSIXct(
    st_date_start,
    format = "%Y-%m-%d %H:%M", tz = "UTC"
)
# Construct time in hours for UTC time
df_allhours_longer <- df_allhours_longer %>%
    mutate(de_utc_time = as.POSIXct(
        hour_of_year * 60^2,
        origin = de_utc_origin, tz = "UTC"
    ))

# 7. Convert UTC time to time in Shanghai
df_allhours_longer <- df_allhours_longer %>%
    mutate(de_shanghai_time = as.POSIXct(
        de_utc_time,
        tz = "Asia/Shanghai"
    )) %>%
    mutate(
        de_shanghai_day = lubridate::day(de_shanghai_time),
        de_shanghai_month = lubridate::month(de_shanghai_time),
        de_shanghai_year = lubridate::year(de_shanghai_time)
        )

# Generate heat-spell statistics -------------------------
# 0. Define x, the temperature threshold, and y, the number of days in a heat spell. Let x = 30, y = 5 be default values. Define a function that takes as inputs the dataframe, x, and y.
# Now we describe the components of this function:
# 1. From the hourly data file, we compute daily max, max_day.
#   - we group by location_id, de_shanghai_year, de_shanghai_month, de_shanghai_day, and compute max(temp) for each group, to get max_day.
#   - in the resulting dataframe, each row is a day, rather than an hour, name the dataframe df_daily_max, with columns location_id, de_shanghai_year, de_shanghai_month, de_shanghai_day.
# 2. We consider if daily max exceeds bounds max_day >= X, DAILY-MAX-EXCEED-X: 0s and 1s created, 1 if max_day >= x.
#   - we create a new column DAILY-MAX-EXCEED-X, which is 1 if max_day >= x, and 0 otherwise.
# 3. Sort by days, and count number of connected 1s in each set, for each connected group of DAILY-MAX-EXCEED-X, compute total connects 1s, DAILY-MAX-EXCEED-X-DAYS.
#  - we sort the data by location_id, de_shanghai_year, de_shanghai_month, de_shanghai_day.
#  - When DAILY-MAX-EXCEED-X = 1 over consecutive days, we count the number of consecutive days, and assign that number to a new column DAILY-MAX-EXCEED-X-DAYS for each day in that group. For example, if we have 1s for 7 consecutive days, then for those 7 days, DAILY-MAX-EXCEED-X-DAYS = 7.
# 4. Generate HEAT-SPELL-EXCEED-X-FOR-Y-DAYS = 0s and 1s again, if DAILY-MAX-EXCEED-X-DAYS >= y.
# 5. Within each location_id, count the portion of days where HEAT-SPELL-EXCEED-X-FOR-Y-DAYS = 1, and store that as a new column HEAT-SPELL-EXCEED-X-FOR-Y-DAYS-PORTION.
#  - the resulting dataframe is now specific to each location_id
#  - the resulting dataframe has columns location_id, temperature-threshold (for x value), consecutive-days-thresholds (for y value), HEAT-SPELL-EXCEED-X-FOR-Y-DAYS-PORTION,
fn_heat_spell_analysis <- function(df, x = 30, y = 5) {
    # Step 1: Compute daily max temperature
    df_daily_max <- df %>%
        group_by(location_id, de_shanghai_year, de_shanghai_month, de_shanghai_day) %>%
        summarise(max_day = max(temp), .groups = "drop")

    # Step 2: Create indicator for exceeding threshold
    df_daily_max <- df_daily_max %>%
        mutate(daily_max_exceed_x = as.integer(max_day >= x))

    # Step 3: Count consecutive days exceeding threshold
    df_daily_max <- df_daily_max %>%
        arrange(location_id, de_shanghai_year, de_shanghai_month, de_shanghai_day) %>%
        group_by(location_id) %>%
        mutate(
            group_id = cumsum(daily_max_exceed_x == 0),
            daily_max_exceed_x_days = ifelse(
                daily_max_exceed_x == 1,
                ave(daily_max_exceed_x, group_id, FUN = sum),
                0
            )
        ) %>%
        ungroup() %>%
        select(-group_id)

    # Step 4: Create heat spell indicator
    df_daily_max <- df_daily_max %>%
        mutate(heat_spell_exceed_x_for_y_days = as.integer(daily_max_exceed_x_days >= y))

    # Step 5: Calculate proportion within each location
    df_result <- df_daily_max %>%
        group_by(location_id) %>%
        summarise(
            heat_spell_exceed_x_for_y_days_portion = mean(heat_spell_exceed_x_for_y_days),
            temperature_threshold = x,
            consecutive_days_threshold = y,
            .groups = "drop"
        )

    return(list(df_result = df_result, df_daily_max = df_daily_max))
}

df_heat_spell_test <- fn_heat_spell_analysis(df_allhours_longer, x = 20, y = 5)$df_result
print(df_heat_spell_test)


# Generate share of time statistics-------------------------
# 0. Define x, the temperature threshold. Define a function that takes as inputs the dataframe, and x.
# Now we describe the components of this function:
# 1. From the hourly data file, we compute if hourly temperature exceeds bound x, HOURLY-VAL-EXCEED-X: 0s and 1s created, 1 if temp >= x.
# 2. Within each location_id, count the portion of hours where HOURLY-VAL-EXCEED-X = 1, and store that as a new column TEMP-EXCEED-X-PORTION.
#  - the resulting dataframe is now specific to each location_id
#  - the resulting dataframe has columns location_id, temperature-threshold (for x value), TEMP-EXCEED-X-PORTION,
fn_temp_exceed_analysis <- function(df, x = 30) {
    # Step 1: Create indicator for exceeding threshold
    df_hourly <- df %>%
        mutate(hourly_val_exceed_x = as.integer(temp >= x))

    # Step 2: Calculate proportion within each location
    df_result <- df_hourly %>%
        group_by(location_id) %>%
        summarise(
            temp_exceed_x_portion = mean(hourly_val_exceed_x),
            temperature_threshold = x,
            .groups = "drop"
        )

    return(df_result)
}

df_temp_exceed_test <- fn_temp_exceed_analysis(df_allhours_longer, x = 20)
print(df_temp_exceed_test)

# Multiple heat spell definition and temperature threshold combinations -------------------------
# Now we want to analyze the relationship between the `temp_exceed_x_portion` and `heat_spell_exceed_x_for_y_days_portion` variables across different `location_id`.
# Part A:
# 1. We consider an array of x values, ar_x_temp_thresholds, and an array of y values, ar_y_consecutive_days_thresholds.
# 2. We loop over the x and y values, and call the `fn_heat_spell_analysis` for each combination of x and y, to get the resulting dataframes for each combination.
# 3. We stack the resulting df_result dataframes together, storing different x and y values in `temperature_threshold` and `consecutive_days_threshold` columns, to get a combined dataframe df_heat_spell_results.
# Part B:
# 1. We loop over the x values, and call the `fn_temp_exceed_analysis` for each x value, to get the resulting dataframes for each x value.
# 2. We stack the resulting dataframes together, storing different x values in `temperature_threshold` column, to get a combined dataframe df_temp_exceed_results.
# Part C:
# 1. We reshape the `df_heat_spell_results` dataframe from part A, so that the `consecutive_days_threshold` values are reshaped from row to column values, to get a dataframe df_heat_spell_results_wide, with columns location_id, temperature_threshold, and then one column for each `consecutive_days_threshold` value, with the corresponding `heat_spell_exceed_x_for_y_days_portion` values.
# 2. We merge the `df_heat_spell_results_wide` dataframe with the `df_temp_exceed_results` dataframe from part B, by location_id and temperature_threshold, to get a combined dataframe df_combined, with columns location_id, temperature_threshold, temp_exceed_x_portion, and then one column for each `consecutive_days_threshold` value, with the corresponding `heat_spell_exceed_x_for_y_days_portion` values.

# Part A: Heat spell analysis across multiple thresholds
ar_x_temp_thresholds <- c(26, 32, 38)
ar_y_consecutive_days_thresholds <- c(1, 3, 5)

df_heat_spell_results <- expand.grid(
    x = ar_x_temp_thresholds,
    y = ar_y_consecutive_days_thresholds
) %>%
    pmap_df(~ fn_heat_spell_analysis(df_allhours_longer, x = ..1, y = ..2)$df_result)

# Part B: Temperature exceed analysis across multiple thresholds
df_temp_exceed_results <- map_df(
    ar_x_temp_thresholds,
    ~ fn_temp_exceed_analysis(df_allhours_longer, x = .)
)

# Part C: Reshape and merge
df_heat_spell_results_wide <- df_heat_spell_results %>%
    pivot_wider(
        names_from = consecutive_days_threshold,
        values_from = heat_spell_exceed_x_for_y_days_portion,
        names_prefix = "heat_spell_days_"
    )

df_combined <- df_heat_spell_results_wide %>%
    left_join(
        df_temp_exceed_results,
        by = c("location_id", "temperature_threshold")
    )

print(df_combined)

# From grid-values to county-average statistics ---------------------------------------------
# Now we compute county average statistics.
# 1. Import in three files:
#  - `df_key_loc_china_coord2county.csv` as df_key_loc_coord2county keep location_id and GBCounty columns.
# 2. Reshape df_combined to wide, df_combined_wide, so that:
#  - identifying column is `location_id`
#  - reshape `temperature_threshold` values from row to column names, with column names `temp_threshold_X` for each x value
#  - keep as values the values from the `heat_spell_days_Y` columns as well as the `temp_exceed_x_portion` column, with column names `heat_spell_days_Y_X` and `temp_exceed_X`, respectively, for each x and y value combination, now the temperature thresholds values are reshaped from row to column values.
# 3. Merge df_key_loc_coord2county with df_combined_wide:
#  - Get unique `location_id` from df_combined_wide
#  - Filter df_key_loc_coord2county to only include rows with the `location_id` values from the previous step, to get df_key_loc_coord2county_filtered
#  - Merge df_key_loc_coord2county_filtered with df_combined_wide by `location_id`, to get df_combined_wide_with_county, multiple rows in `df_key_loc_coord2county_filtered` are matched with the same row in `df_combined_wide`. Report merging statistics.
# 4. For the merged file, generate statistics by GBCounty:
# - group by GBCounty, and compute the mean of all the data columns: the `heat_spell_days_Y_X` and `temp_exceed_X` columns for each GBCounty, to get df_county_stats, with columns GBCounty, and then one column for each `heat_spell_days_Y_X` and `temp_exceed_X` variable, with the corresponding mean values for each GBCounty.
# 5. Reshape from wide to long, df_county_stats_long:
#   - each row is `GBCounty` and `temperature_threshold` combination, with columns GBCounty, temperature_threshold, heat_spell_days_Y_X, temp_exceed_X. Basically the same columns as the columns originally in `df_combined`, but now we have `GBCounty` instead of `location_id`, and all the data columns have been aggregated to the county level.

# Part 1: Import location to county mapping
df_key_loc_coord2county <- read_csv(spn_key_loc_coord2county) %>%
    select(location_id, GBCounty)

# Part 2: Reshape df_combined to wide format
df_combined_wide <- df_combined %>%
    pivot_wider(
        id_cols = location_id,
        names_from = temperature_threshold,
        values_from = c(temp_exceed_x_portion, heat_spell_days_1, heat_spell_days_3, heat_spell_days_5),
        names_glue = "{.value}_{temperature_threshold}"
    )

# Part 3: Merge with county information
ar_location_ids <- unique(df_combined_wide$location_id)
df_key_loc_coord2county_filtered <- df_key_loc_coord2county %>%
    filter(location_id %in% ar_location_ids)

df_combined_wide_with_county <- df_key_loc_coord2county_filtered %>%
    left_join(df_combined_wide, by = "location_id")

cat("Merge statistics:\n")
cat("Rows in df_key_loc_coord2county_filtered:", nrow(df_key_loc_coord2county_filtered), "\n")
cat("Rows in df_combined_wide:", nrow(df_combined_wide), "\n")
cat("Rows in df_combined_wide_with_county:", nrow(df_combined_wide_with_county), "\n")

# Part 4: Compute county-level statistics
df_county_stats <- df_combined_wide_with_county %>%
    group_by(GBCounty) %>%
    summarise(across(-c(location_id), list(mean = mean), na.rm = TRUE), .groups = "drop")

# Part 5: Reshape from wide to long
df_county_stats_long <- df_county_stats %>%
    pivot_longer(
        cols = -GBCounty,
        names_to = c(".value", "temperature_threshold"),
        names_pattern = "(.+)_([0-9]+)_mean"
    ) %>%
    rename_with(~ gsub("_mean$", "", .), starts_with("temp_exceed")) %>%
    rename_with(~ gsub("_mean$", "", .), starts_with("heat_spell"))


# Bringing in information on population weights ---------------------------------------------
# Now we want to contrast and compare.
# 1. Import in files:
#  - `df_key_demo_china_census.csv` as df_key_demo_census
#  - `df_china_census_county.csv` as df_census_county
# 2. In `df_key_demo_census`, filter by age_group_m3 == "0_14", get as array all unique popgrp_key as `ar_popgrp_key_0_14`.
# 3. in `df_census_county`:
#  - Get the unique `GBCounty` values from `df_county_stats_long`, and store that as an array `ar_gbcounty_in_stats`.
#  - filter for the subset of rows with `GBCounty` values that are in the `GBCounty` column of `df_county_stats_long`, to get df_census_county_filtered.
#  - select the GBCounty column and columns containing popgrp_key values in `ar_popgrp_key_0_14`
#  - for each row, sum across the `popgrp_key` columns to get total population for age group 0-14, and store that in a new column `pop_0_14`.
# 4. Merge df_county_stats_long with the pop_0_14 values from what was created in step 3.

# Part 1: Import files
df_key_demo_census <- read_csv(spn_key_demo_census)
df_census_county <- read_csv(spn_census_county)

# Part 2: Get popgrp_key values for age group 0-14
ar_popgrp_key_0_14 <- df_key_demo_census %>%
    filter(age_group_m3 == "0_14") %>%
    pull(popgrp_key) %>%
    unique()

# Part 3: Process census data
ar_gbcounty_in_stats <- unique(df_county_stats_long$GBCounty)

df_census_county_filtered <- df_census_county %>%
    filter(GBCounty %in% ar_gbcounty_in_stats) %>%
    select(GBCounty, all_of(ar_popgrp_key_0_14)) %>%
    mutate(pop_0_14 = rowSums(across(all_of(ar_popgrp_key_0_14)), na.rm = TRUE))

# Part 4: Merge with county stats
df_county_stats_long <- df_county_stats_long %>%
    left_join(
        df_census_county_filtered %>% select(GBCounty, pop_0_14),
        by = "GBCounty"
    )

# Part 5: Reweight
# generate a weight column, with weights equal to pop_0_14 / sum(pop_0_14)
df_county_stats_long <- df_county_stats_long %>%
    mutate(weight = pop_0_14 / sum(pop_0_14, na.rm = TRUE))

# Compute heat-speed and share-time correlations -------------------------
# Compute correlations between the temp_exceed_x_portion variable and the heat_spell_days_Y_X variables, weighted by the population weights, for each temperature threshold and consecutive days threshold combination.
# 1. Reshape from wide to even longer so that each `heat_spell_days_Y` is reshaped to long with Y stored as a variable, with variable name `consecutive_days_threshold`, and the corresponding values stored in a column `heat_spell_days_portion`
# 2. Compute population weighted correlation using the `weight` column for combinations of `temperature_threshold` and `consecutive_days_threshold`, between the `temp_exceed_x_portion` variable and the `heat_spell_days_portion` variable, to get a dataframe df_correlations with columns temperature_threshold, consecutive_days_threshold, and correlation.

# Part 1: Reshape to longer format
df_county_stats_long_longer <- df_county_stats_long %>%
    pivot_longer(
        cols = starts_with("heat_spell_days_"),
        names_to = "consecutive_days_threshold",
        names_pattern = "heat_spell_days_([0-9]+)",
        values_to = "heat_spell_days_portion"
    )

# Part 2: Compute weighted correlations
df_correlations <- df_county_stats_long_longer %>%
    filter(!is.na(temp_exceed_x_portion) & !is.na(heat_spell_days_portion) & !is.na(weight)) %>%
    group_by(temperature_threshold, consecutive_days_threshold) %>%
    summarize(
        correlation = tryCatch(
            stats::cov.wt(
                cbind(temp_exceed_x_portion, heat_spell_days_portion),
                wt = weight,
                cor = TRUE
            )$cor[1, 2],
            error = function(e) NA_real_
        ),
        .groups = "drop"
    )
# print final results
print("Correlations:")
print(df_correlations)

# Visualize correlation -------------------------
# - Present three scatter plot of `temp_exceed_x_portion` and `heat_spell_days_portion`
# 0 one for each different `temperature_threshold` value.
# - Each point is a GBCounty
# - Facet by `consecutive_days_threshold`
# - Add a title and axis labels to the plot
# - Generate linear regression line in each plot, weighted using the `weight` column, and add the correlation value to the plot as text annotation.
# - Also show 10 grid lines horizontally and vertically.
# Create visualization
p <- df_county_stats_long_longer %>%
    filter(!is.na(temp_exceed_x_portion) & !is.na(heat_spell_days_portion)) %>%
    ggplot(aes(x = temp_exceed_x_portion, y = heat_spell_days_portion, size = weight)) +
    geom_point(alpha = 0.6) +
    geom_smooth(aes(weight = weight), method = "lm", se = TRUE, color = "red") +
    facet_grid(consecutive_days_threshold ~ temperature_threshold, labeller = labeller(
        temperature_threshold = label_both,
        consecutive_days_threshold = label_both
    )) +
    labs(
        title = "Relationship between Temperature Exceed Portion and Heat Spell Days",
        x = "Proportion of Hours Exceeding Temperature Threshold",
        y = "Proportion of Days in Heat Spell",
        size = "Population Weight"
    ) +
    theme_minimal() +
    theme(
        strip.text = element_text(size = 9),
        panel.grid.minor = element_blank()
    )

# Add correlation annotations
df_correlations_for_annotation <- df_correlations %>%
    mutate(
        temperature_threshold = as.character(temperature_threshold),
        consecutive_days_threshold = as.character(consecutive_days_threshold)
    )

p <- p + geom_text(
    data = df_correlations_for_annotation,
    aes(label = paste("r =", round(correlation, 3)), x = Inf, y = -Inf),
    hjust = 1.1, vjust = -0.5, size = 3, inherit.aes = FALSE
)

print(p)
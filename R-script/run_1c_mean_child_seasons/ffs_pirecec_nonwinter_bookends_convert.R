# Implements Part 2 of https://github.com/ClimateInequality/PrjCEC/issues/35
# building on `R-script/run_1b_mean_child_6t22/ffs_pierecec_daytimeonly_convert.R`

# 1. Load libraries ------
source("R-script/ffs_pirecec_support.R")

# Load data managment and statistics packages
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)

# Day time hour parameters, inclusive boundaries 6 to 22
ar_it_month_arp2sep <- c(4,5,6,7,8,9)
ar_it_month_oct2mar <- c(1,2,3,10,11,12)
verbose <- TRUE
bl_main_save <- TRUE

# 2. Get paths, constructed in `R-script/ffs_pirecec_support.R`
ls_paths <- ffs_pirecec_path_run()
spt_cec_sandbox_r_func <- ls_paths$spt_cec_sandbox_r_func
spt_pire_team_kf <- ls_paths$spt_pire_team_kf
# Data input folder
spt_path_data <- file.path(spt_pire_team_kf, "clean_data", fsep = .Platform$file.sep)
# Results/data output folder, store data to file
spt_path_out <- spt_path_data

# 3. Data file names -----
# it_file_ctr <- 1
for (it_file_ctr in c(1,2,3,4)) {
    if (it_file_ctr == 1) {
        st_file_envir <- "df_era5_utci_china_1989_1991_hour.csv"
        st_file_out_envir <- "df_era5_utci_china_1989_1991_hour_apr2sep.csv"
        st_date_start <- "1989-01-01 00:00"
    } else if (it_file_ctr == 2) {
        st_file_envir <- "df_era5_utci_china_2019_2021_hour.csv"
        st_file_out_envir <- "df_era5_utci_china_2019_2021_hour_apr2sep.csv"
        st_date_start <- "2019-01-01 00:00"
    } else if (it_file_ctr == 3) {
        st_file_envir <- "df_era5_utci_china_1989_1991_hour.csv"
        st_file_out_envir <- "df_era5_utci_china_1989_1991_hour_oct2mar.csv"
        st_date_start <- "1989-01-01 00:00"
    } else if (it_file_ctr == 4) {
        st_file_envir <- "df_era5_utci_china_2019_2021_hour.csv"
        st_file_out_envir <- "df_era5_utci_china_2019_2021_hour_oct2mar.csv"
        st_date_start <- "2019-01-01 00:00"
    }

    # 4. Read in file, all hours data
    # prior operations had converted "hour" to integer index by days.
    spn_path_data_all_hours <- file.path(spt_path_data, st_file_envir, fsep = .Platform$file.sep)
    df_allhours <- read_csv(spn_path_data_all_hours)

    # 5. Pivot longer
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
        mutate(de_shanghai_month = lubridate::month(de_shanghai_time))

    if (verbose) {
        print(glue::glue("F-812763, S1"))
        # Test averaging across hours and
        print(df_allhours_longer %>%
            group_by(de_shanghai_month) %>%
            summarize(temp_avg = mean(temp), temp_std = sd(temp)), n = 30)
        # Review information
        print(df_allhours_longer, n = 100)
    }

    # 8. Cut non daytime hours
    if (it_file_ctr %in% c(1,2) ) {
        df_allhours_daytime <- df_allhours_longer %>%
            filter(
                de_shanghai_month %in% ar_it_month_arp2sep
            )
    } else if (it_file_ctr %in% c(3,4)) {
        df_allhours_daytime <- df_allhours_longer %>%
            filter(
                de_shanghai_month %in% ar_it_month_oct2mar
            )
    }

    # 9. reindex hour of year
    df_allhours_daytime <- df_allhours_daytime %>%
        group_by(location_id) %>%
        arrange(de_utc_time, .by_group = TRUE) %>%
        mutate(day = row_number())
    if (verbose) {
        print(glue::glue("F-812763, S2"))
        print(max(df_allhours_daytime$day))
    }

    # 10. select and reshape to wide
    df_allhours_daytime_wide <- df_allhours_daytime %>%
        select(location_id, day, temp) %>%
        pivot_wider(
            id_cols = c("location_id"),
            names_from = day,
            names_prefix = "day",
            values_from = temp
        )

    # 11. save
    if (bl_main_save) {
        spn_path <- file.path(
            spt_path_out, st_file_out_envir,
            fsep = .Platform$file.sep
        )
        write_csv(df_allhours_daytime_wide, spn_path)
        print(glue::glue("F-812763, S3"))
        print(glue::glue("File saved: {spn_path}"))
    }
}

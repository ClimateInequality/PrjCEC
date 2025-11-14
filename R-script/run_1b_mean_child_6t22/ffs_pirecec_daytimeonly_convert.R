# Implements Part 1 of https://github.com/ClimateInequality/PrjCEC/issues/35

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
it_start_hour_day <- 6
it_end_hour_day <- 21
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
for (it_file_ctr in seq(1, 2)) {
    if (it_file_ctr == 1) {
        st_file_envir <- "df_era5_utci_china_1990_hour.csv"
        st_file_out_envir <- "df_era5_utci_china_1990_hour6t22.csv"
        st_date_start <- "1990-01-01 00:00"
    } else if (it_file_ctr == 2) {
        st_file_envir <- "df_era5_utci_china_2020_hour.csv"
        st_file_out_envir <- "df_era5_utci_china_2020_hour6t22.csv"
        st_date_start <- "2020-01-01 00:00"
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
        mutate(de_shanghai_hour = lubridate::hour(de_shanghai_time))

    if (verbose) {
        print(glue::glue("F-91031, S1"))
        # Test averaging across hours and
        print(df_allhours_longer %>%
            group_by(de_shanghai_hour) %>%
            summarize(temp_avg = mean(temp), temp_std = sd(temp)), n = 30)
        # Review information
        print(df_allhours_longer, n = 100)
    }

    # 8. Cut non daytime hours
    df_allhours_daytime <- df_allhours_longer %>%
        filter(
            de_shanghai_hour >= it_start_hour_day,
            de_shanghai_hour <= it_end_hour_day
        )

    # 9. reindex hour of year
    df_allhours_daytime <- df_allhours_daytime %>%
        group_by(location_id) %>%
        arrange(de_utc_time, .by_group = TRUE) %>%
        mutate(day = row_number())
    if (verbose) {
        print(glue::glue("F-91031, S2"))
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
        print(glue::glue("F-91031, S3"))
        print(glue::glue("File saved: {spn_path}"))
    }
}


# ---------------------------------------------------------------
# # Alternative very slow method below:
# # day1 = hour 1 = 1/1 12am
# # day8760 = hour 8760 = 12/31 11pm
# df_allhours_longer <- df_allhours_longer %>%
#     mutate(
#         day = ceiling(hour_of_year / 24) - 1,
#         hour = (hour_of_year %% 24) - 1
#     ) %>%
#     mutate(hour = case_when(hour == 0 ~ 24, TRUE ~ hour))

# # 6. "Day" to "hour" conversion, and generate date
# # day1 = hour 1 = 1/1 12am
# # day8760 = hour 8760 = 12/31 11pm
# df_allhours_longer <- df_allhours_longer %>%
#     rename(hour_of_year = day) %>%
#     mutate(hour_of_year = as.numeric(hour_of_year)) %>%
#     mutate(
#         day = ceiling(hour_of_year / 24) - 1,
#         hour = (hour_of_year %% 24) - 1
#     ) %>%
#     mutate(hour = case_when(hour == 0 ~ 24, TRUE ~ hour))

# # 7. Generate date for year and time string
# df_allhours_longer <- df_allhours_longer %>%
#     mutate(date = as.Date(day, origin = "1990-01-01"))

# df_allhours_longer <- df_allhours_longer %>%
#     mutate(
#         time_utc = as.POSIXct(
#             paste0(date, " ", hour, ":00:00"),
#             format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
#         )
#     )

# # 9. convert to shanghai time
# df_allhours_longer <- df_allhours_longer %>%
#     mutate(
#         time_shanghai = as.POSIXlt(time_utc, tz = "Asia/Shanghai", usetz = TRUE)
#     )

# # max(df_allhours_longer %>% distinct(hour_of_year))
# # max(df_allhours_longer %>% distinct(day))
# # print(df_allhours_longer %>% distinct(date), n=400)


# print(df_allhours_longer, n = 60)

# #
# print(dim(df_allhours))
# str(df_allhours)
# # colnames(df_allhours)

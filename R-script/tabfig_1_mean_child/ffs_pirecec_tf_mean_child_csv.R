# Implement step 1 of https://github.com/ClimateInequality/PrjCEC/issues/36
# load library
library(readr)
library(dplyr)
library(tidyr)

# File names
bl_main_save <- TRUE
spt_path_datares <- file.path("data-res", fsep = .Platform$file.sep)
spt_path_res <- file.path("res", "res_mean_child", fsep = .Platform$file.sep)
ls_st_files <- c(
    "dm_90h6t22_share_gr_age_group_m3is0_14_all_locations.csv",
    "dm_90h24_share_gr_age_group_m3is0_14_all_locations.csv",
    "dm_20h6t22_share_gr_age_group_m3is0_14_all_locations.csv",
    "dm_20h24_share_gr_age_group_m3is0_14_all_locations.csv",
    "dm_90apr2sep_share_gr_age_group_m3is0_14_all_locations.csv",
    "dm_90oct2mar_share_gr_age_group_m3is0_14_all_locations.csv",
    "dm_20apr2sep_share_gr_age_group_m3is0_14_all_locations.csv",
    "dm_20oct2mar_share_gr_age_group_m3is0_14_all_locations.csv"
)

# 1. Generate and load four data files with the same structure for time-type/year 3 by 2 combos
spn_path <- file.path(spt_path_datares, ls_st_files[1], fsep = .Platform$file.sep)
df_dm_90h6t22 <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[2], fsep = .Platform$file.sep)
df_dm_90h24 <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[3], fsep = .Platform$file.sep)
df_dm_20h6t22 <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[4], fsep = .Platform$file.sep)
df_dm_20h24 <- read_csv(spn_path)
# seasons files
spn_path <- file.path(spt_path_datares, ls_st_files[5], fsep = .Platform$file.sep)
df_dm_90apr2sep <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[6], fsep = .Platform$file.sep)
df_dm_90oct2mar <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[7], fsep = .Platform$file.sep)
df_dm_20apr2sep <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[8], fsep = .Platform$file.sep)
df_dm_20oct2mar <- read_csv(spn_path)

# 2. Stack outputs together to single file
df_all <- bind_rows(
    df_dm_90h6t22 %>%
        mutate(
            daily_time = "1G_6t22",
            year = 1990
        ),
    df_dm_90h24 %>%
        mutate(
            daily_time = "0G_24",
            year = 1990
        ),
    df_dm_20h6t22 %>%
        mutate(
            daily_time = "1G_6t22",
            year = 2020
        ),
    df_dm_20h24 %>%
        mutate(
            daily_time = "0G_24",
            year = 2020
        ),
    df_dm_90apr2sep %>%
        mutate(
            daily_time = "2G_apr2sep",
            year = 1990
        ),
    df_dm_90oct2mar %>%
        mutate(
            daily_time = "3G_oct2mar",
            year = 1990
        ),
    df_dm_20apr2sep %>%
        mutate(
            daily_time = "2G_apr2sep",
            year = 2020
        ),
    df_dm_20oct2mar %>%
        mutate(
            daily_time = "3G_oct2mar",
            year = 2020
        )
) %>%
    select(
        # all 0 - 14 children
        -popgrp, -age_group_m3, -stats_type,
        # overall average (not children)
        -expo_overall_mean,
        # no location differences, national average
        -all_locations
    ) %>%
    rename(
        cdf = expo_grp_mean,
        utci_thres = fl_temp_bound
    )

# 3. Generate pmf from cdf
df_all <- df_all %>%
    group_by(daily_time, year) %>%
    arrange(utci_thres, .by_group = TRUE) %>%
    mutate(pmf = lag(cdf) - (cdf))

# 4. Export file with pmf, cdf, time, threshold, time-type for Figure A.
df_all_fig_a <- df_all %>%
    select(utci_thres, cdf, pmf, year) %>% 
    rename(Year = year, utci = utci_thres)
if (bl_main_save) {
    spn_path <- file.path(
        spt_path_res, "fig_a_data.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_all_fig_a, spn_path)
    print(glue::glue("F-203898, S2"))
    print(glue::glue("File saved: {spn_path}"))
}

# 5. Reshape cdf data time/year from long to wide
df_all_wide <- df_all %>%
    select(-pmf, -popgrp_mass) %>%
    pivot_wider(
        id_cols = c("daily_time", "utci_thres"),
        names_from = year,
        names_prefix = "year_",
        values_from = cdf
    )

# 6. Based on cdf file Generate percentage points changes and percentage change columns, out as input for Figure B.
# Do not save
bl_save_b_data <- FALSE
df_all_wide_fig_b <- df_all_wide %>%
    mutate(
        cdf_percpoint_chg = year_2020 - year_1990,
        cdf_percent_chg = (year_2020 - year_1990) / year_1990
    )
if (bl_main_save && bl_save_b_data) {
    spn_path <- file.path(
        spt_path_res, "fig_b_data.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_all_wide_fig_b, spn_path)
    print(glue::glue("F-203898, S2"))
    print(glue::glue("File saved: {spn_path}"))
}

# 7. Reshape time-type (all-hours vs day-time) from long to wide, out as input for Table A.
df_all_wide_tab_a <- df_all_wide_fig_b %>%
    pivot_wider(
        id_cols = c("utci_thres"),
        names_from = daily_time,
        names_prefix = "time_",
        values_from = c(
            year_1990, year_2020, cdf_percpoint_chg, cdf_percent_chg
        )
    ) %>%
    select(
        utci_thres,
        contains("24"),
        contains("6t22")
    )
if (bl_main_save) {
    spn_path <- file.path(
        spt_path_res, "tab_a_24vsday_data.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_all_wide_tab_a, spn_path)
    print(glue::glue("F-203898, S3"))
    print(glue::glue("File saved: {spn_path}"))
}

# 8. Reshape reasons from long to wide, out as input for Table B.
df_all_wide_tab_b <- df_all_wide_fig_b %>%
    pivot_wider(
        id_cols = c("utci_thres"),
        names_from = daily_time,
        names_prefix = "time_",
        values_from = c(
            year_1990, year_2020, cdf_percpoint_chg, cdf_percent_chg
        )
    ) %>%
    select(
        utci_thres,
        contains("apr2sep"),
        contains("oct2mar")
    )
if (bl_main_save) {
    spn_path <- file.path(
        spt_path_res, "tab_b_season_data.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_all_wide_tab_b, spn_path)
    print(glue::glue("F-203898, S4"))
    print(glue::glue("File saved: {spn_path}"))
}

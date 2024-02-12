# Implement step 1 of https://github.com/ClimateInequality/PrjCEC/issues/38
# load library
library(readr)
library(dplyr)
library(tidyr)

# 0. File names and loading
bl_main_save <- TRUE
spt_path_datares <- file.path("data-res", fsep = .Platform$file.sep)
spt_path_res <- file.path("res", "res_decompose", fsep = .Platform$file.sep)
ls_st_files <- c(
    "dm_90h24_share_gr_age_group_m3is0_14_all_locations.csv",
    "dm_20h24_share_gr_age_group_m3is0_14_all_locations.csv",
    "dmloc_90region_share_gr_age_group_m3is0_14_region_name.csv",
    "dmloc_20region_share_gr_age_group_m3is0_14_region_name.csv",
    "dmloc_20pop90utci_share_gr_age_group_m3is0_14_all_locations.csv",
    "dmloc_20pop90utciregion_share_gr_age_group_m3is0_14_region_name.csv",
    "dmloc_90pop20utci_share_gr_age_group_m3is0_14_all_locations.csv",
    "dmloc_90pop20utciregion_share_gr_age_group_m3is0_14_region_name.csv"
)
spn_path <- file.path(spt_path_datares, ls_st_files[1], fsep = .Platform$file.sep)
df_dm_90h24 <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[2], fsep = .Platform$file.sep)
df_dm_20h24 <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[3], fsep = .Platform$file.sep)
df_dml_90region <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[4], fsep = .Platform$file.sep)
df_dml_20region <- read_csv(spn_path)
# seasons files
spn_path <- file.path(spt_path_datares, ls_st_files[5], fsep = .Platform$file.sep)
df_dml_20pop90utci <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[6], fsep = .Platform$file.sep)
df_dml_20pop90utciregion <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[7], fsep = .Platform$file.sep)
df_dml_90pop20utci <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[8], fsep = .Platform$file.sep)
df_dml_90pop20utciregion <- read_csv(spn_path)

# 1. Combine jointly mean results and decompose files. Decompose type is "yeartype", year to string, year var also yeartype
df_all <- bind_rows(
    df_dm_90h24 %>%
        mutate(
            loc_level = "national",
            yeartype = "90pop90utci"
        ),
    df_dm_20h24 %>%
        mutate(
            loc_level = "national",
            yeartype = "20pop20utci"
        ),
    df_dml_90region %>%
        mutate(
            loc_level = "regional",
            yeartype = "90pop90utci"
        ),
    df_dml_20region %>%
        mutate(
            loc_level = "regional",
            yeartype = "20pop20utci"
        ),
    df_dml_20pop90utci %>%
        mutate(
            loc_level = "national",
            yeartype = "20pop90utci"
        ),
    df_dml_20pop90utciregion %>%
        mutate(
            loc_level = "regional",
            yeartype = "20pop90utci"
        ),
    df_dml_90pop20utci %>%
        mutate(
            loc_level = "national",
            yeartype = "90pop20utci"
        ),
    df_dml_90pop20utciregion %>%
        mutate(
            loc_level = "regional",
            yeartype = "90pop20utci"
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

# 2. Reshape yeartype from long to wide
df_all_wide <- df_all %>%
    select(-popgrp_mass) %>%
    pivot_wider(
        id_cols = c("loc_level", "region_name", "utci_thres"),
        names_from = yeartype,
        names_prefix = "yt_",
        values_from = cdf
    )

# 3. Generate points changes, and share of gap
df_all_wide_fig_a <- df_all_wide %>%
    mutate(
        cdf_percpoint_chg_20_v90 = yt_20pop20utci - yt_90pop90utci,
        cdf_percpoint_chg_20utci_v90 = yt_90pop20utci - yt_90pop90utci,
        cdf_percpoint_chg_20pop_v90 = yt_20pop90utci - yt_90pop90utci,
    ) %>%
    mutate(
        cdf_percent_chg_20utci_v90 = cdf_percpoint_chg_20utci_v90 / cdf_percpoint_chg_20_v90,
        cdf_percent_chg_20pop_v90 = cdf_percpoint_chg_20pop_v90 / cdf_percpoint_chg_20_v90
    )
if (bl_main_save) {
    spn_path <- file.path(
        spt_path_res, "fig_a_data.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_all_wide_fig_a, spn_path)
    print(glue::glue("F-196308, S1"))
    print(glue::glue("File saved: {spn_path}"))
}

# 4. National aggregate file for Table A
# 4. Regional file for Table B
df_all_wide_tab_a <- df_all_wide_fig_a %>%
    mutate(region_name = case_when(
        loc_level == "national" ~ "National",
        region_name == "Eastern" ~ "Eastern region",
        region_name == "Northeastern" ~ "Northeastern region",
        TRUE ~ region_name
    )) %>%
    select(
        region_name,
        utci_thres,
        contains("90pop90utci"),
        contains("20pop20utci"),
        contains("20_v90"),
        contains("20utci"),
        contains("20pop"),
    )
if (bl_main_save) {
    spn_path <- file.path(
        spt_path_res, "tab_a_nationalregional_data.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_all_wide_tab_a, spn_path)
    print(glue::glue("F-196308, S2"))
    print(glue::glue("File saved: {spn_path}"))
}

# 8. Reshape reasons from long to wide, out as input for Table B.
df_all_wide_tab_b <- df_all_wide_fig_a %>%
    filter(loc_level == "regional") %>%
    select(
        region_name,
        utci_thres,
        contains("90pop90utci"),
        contains("20pop20utci"),
        contains("20_v90"),
        contains("20utci"),
        contains("20pop"),
    )
if (bl_main_save) {
    spn_path <- file.path(
        spt_path_res, "tab_b_region_data.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_all_wide_tab_b, spn_path)
    print(glue::glue("F-196308, S3"))
    print(glue::glue("File saved: {spn_path}"))
}

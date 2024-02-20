# Implement step 1 of https://github.com/ClimateInequality/PrjCEC/issues/37
# load library
library(readr)
library(dplyr)
library(tidyr)

# File names
bl_main_save <- TRUE
spt_path_datares <- file.path("data-res", fsep = .Platform$file.sep)
spt_path_res <- file.path("res", "res_region_prov", fsep = .Platform$file.sep)
ls_st_files <- c(
    "dmloc_90region_share_gr_age_group_m3is0_14_region_name.csv",
    "dmloc_90prov_share_gr_age_group_m3is0_14_Prov_En.csv",
    "dmloc_20region_share_gr_age_group_m3is0_14_region_name.csv",
    "dmloc_20prov_share_gr_age_group_m3is0_14_Prov_En_1990.csv"
)
ar_utci_higher_group <- c(32, 35, 38)
ar_utci_lower_group <- c(23, 26, 29)

# 1. Generate and load four data files with the same structure for region-or-province/year 2 by 2 combos
spn_path <- file.path(spt_path_datares, ls_st_files[1], fsep = .Platform$file.sep)
df_dml_90region <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[2], fsep = .Platform$file.sep)
df_dml_90prov <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[3], fsep = .Platform$file.sep)
df_dml_20region <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[4], fsep = .Platform$file.sep)
df_dml_20prov <- read_csv(spn_path) %>%
    rename(Prov_En = Prov_En_1990)

# 2. Stack outputs together to single file
df_all <- bind_rows(
    df_dml_90region %>%
        mutate(
            loc_level = "region",
            year = 1990
        ),
    df_dml_90prov %>%
        mutate(
            loc_level = "province",
            year = 1990
        ),
    df_dml_20region %>%
        mutate(
            loc_level = "region",
            year = 2020
        ),
    df_dml_20prov %>%
        mutate(
            loc_level = "province",
            year = 2020
        )
) %>%
    select(
        # all 0 - 14 children
        -popgrp, -age_group_m3, -stats_type,
        # overall average (not children)
        -expo_overall_mean,
    ) %>%
    rename(
        cdf = expo_grp_mean,
        utci_thres = fl_temp_bound
    ) %>%
    mutate(
        region_prov_name = case_when(
            region_name != "NA" ~ region_name,
            Prov_En != "NA" ~ Prov_En
        )
    ) %>%
    select(-region_name, -Prov_En)
# Province name adjustment:
# neimenggu for shorter name improved table fit, xizang for consistency with neimenggu
df_all <- df_all %>%
    mutate(region_prov_name = case_when(
        # region_prov_name == "Neimenggu" ~ "Inner Mongolia",
        region_prov_name == "Inner Mongolia" ~ "Neimenggu",
        # region_prov_name == "Xizang" ~ "Tibet",
        region_prov_name == "Tibet" ~ "Xizang",
        TRUE ~ region_prov_name
    ))

# 3. Reshape time/year from long to wide
df_all_wide <- df_all %>%
    select(-popgrp_mass) %>%
    pivot_wider(
        id_cols = c("loc_level", "region_prov_name", "utci_thres"),
        names_from = year,
        names_prefix = "year_",
        values_from = cdf
    )

# 4. Generate points changes and percentage change columns, out as input for Figure B adn C.
df_all_wide_fig_a <- df_all_wide %>%
    mutate(
        cdf_percpoint_chg = year_2020 - year_1990,
        cdf_percent_chg = (year_2020 - year_1990) / year_1990
    )
if (bl_main_save) {
    spn_path <- file.path(
        spt_path_res, "fig_a_data.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_all_wide_fig_a, spn_path)
    print(glue::glue("F-818152, S1"))
    print(glue::glue("File saved: {spn_path}"))
}

# 5. Reshape temperature levels from long to wide, locations as rows, out as input for Table A1 and A2 with separately selected temperature levels.
ar_it_a12 <- c(1, 2)
for (it_a12 in ar_it_a12) {
    if (it_a12 == 1) {
        ar_utci_group <- ar_utci_higher_group
        snm_file_out <- "tab_a1_strongheat_data.csv"
    } else if (it_a12 == 2) {
        ar_utci_group <- ar_utci_lower_group
        snm_file_out <- "tab_a2_moderateheat_data.csv"
    }
    df_all_wide_tab_a1or2 <- df_all_wide_fig_a %>%
        filter(utci_thres %in% ar_utci_group) %>%
        pivot_wider(
            id_cols = c("loc_level", "region_prov_name"),
            names_from = utci_thres,
            names_prefix = "utci_",
            values_from = c(
                year_1990, year_2020, cdf_percpoint_chg, cdf_percent_chg
            )
        )
    if (bl_main_save) {
        spn_path <- file.path(
            spt_path_res, snm_file_out,
            fsep = .Platform$file.sep
        )
        write_csv(df_all_wide_tab_a1or2, spn_path)
        print(glue::glue("F-818152, S2"))
        print(glue::glue("File saved: {spn_path}"))
    }
}

# Implements Part 1 of https://github.com/ClimateInequality/PrjCEC/issues/40
# Derived from `sandbox/R/run_full_dist/ffp_gpp_inequality_child_combine_cdf.R`
library(readr)
library(dplyr)
library(tidyr)

# library(tidyverse)
# 1. Merge input double-threhold files together, rows are share of time, columns are temp utci_thresholds
# 2. Filter to only consider a subst of share of time, at 0.05 intervals, 5, 10, 15, 20, 25, 30, 35, 40, 45. 9 groups.
# 3. Reshape utci_thresholds wide to long
# 4. 1 - all to see 1 - CDF.
# 5. Group by utci_thres, by share of days, arrange by filder counter
# 6. Percentage points change: Take diff with file counter as "t" (there are only two files, so difference across time)
# 7. Percentage change: Take ratio with file counter as "t" (there are only two files, so difference across time)
# 8. Rehsape long to wide so temp-utci_thresholds are rows and share-of-time are columns, keep year and percent and percpoint as rows

# Core parameters
# - col 1-3: 4, 8, 12 percent of time (2 weeks, 1 months, 6 weeks), note $(24*7*2)/(365*24)=0.03835616438$ and $100/24 = 4.16666666667$, the average of these are exactly 8 percent.
# - col 4-6: 16, 20, 24 percent of time (2 months, 10 weeks, 3 months)
# - col 7-9: 28, 32, 36 percent of time (14 weeks, 4 months, 16 weeks), note $(24*7*4*4)/(365*24)=0.30684931506$ $(100/24)*8 = 33.3333333333$, the average of these two numbers is exactly 32
ar_it_share_time <- seq(4, 36, by=4)/100

# 0. File names and loading
bl_main_save <- TRUE
spt_path_datares <- file.path("data-res", fsep = .Platform$file.sep)
spt_path_res <- file.path("res", "res_atrisk", fsep = .Platform$file.sep)
ls_st_files <- c(
  "dmdbl_90atrisk_share_gr_age_group_m3is0_14_GBCounty.csv",
  "dmdbl_20atrisk_share_gr_age_group_m3is0_14_GBCounty.csv"
)
spn_path <- file.path(spt_path_datares, ls_st_files[1], fsep = .Platform$file.sep)
df_dm_90atrisk <- read_csv(spn_path)
spn_path <- file.path(spt_path_datares, ls_st_files[2], fsep = .Platform$file.sep)
df_dm_20atrisk <- read_csv(spn_path)

# 1. Merge input double-threhold files together, rows are share of time, columns are temp utci_thresholds
df_all <- bind_rows(
  df_dm_90atrisk %>%
    mutate(year = 1990),
  df_dm_20atrisk %>%
    mutate(year = 2020)
) %>%
  select(
    year, share_time, everything()
  )

# 2. Filter to only consider a subst of share of time, at 0.05 intervals, 5, 10, 15, 20, 25, 30, 35, 40, 45. 9 groups.
df_all_sel <- df_all %>% 
  filter(share_time %in% ar_it_share_time)

# 3. Reshape utci_thresholds wide to long
df_temp_cdf_jnt_long <- df_all_sel %>%
  pivot_longer(
    cols = starts_with("share_gr"),
    names_to = c("utci_thres"),
    names_pattern = "share_gr(.*)",
    values_to = "cdf"
  ) %>%
  mutate(utci_thres = as.numeric(utci_thres)) %>%
  select(
    year, utci_thres, share_time, cdf
  )

# 4. 1 - all to see 1 - CDF
df_temp_cdf_jnt_long <- df_temp_cdf_jnt_long %>%
  mutate(cdf_comp = 1 - cdf)

# 5. Group by utci_thres, by share of days, arrange by filder counter
# - Percentage points change: Take diff with file counter as "t" (there are only two files, so difference across time)
# - Percentage change: Take ratio with file counter as "t" (there are only two files, so difference across time)
df_temp_cdf_jnt_long_stats <- df_temp_cdf_jnt_long %>%
  group_by(utci_thres, share_time) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    cdf_comp_diff = cdf_comp - lag(cdf_comp),
    cdf_comp_diff_ratio = cdf_comp_diff / lag(cdf_comp)
  )

# 6. Rehsape long to wide so temp-utci_thresholds are rows and share-of-time are columns, keep year and percent and percpoint as rows
# First, reshape stats to rows from columns, the four cdf columns
df_temp_cdf_jnt_longer <- df_temp_cdf_jnt_long_stats %>%
  pivot_longer(
    cols = starts_with("cdf"),
    names_to = c("stats"),
    names_pattern = paste0("(.*)"),
    values_to = "value"
  ) 
# Second, reshape share of days to column
df_temp_cdf_jnt_wideshrdays <- df_temp_cdf_jnt_longer %>%
  mutate(share_time = round(share_time*100, 0)) %>%
  pivot_wider(id_cols = c("year", "utci_thres", "stats"),
              names_from = share_time,
              names_prefix = "shrtime_",
              values_from = value)

# 7. Output for Table A
df_all_tab_a <- df_temp_cdf_jnt_wideshrdays %>%
    filter(stats == "cdf_comp") %>%
    select(
      stats, year, utci_thres, everything()
    ) %>% arrange(year, desc(utci_thres))
if (bl_main_save) {
    spn_path <- file.path(
        spt_path_res, "tab_a_level_data.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_all_tab_a, spn_path)
    print(glue::glue("F-813162, S1"))
    print(glue::glue("File saved: {spn_path}"))
}

# 8. Output for Table B
# change show on in 2020 rows
df_all_tab_b <- df_temp_cdf_jnt_wideshrdays %>%
    filter(
      stats %in% c("cdf_comp_diff", "cdf_comp_diff_ratio"),
      year == 2020
    ) %>%
    select(
      stats, -year, utci_thres, everything()
    ) %>% arrange(stats, desc(utci_thres))
if (bl_main_save) {
    spn_path <- file.path(
        spt_path_res, "tab_b_change_data.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_all_tab_b, spn_path)
    print(glue::glue("F-813162, S2"))
    print(glue::glue("File saved: {spn_path}"))
}

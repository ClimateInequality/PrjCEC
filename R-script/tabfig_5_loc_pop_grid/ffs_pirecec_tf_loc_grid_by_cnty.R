# Implement step 1 of https://github.com/ClimateInequality/PrjCEC/issues/37
# load library
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(kableExtra)


# Print
verbose <- TRUE
it_row_print <- 300
it_file_code <- 436536
bl_main_save <- TRUE
# Paths
spt_path_datares <- file.path("data-res", fsep = .Platform$file.sep)
spt_path_res <- file.path("res", "res_pop_grid_cnty", fsep = .Platform$file.sep)
snm_out_file_cty_grid_cnt <- "tab_aux_child_grid_by_cnty"
# File load and name
spn_path <- file.path(spt_path_datares, "aux_county_demo_share_grids_counts.csv", fsep = .Platform$file.sep)
df_data_tab_a <- read_csv(spn_path)

# 1. Compute total population in counties with 1, 2, 3, ..., N grid points -----------------------------
# population_agg_loc = sum of gender and age sub-groups from each GBCounty
# population_agg_loc does not sum up to 1 across all locations, sums to the total child population share in the country.
# population_agg_loc_n_coord = sum of gender and age sub-groups from all GBCounty with the same number of coordinates in them.
df_data_tab_a_agg <- df_data_tab_a %>%
  group_by(year, n_coord_GBCounty) %>%
  summarize(
    popagg_nco_n = n(),
    popagg_nco = sum(population_agg_loc, na.rm = TRUE)
  ) %>%
  mutate_at(vars(one_of(c("popagg_nco"))), list(~ replace_na(., 0)))

# 2. get min and max grid counts for each year ------------------------------
ar_n_coord_GBCounty_1990 <- df_data_tab_a_agg %>%
  filter(year == 1990) %>%
  pull(n_coord_GBCounty)
it_grid_count_max_1990 <- max(ar_n_coord_GBCounty_1990)
ar_n_coord_GBCounty_2020 <- df_data_tab_a_agg %>%
  filter(year == 2020) %>%
  pull(n_coord_GBCounty)
it_grid_count_max_2020 <- max(ar_n_coord_GBCounty_2020)
# get total county count
it_county_1990_n <- dim(df_data_tab_a %>% filter(year == 1990) %>% distinct(GBCounty))[1]
it_county_2020_n <- dim(df_data_tab_a %>% filter(year == 2020) %>% distinct(GBCounty))[1]

# 3. Add in missing rows so that there is a row for each possible `n_coord_GBcounty` level from 1 to max ----------------
ar_years_in_survey <- c(1990, 2020)
ar_grid_max <- c(it_grid_count_max_1990, it_grid_count_max_2020)
mt_combine <- cbind(ar_years_in_survey, ar_grid_max)
tb_grid_count_full <- as_tibble(mt_combine)
ar_st_varnames <- c("year", "n_coord_GBCounty")
tb_grid_count_full <- as_tibble(mt_combine) %>% rename_all(~ c(ar_st_varnames))
tb_grid_count_full <- tb_grid_count_full %>%
  uncount(n_coord_GBCounty) %>%
  group_by(year) %>%
  mutate(one = 1, n_coord_GBCounty = cumsum(one)) %>%
  select(-one) %>%
  ungroup()
# Merge
tb_grid_count_full <- tb_grid_count_full %>%
  left_join(
    df_data_tab_a_agg,
    by = c("year" = "year", "n_coord_GBCounty" = "n_coord_GBCounty")
  )
print(tb_grid_count_full, n = 700)
tb_grid_count_full <- tb_grid_count_full %>%
  mutate_at(
    vars(one_of(c("popagg_nco_n", "popagg_nco"))),
    list(~ replace_na(., 0))
  )

# 4. Compute population share cumulative -----------------------------------
df_data_tab_a_agg_cdf <- tb_grid_count_full %>%
  group_by(year) %>%
  mutate(
    # sc: share conditional on what is included here only (children)
    popagg_nco_sc = popagg_nco / sum(popagg_nco),
    popagg_nco_sc_cumu = cumsum(popagg_nco_sc)
  ) %>%
  ungroup(year)

# 5. Group grid counts, generate county count by grid count groups ------------------
ar_fl_cuts <- c(1, 2, 3, 4, 5, 6, 11, 21, 31, 41, 51, 101, 201, 331)
ar_st_cuts_lab <- vector(mode = "character", length = length(ar_fl_cuts) - 1)
ar_fl_cuts_m1 <- head(ar_fl_cuts, -1)
ar_fl_cuts_t1 <- tail(ar_fl_cuts, -1) - 1
for (it_cut_ctr in seq(1, length(ar_st_cuts_lab))) {
  fl_head <- ar_fl_cuts_m1[it_cut_ctr]
  fl_tail <- ar_fl_cuts_t1[it_cut_ctr]
  if (fl_head == fl_tail) {
    st_cuts_lab <- fl_head
  } else {
    st_cuts_lab <- paste0(fl_head, " to ", fl_tail)
  }
  ar_st_cuts_lab[it_cut_ctr] <- st_cuts_lab
}
# generate labels
# ar_st_cuts_lab <- ar_fl_cuts
df_data_tab_a_agg_cdf <- df_data_tab_a_agg_cdf %>%
  mutate(n_coord_GBCounty_grp = base::cut(n_coord_GBCounty,
    breaks = ar_fl_cuts,
    labels = ar_st_cuts_lab,
    # if right is FALSE, interval is closed on the left
    right = FALSE
  ))
# Aggregate up to `n_coord_GBCounty_grp`
df_data_tab_a_agggrp_cdf <- df_data_tab_a_agg_cdf %>%
  arrange(year, n_coord_GBCounty_grp) %>%
  group_by(year, n_coord_GBCounty_grp) %>%
  summarize(
    popagg_ncog_n = sum(popagg_nco_n),
    popagg_ncog_sc = sum(popagg_nco_sc),
  ) %>%
  mutate(
    popagg_ncog_sc_cumu = cumsum(popagg_ncog_sc)
  )
# rotate wider
df_data_tab_a_agg_cdf_wider <- df_data_tab_a_agg_cdf %>%
  pivot_wider(
    id_cols = c(
      "n_coord_GBCounty", "n_coord_GBCounty_grp",
    ),
    names_from = year,
    names_prefix = "year_",
    values_from = c(popagg_nco, popagg_nco_sc, popagg_nco_sc_cumu)
  ) %>%
  arrange(n_coord_GBCounty)
# rotate grouped aggregate wider
df_data_tab_a_agggrp_cdf_wider <- df_data_tab_a_agggrp_cdf %>%
  pivot_wider(
    id_cols = c(
      "n_coord_GBCounty_grp",
    ),
    names_from = year,
    names_prefix = "year_",
    values_from = c(
      popagg_ncog_n, popagg_ncog_sc, popagg_ncog_sc_cumu
    )
  ) %>%
  arrange(n_coord_GBCounty_grp)
if (verbose) {
  print(glue::glue("F-{it_file_code}, 5a"))
  print(df_data_tab_a_agg_cdf_wider, n = 500)
  print(df_data_tab_a_agggrp_cdf_wider, n = 500)
}
# # A tibble: 12 x 7
#    n_coord_GBCounty_grp popagg_ncog_n_year_1990 popagg_ncog_n_year_2020 popagg_ncog_sc_year_1990 popagg_ncog_sc_year_2020 popagg_ncog_sc_cumu_year_1990 popagg_ncog_sc_cumu_year_2020
#    <fct>                                  <int>                   <int>                    <dbl>                    <dbl>                         <dbl>                         <dbl>
#  1 1                                        503                     957                0.199                     0.345                            0.199                         0.345
#  2 2                                        467                     508                0.213                     0.202                            0.412                         0.547
#  3 3                                        378                     382                0.176                     0.155                            0.588                         0.702
#  4 4                                        307                     309                0.161                     0.129                            0.748                         0.831
#  5 5 to 9                                   430                     411                0.200                     0.130                            0.948                         0.961
#  6 10 to 19                                 149                     155                0.0347                    0.0239                           0.983                         0.985
#  7 20 to 29                                  49                      45                0.00714                   0.00679                          0.990                         0.992
#  8 30 to 39                                  28                      27                0.00345                   0.00228                          0.994                         0.994
#  9 40 to 49                                  19                      18                0.00272                   0.00228                          0.996                         0.996
# 10 50 to 99                                  24                      27                0.00254                   0.00266                          0.999                         0.999
# 11 100 to 199                                12                      11                0.000901                  0.000934                         1.00                          1.00
# 12 200 to 330                                 3                       3                0.0000825                 0.000125                         1                             1

# 6. Format table for output -----------------------------------------------
ar_st_col_adjust <- c(
  "popagg_ncog_sc_year_1990",
  "popagg_ncog_sc_year_2020",
  "popagg_ncog_sc_cumu_year_1990",
  "popagg_ncog_sc_cumu_year_2020"
)
df_data_formatted <- df_data_tab_a_agggrp_cdf_wider
for (svrpct in ar_st_col_adjust) {
  df_data_formatted <- df_data_formatted %>%
    mutate(
      !!sym(svrpct) := case_when(
        !!sym(svrpct) <= 1 ~ paste0(
          format(round(!!sym(svrpct) * 100, 2), nsmall = 2), "%"
        )
      )
    )
}
# r$> df_data_formatted
# # A tibble: 12 x 7
#    n_coord_GBCounty_grp popagg_ncog_n_year_1990 popagg_ncog_n_year_2020 popagg_ncog_sc_year_1990 popagg_ncog_sc_year_2020 popagg_ncog_sc_cumu_year_1990 popagg_ncog_sc_cumu_year_2020
#    <fct>                                  <int>                   <int> <chr>                    <chr>                    <chr>                         <chr>
#  1 1                                        503                     957 "19.9%"                  "34.5%"                  " 19.9%"                      " 34.5%"
#  2 2                                        467                     508 "21.3%"                  "20.2%"                  " 41.2%"                      " 54.7%"
#  3 3                                        378                     382 "17.6%"                  "15.5%"                  " 58.8%"                      " 70.2%"
#  4 4                                        307                     309 "16.1%"                  "12.9%"                  " 74.8%"                      " 83.1%"
#  5 5 to 9                                   430                     411 "20.0%"                  "13.0%"                  " 94.8%"                      " 96.1%"
#  6 10 to 19                                 149                     155 " 3.5%"                  " 2.4%"                  " 98.3%"                      " 98.5%"
#  7 20 to 29                                  49                      45 " 0.7%"                  " 0.7%"                  " 99.0%"                      " 99.2%"
#  8 30 to 39                                  28                      27 " 0.3%"                  " 0.2%"                  " 99.4%"                      " 99.4%"
#  9 40 to 49                                  19                      18 " 0.3%"                  " 0.2%"                  " 99.6%"                      " 99.6%"
# 10 50 to 99                                  24                      27 " 0.3%"                  " 0.3%"                  " 99.9%"                      " 99.9%"
# 11 100 to 199                                12                      11 " 0.1%"                  " 0.1%"                  "100.0%"                      "100.0%"
# 12 200 to 330                                 3                       3 " 0.0%"                  " 0.0%"                  "100.0%"                      "100.0%"
# Output to table
st_kableformat <- "latex"
# First, we define column names, which correspond to previously defined variable selection list.
ar_st_col_names <- c(
  "",
  "1990", "2020",
  "1990", "2020",
  "1990", "2020"
)
# Define column groups, grouping the names above
ar_st_col_groups <- c(
  "Number of grid points" = 1,
  "Number of counties" = 2,
  "Percent of children" = 2,
  "Cumulative \\% of children" = 2
)
# Define column groups, grouping the names above
# ar_st_col_groups_super <- c(
#   " " = 1,
#   "Loan terms" = 9
# )

# 7. Create table with kableExtra ----------------------------------------
df_data_formatted_final <- df_data_formatted %>% ungroup()
st_caption <- paste0(
  "The Distribution of the Number of Overlapping Grid Points (0.25 x 0.25 Degree) ",
  "and the Distribution of Children (ages 0--14) ",
  "among the ", it_county_1990_n, " Chinese Counties in 1990 and ", it_county_2020_n, " Chinese Counties in 2020"
)
bk_grid_dist_county <- kbl(
  df_data_formatted_final,
  format = st_kableformat,
  # escape = F,
  linesep = "",
  booktabs = T,
  longtable = T,
  align = "c",
  caption = st_caption,
  col.names = ar_st_col_names
)
# Third, we add in row groups
bk_grid_dist_county <- bk_grid_dist_county %>%
  add_header_above(ar_st_col_groups)
# add_header_above(ar_st_col_groups_super)
# Fourth, we add in column groups.
bk_grid_dist_county <- bk_grid_dist_county %>%
  pack_rows(
    "Panel A: Less than 6 grid points falling within a county",
    1, 5,
    latex_gap_space = "0.25em", hline_after = T, latex_align = "c"
  ) %>%
  pack_rows(
    "Panel B: Between 6 and 50 grid points falling within a county",
    6, 10,
    latex_gap_space = "0.25em", hline_after = T, latex_align = "c"
  ) %>%
  pack_rows(
    "Panel C: 51 or more grid points falling within a county",
    11, 13,
    latex_gap_space = "0.25em", hline_after = T, latex_align = "c"
  )
# Fifth, column formatting.
bk_grid_dist_county <- bk_grid_dist_county %>%
  column_spec(1:1, width = "3cm") %>%
  column_spec(2:7, width = "1.3cm")
# Final adjustments
st_texcmd <- "frac"
bk_grid_dist_county <- gsub(bk_grid_dist_county,
  pattern = paste0("\\textbackslash{}", st_texcmd, "\\"),
  replacement = paste0("\\", st_texcmd), fixed = TRUE
)
st_texcmd <- "text"
bk_grid_dist_county <- gsub(bk_grid_dist_county,
  pattern = paste0("\\textbackslash{}", st_texcmd, "\\"),
  replacement = paste0("\\", st_texcmd), fixed = TRUE
)
bk_grid_dist_county <- gsub(bk_grid_dist_county,
  pattern = paste0("\\textbackslash{}\\"),
  replacement = paste0("\\"), fixed = TRUE
)
bk_grid_dist_county <- gsub(bk_grid_dist_county,
  pattern = "\\}\\{",
  replacement = "}{", fixed = TRUE
)
bk_grid_dist_county <- gsub(bk_grid_dist_county,
  pattern = "\\}",
  replacement = "}", fixed = TRUE
)
bk_grid_dist_county <- gsub(bk_grid_dist_county,
  pattern = "\\$",
  replacement = "$", fixed = TRUE
)
# Sixth, display.
# pl_bk_asset_count <- bk_grid_dist_county %>% as_image()
if (st_kableformat == "latex") {
  if (bl_main_save) {
    spn_file <- file.path(
      spt_path_res, paste0(snm_out_file_cty_grid_cnt, ".tex"),
      fsep = .Platform$file.sep
    )
    fileConn <- file(spn_file)
    writeLines(bk_grid_dist_county, fileConn)
    close(fileConn)
    if (verbose) {
      print(glue::glue("F-815346, S3"))
      print(glue::glue("Latex saved: {spn_file}"))
    }
  }
}

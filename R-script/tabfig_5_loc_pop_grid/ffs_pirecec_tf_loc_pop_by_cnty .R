# Implement step 3 of https://github.com/ClimateInequality/PrjCEC/issues/47
# load library
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(kableExtra)

# Print
verbose <- TRUE
it_row_print <- 300
it_file_code <- 321782
bl_main_save <- TRUE
# Paths
spt_path_datares <- file.path("data-res", fsep = .Platform$file.sep)
spt_path_res <- file.path("res", "res_pop_grid_cnty", fsep = .Platform$file.sep)
snm_out_file_cty_child_pop <- "tab_aux_child_pop_by_cnty"
# File load and name
spn_path <- file.path(spt_path_datares, "aux_county_demo_share_grids_counts.csv", fsep = .Platform$file.sep)
df_data_tab <- read_csv(spn_path)

# 1. Error checking and population count 100 percent and also population levels ------------------------
# Rows with invalid population information 
if (verbose) {
  print(glue::glue("F-{it_file_code}, 1a"))
  print(df_data_tab %>% filter(is.na(population_agg_loc)))
}
# # A tibble: 9 x 8
# # Groups:   year [1]
#   GBCounty n_coord_GBCounty population_agg_loc GBProv Prov_En  region_name  year pop_share_loc
#      <dbl>            <dbl>              <dbl>  <dbl> <chr>    <chr>       <dbl>         <dbl>
# 1   330113                1                 NA 330000 Zhejiang Eastern      2020            NA
# 2   330114                1                 NA 330000 Zhejiang Eastern      2020            NA
# 3   350527                1                 NA 350000 Fujian   Eastern      2020            NA   
# 4   460301                1                 NA 460000 Hainan   Eastern      2020            NA   
# 5   460302                3                 NA 460000 Hainan   Eastern      2020            NA   
# 6   659011                6                 NA 650000 Xinjiang Western      2020            NA   
# 7   710000               54                 NA     NA NA       NA           2020            NA   
# 8   810013                3                 NA     NA NA       NA           2020            NA   
# 9   820004                1                 NA     NA NA       NA           2020            NA   
# Adjust population shares to be out of 100 percent, within each year
# na.rm = TRUE due to the 9 locations in 2020 without pop info
df_data_tab <- df_data_tab %>% 
    group_by(year) %>%
    mutate(
        pop_share_loc = population_agg_loc/sum(population_agg_loc, na.rm = TRUE)
        )
# convert population shares to also population counts
# Using the case of China---home to approximately 249.9 million children ages 0--14 in 2020, from from HKW
# 352,612 million in 1980 according to HKW. 
# 324.5 million in 1990 according to HKW table A.3
df_data_tab <- df_data_tab %>% 
    mutate(
        pop_level_loc = case_when(
          year == 1990 ~ pop_share_loc*324500,
          year == 2020 ~ pop_share_loc*249900
        )
      )

# 2. Reshape data to long so percent and level calculations can be done concurrently ------------------------
# Convert from wide to long, so that pop_share_loc and pop_level_loc are in the same column
# this is done to make the percentile calculations easier
df_data_tab <- df_data_tab %>%
  pivot_longer(cols = starts_with('pop_'),
               names_to = c('pop_stats'),
               names_pattern = paste0("pop_(.*)"),
               values_to = "value")

# 3. Duplicate national file, so national and regional calculations can be done together ------------------------
df_data_tab_national <- df_data_tab %>% mutate(region_name = "National")
df_data_tab_natreg <- bind_rows(df_data_tab_national, df_data_tab)

# 4. Compute quantiles/CDF by year and region/overall and group-mean ---------------------------
# cumsum(value)/sum(value) is needed so that regional CDF sums to 1, but we don't want to change value, so that we can get child population count by multiplying that with the total child count in each year. 
df_data_tab_natreg <- df_data_tab_natreg %>% 
    select(pop_stats, year, region_name, value) %>%
    arrange(pop_stats, year, region_name, value) %>%
    group_by(pop_stats, year, region_name) %>%
    mutate(cdf = cumsum(value)/sum(value, na.rm = TRUE))
# percentilf file name
tstm_loans_sel_long_pct <- df_data_tab_natreg
# Define percentile inputs
ar_fl_percentiles <- c(0.01, 0.1, 0.25, 0.50, 0.75, 0.9, 0.99)
st_var_prefix <- "val"
st_var_prefix_perc <- paste0(st_var_prefix, "_p")
svr_mean <- paste0(st_var_prefix, "_mean")
svr_std <- paste0(st_var_prefix, "_std")
# Generate within-group percentiles
for (it_percentile_ctr in seq(1, length(ar_fl_percentiles))) {
  # Current within group percentile to compute
  fl_percentile <- ar_fl_percentiles[it_percentile_ctr]
  # Percentile and mean stats
  svr_percentile <- paste0(st_var_prefix_perc, round(fl_percentile * 100))
  # Frame with specific percentile
  df_within_percentiles_cur <- tstm_loans_sel_long_pct %>%
    group_by(pop_stats, year, region_name) %>%
    filter(cdf >= fl_percentile) %>%
    slice(1) %>%
    mutate(!!sym(svr_percentile) := value) %>%
    select(pop_stats, year, region_name, one_of(svr_percentile))
  # Merge percentile frames together
  if (it_percentile_ctr > 1) {
    df_within_percentiles <- df_within_percentiles %>%
      left_join(df_within_percentiles_cur,
        by = c(
          "pop_stats" = "pop_stats",
          "region_name" = "region_name", 
          "year" = "year"
          )
      )
  } else {
    df_within_percentiles <- df_within_percentiles_cur
  }
}
# Add in within group mean
df_within_percentiles_mean <- tstm_loans_sel_long_pct %>%
  group_by(year, region_name) %>%
  # mutate(!!sym(svr_mean) := mean(value, na.rm = TRUE)) %>%
  # mutate(!!sym(svr_std) := sqrt(mean((value - !!sym(svr_mean))^2, na.rm = TRUE))) %>%
  slice(1)
# Join mean file to percentile file
df_within_percentiles <- df_within_percentiles %>%
  left_join(df_within_percentiles_mean,
    by = c(
      "pop_stats" = "pop_stats",
      "region_name" = "region_name", 
      "year" = "year"
      )
  ) %>%
  select(
    year, region_name,
    contains(st_var_prefix_perc)
    # one_of(svr_mean) ,
    # one_of(svr_std)
  )
# display
if (verbose) {
  print(glue::glue("F-{it_file_code}, 4a: df_within_percentiles"))
  print(df_within_percentiles)
}
# # A tibble: 4 x 9
# # Groups:   year, region_name [4]
#    year region_name  val_p10  val_p25   val_p50   val_p75  val_p90 val_mean  val_std
#   <dbl> <chr>          <dbl>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>    <dbl>
# 1  1990 Central     0.000508 0.00112  NA        NA        NA       0.000505 0.000327
# 2  1990 Eastern     0.000457 0.000997 NA        NA        NA       0.000531 0.000410
# 3  1990 Western     0.000330 0.000947 NA        NA        NA       0.000302 0.000272
# 4  1990 national    0.000244 0.000360  0.000572  0.000897  0.00125 0.000422 0.000346
# # Groups:   year, region_name [10]
#     year region_name     val_p1   val_p10  val_p25  val_p50  val_p75  val_p90  val_p99
#    <dbl> <chr>            <dbl>     <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#  1  1990 Central      0.000122  0.000277  0.000434 0.000665 0.000963 0.00125  0.00166
#  2  1990 Eastern      0.000161  0.000291  0.000426 0.000635 0.000989 0.00150  0.00448
#  3  1990 National     0.0000848 0.000244  0.000360 0.000572 0.000897 0.00125  0.00382
#  4  1990 Northeastern 0.0000777 0.000258  0.000360 0.000510 0.000770 0.00102  0.00303
#  5  1990 Western      0.0000449 0.000169  0.000278 0.000423 0.000735 0.00108  0.00174 
#  6  2020 Central      0.0000811 0.000219  0.000347 0.000571 0.000837 0.00108  0.00150
#  7  2020 Eastern      0.000133  0.000271  0.000392 0.000604 0.000957 0.00152  0.00551
#  8  2020 National     0.0000619 0.000192  0.000310 0.000513 0.000807 0.00118  0.00276
#  9  2020 Northeastern 0.0000239 0.0000862 0.000134 0.000206 0.000306 0.000468 0.000932
# 10  2020 Western      0.0000426 0.000148  0.000250 0.000398 0.000660 0.000942 0.00162
# Using the case of China---home to approximately 249.9 million children ages 0--14 in 2020, from from HKW
# 352,612 million in 1980 according to HKW. 
# 324.5 million in 1990 according to HKW table A.3
# Adding missing grouping variables: `pop_stats`
# # A tibble: 20 x 10
# # Groups:   pop_stats, year, region_name [20]
#    pop_stats  year region_name      val_p1    val_p10    val_p25    val_p50    val_p75    val_p90     val_p99
#    <chr>     <dbl> <chr>             <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>       <dbl>
#  1 level_loc  1990 Central      39.6       89.9       141.       216.       312.       405.        540.
#  2 level_loc  1990 Eastern      52.2       94.3       138.       206.       321.       487.       1453.
#  3 level_loc  1990 National     27.5       79.3       117.       186.       291.       406.       1240.
#  4 level_loc  1990 Northeastern 25.2       83.9       117.       165.       250.       332.        984.
#  5 level_loc  1990 Western      14.6       54.9        90.1      137.       238.       350.        564.
#  6 level_loc  2020 Central      20.3       54.7        86.7      143.       209.       271.        374.
#  7 level_loc  2020 Eastern      33.3       67.8        97.9      151.       239.       379.       1376.
#  8 level_loc  2020 National     15.5       47.9        77.4      128.       202.       296.        690.
#  9 level_loc  2020 Northeastern  5.97      21.5        33.4       51.5       76.4      117.        233.
# 10 level_loc  2020 Western      10.6       37.1        62.4       99.4      165.       235.        405.
# 11 share_loc  1990 Central       0.000122   0.000277    0.000434   0.000665   0.000963   0.00125     0.00166
# 12 share_loc  1990 Eastern       0.000161   0.000291    0.000426   0.000635   0.000989   0.00150     0.00448
# 13 share_loc  1990 National      0.0000848  0.000244    0.000360   0.000572   0.000897   0.00125     0.00382
# 14 share_loc  1990 Northeastern  0.0000777  0.000258    0.000360   0.000510   0.000770   0.00102     0.00303
# 15 share_loc  1990 Western       0.0000449  0.000169    0.000278   0.000423   0.000735   0.00108     0.00174
# 16 share_loc  2020 Central       0.0000811  0.000219    0.000347   0.000571   0.000837   0.00108     0.00150
# 17 share_loc  2020 Eastern       0.000133   0.000271    0.000392   0.000604   0.000957   0.00152     0.00551
# 18 share_loc  2020 National      0.0000619  0.000192    0.000310   0.000513   0.000807   0.00118     0.00276
# 19 share_loc  2020 Northeastern  0.0000239  0.0000862   0.000134   0.000206   0.000306   0.000468    0.000932
# 20 share_loc  2020 Western       0.0000426  0.000148    0.000250   0.000398   0.000660   0.000942    0.00162

# 5. tab-gen: group arrangements sorting, labels, formatings -----------------------------------
df_within_percentiles <- df_within_percentiles %>%
  mutate(
    region_name_sorter = case_when(
      region_name == "National" ~ 0,
      region_name == "Central" ~ 1,
      region_name == "Eastern" ~ 2,
      region_name == "Northeastern" ~ 3,
      region_name == "Western" ~ 4
    )
  ) %>% 
  arrange(
   region_name_sorter, year, pop_stats
  )
# Variable labeling
df_within_percentiles <- df_within_percentiles %>%
  mutate(
    pop_stats = case_when(
      pop_stats == "level_loc" ~ "Number",
      pop_stats == "share_loc" ~ "Percent",
    )
  ) 
# formatting
ar_st_col_adjust <- c(
  "val_p1", 
  "val_p10", 
  "val_p25", 
  "val_p50", 
  "val_p75", 
  "val_p90", 
  "val_p99"
  )
# Format
df_data_formatted <- df_within_percentiles
for (svrpct in ar_st_col_adjust) {
  df_data_formatted <- df_data_formatted %>%
    mutate(
      !!sym(svrpct) := case_when(
        !!sym(svrpct) < 1 ~ paste0(
          format(round(!!sym(svrpct) * 100, 3), nsmall = 1), "%"
        ),
        !!sym(svrpct) > 1 ~ paste0(
          format(round(!!sym(svrpct), 1), nsmall = 1), "K"
        )
      )
    )
}
# r$> df_data_formatted
# # A tibble: 20 x 11
# # Groups:   pop_stats, year, region_name [20]
#    pop_stats  year region_name  val_p1 val_p10 val_p25 val_p50 val_p75 val_p90 val_p99 region_name_sorter
#    <chr>     <dbl> <chr>        <chr>  <chr>   <chr>   <chr>   <chr>   <chr>   <chr>                <dbl>
#  1 Number     1990 National     27.5K  79.3K   117.0K  185.7K  291.0K  405.5K  1240.1K                  0
#  2 Percent    1990 National     0.008% 0.024%  0.036%  0.057%  0.09%   0.125%  0.382%                   0
#  3 Number     2020 National     15.5K  47.9K   77.4K   128.3K  201.7K  296.0K  689.6K                   0
#  4 Percent    2020 National     0.006% 0.019%  0.031%  0.051%  0.081%  0.118%  0.276%                   0
#  5 Number     1990 Central      39.6K  89.9K   140.9K  215.7K  312.4K  404.9K  540.0K                   1
#  6 Percent    1990 Central      0.012% 0.028%  0.043%  0.066%  0.096%  0.125%  0.166%                   1
#  7 Number     2020 Central      20.3K  54.7K   86.7K   142.7K  209.3K  270.6K  374.3K                   1
#  8 Percent    2020 Central      0.008% 0.022%  0.035%  0.057%  0.084%  0.108%  0.15%                    1
#  9 Number     1990 Eastern      52.2K  94.3K   138.1K  206.0K  320.9K  486.7K  1453.2K                  2
# 10 Percent    1990 Eastern      0.016% 0.029%  0.043%  0.063%  0.099%  0.15%   0.448%                   2
# 11 Number     2020 Eastern      33.3K  67.8K   97.9K   150.8K  239.1K  378.8K  1376.4K                  2
# 12 Percent    2020 Eastern      0.013% 0.027%  0.039%  0.06%   0.096%  0.152%  0.551%                   2
# 13 Number     1990 Northeastern 25.2K  83.9K   116.8K  165.4K  249.8K  332.4K  984.1K                   3
# 14 Percent    1990 Northeastern 0.008% 0.026%  0.036%  0.051%  0.077%  0.102%  0.303%                   3
# 15 Number     2020 Northeastern 6.0K   21.5K   33.4K   51.5K   76.4K   116.9K  233.0K                   3
# 16 Percent    2020 Northeastern 0.002% 0.009%  0.013%  0.021%  0.031%  0.047%  0.093%                   3
# 17 Number     1990 Western      14.6K  54.9K   90.1K   137.3K  238.4K  350.5K  563.6K                   4
# 18 Percent    1990 Western      0.004% 0.017%  0.028%  0.042%  0.073%  0.108%  0.174%                   4
# 19 Number     2020 Western      10.6K  37.1K   62.4K   99.4K   164.9K  235.4K  405.0K                   4
# 20 Percent    2020 Western      0.004% 0.015%  0.025%  0.04%   0.066%  0.094%  0.162%                   4

# 6. tab-gen: column layout and table output -----------------------------------
# Output to table
st_kableformat <- "latex"
# First, we define column names, which correspond to previously defined variable selection list.
ar_st_col_names <- c(
  "Statistics",
  "Year", 
  "1", "10", "25",
  "50", 
  "75", "90", "99"
)
# Define column groups, grouping the names above
ar_st_col_groups <- c(
  " " = 2,
  "Percentiles" = 7
)
# Define column groups, grouping the names above
ar_st_col_groups_super <- c(
  " " = 1,
  "Loan terms" = 9
)
# Second, we construct main table, and add styling.
df_data_formatted_final <- df_data_formatted %>% 
  ungroup() %>%
  select(-region_name, -region_name_sorter)
bk_child_dist_county <- kbl(
  df_data_formatted_final,
  format = st_kableformat,
  # escape = F,
  linesep = "",
  booktabs = T,
  longtable = T,
  align = "c",
  caption = "The Distribution of Chinese Children (ages 0-14) Across Counties in 1990 and 2020",
  col.names = ar_st_col_names
)
# Third, we add in row groups
bk_child_dist_county <- bk_child_dist_county %>%
  add_header_above(ar_st_col_groups) 
  # add_header_above(ar_st_col_groups_super)
# Fourth, we add in column groups. 
bk_child_dist_county <- bk_child_dist_county %>%
  pack_rows(
    "Panel A: National",
    1, 4,
    latex_gap_space = "0.25em", hline_after = T, latex_align = "c"
  ) %>%
  pack_rows(
    "Panel B: Central region",
    5, 8,
    latex_gap_space = "0.25em", hline_after = T, latex_align = "c"
  ) %>%
  pack_rows(
    "Panel C: Eastern region",
    9, 12,
    latex_gap_space = "0.25em", hline_after = T, latex_align = "c"
  ) %>%
  pack_rows(
    "Panel D: Northeastern region",
    13, 16,
    latex_gap_space = "0.25em", hline_after = T, latex_align = "c"
  ) %>%
  pack_rows(
    "Panel E: Western region",
    17, 20,
    latex_gap_space = "0.25em",  hline_after = T, latex_align = "c"
  )
# Fifth, column formatting.
bk_child_dist_county <- bk_child_dist_county %>%
  column_spec(1:2, width = "1.4cm") %>%
  column_spec(3:9, width = "1.3cm")
# Final adjustments
st_texcmd <- "frac"
bk_child_dist_county <- gsub(bk_child_dist_county,
  pattern = paste0("\\textbackslash{}", st_texcmd, "\\"),
  replacement = paste0("\\", st_texcmd), fixed = TRUE
)
st_texcmd <- "text"
bk_child_dist_county <- gsub(bk_child_dist_county,
  pattern = paste0("\\textbackslash{}", st_texcmd, "\\"),
  replacement = paste0("\\", st_texcmd), fixed = TRUE
)
bk_child_dist_county <- gsub(bk_child_dist_county,
  pattern = "\\}\\{",
  replacement = "}{", fixed = TRUE
)
bk_child_dist_county <- gsub(bk_child_dist_county,
  pattern = "\\}",
  replacement = "}", fixed = TRUE
)
bk_child_dist_county <- gsub(bk_child_dist_county,
  pattern = "\\$",
  replacement = "$", fixed = TRUE
)
# Sixth, display.
# pl_bk_asset_count <- bk_child_dist_county %>% as_image()
if (st_kableformat == "latex") {
  if (bl_main_save) {
    spn_file <- file.path(
      spt_path_res, paste0(snm_out_file_cty_child_pop, ".tex"),
      fsep = .Platform$file.sep
    )
    fileConn <- file(spn_file)
    writeLines(bk_child_dist_county, fileConn)
    close(fileConn)
    if (verbose) {
      print(glue::glue("F-{it_file_code}, 6a"))
      print(glue::glue("Latex saved: {spn_file}"))
    }
  }
}
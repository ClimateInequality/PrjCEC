ar_it_results_set <- c(
    11, 12, 13,
    21,
    31, 32,
    41, 42, 
    51
)
ar_it_results_set <- c(13)
ar_it_results_set <- c(41, 42)

if (11 %in% ar_it_results_set) {
    # all 24 hours
    source("R-script/run_1a_mean_child_all24/ffs_pirecec_demo_1989t1991_all24.R")
    source("R-script/run_1a_mean_child_all24/ffs_pirecec_demo_2019t2021_all24.R")
}
if (12 %in% ar_it_results_set) {
    # day time only
    source("R-script/run_1b_mean_child_6t22/ffs_pirecec_daytimeonly_bookends_convert.R")
    source("R-script/run_1b_mean_child_6t22/ffs_pirecec_demo_1989t1991_6t22.R")
    source("R-script/run_1b_mean_child_6t22/ffs_pirecec_demo_2019t2021_6t22.R")
}
if (13 %in% ar_it_results_set) {
    # source("R-script/run_1c_mean_child_seasons/ffs_pirecec_nonwinter_bookends_convert.R")
    # April to September
    source("R-script/run_1c_mean_child_seasons/ffs_pirecec_demo_1989t1991_apr2sep.R")
    source("R-script/run_1c_mean_child_seasons/ffs_pirecec_demo_2019t2021_apr2sep.R")
    # October to March
    source("R-script/run_1c_mean_child_seasons/ffs_pirecec_demo_1989t1991_oct2mar.R")
    source("R-script/run_1c_mean_child_seasons/ffs_pirecec_demo_2019t2021_oct2mar.R")
}
if (21 %in% ar_it_results_set) {
    # !! select to run only 3 and 4
    source("R-script/run_2a_atrisk_child/ffs_pirecec_demo_jntdist_atrisk_par.R")
    # source("R-script/run_2a_atrisk_child/ffs_pirecec_demo_jntdist_atrisk.R")
}
if (31 %in% ar_it_results_set) {
    source("R-script/run_3a_decompose/ffs_pirecec_demo_1990pop_2020utci_par.R")
    source("R-script/run_3a_decompose/ffs_pirecec_demo_2020pop_1990utci_par.R")
}
if (32 %in% ar_it_results_set) {
    source("R-script/run_3b_decompose_regional/ffs_pirecec_demo_loc_region_1990pop_2020utci_par.R")
    source("R-script/run_3b_decompose_regional/ffs_pirecec_demo_loc_region_2020pop_1990utci_par.R")
}
if (41 %in% ar_it_results_set) {
    # regional results, 24 hours
    source("R-script/run_4a_mean_child_all24_by_region/ffs_pirecec_demo_loc_region_1989t1991_par.R")
    source("R-script/run_4a_mean_child_all24_by_region/ffs_pirecec_demo_loc_region_2019t2021_par.R")
}
if (42 %in% ar_it_results_set) {
    # provincial results, 24 hours
    source("R-script/run_4b_mean_child_all24_by_province/ffs_pirecec_demo_loc_prov_1989t1991_par.R")
    source("R-script/run_4b_mean_child_all24_by_province/ffs_pirecec_demo_loc_prov_2019t2021_par.R")
}
# Result 51 from `R-script/run_all.R` is not based on temperature measurements.
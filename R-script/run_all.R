ar_it_results_set <- c(11, 12, 13, 2)
ar_it_results_set <- c(13)

if (11 %in% ar_it_results_set) {
    # all 24 hours
    source("R-script/run_1a_mean_child_all24/ffs_pirecec_demo_1990_all24.R")
    source("R-script/run_1a_mean_child_all24/ffs_pirecec_demo_2020_all24.R")
}
if (12 %in% ar_it_results_set) {
    # day time only
    source("R-script/run_1b_mean_child_6t22/ffs_pirecec_demo_1990_6t22.R")
    source("R-script/run_1b_mean_child_6t22/ffs_pirecec_demo_2020_6t22.R")
}
if (13 %in% ar_it_results_set) {
    # April to September
    source("R-script/run_1c_mean_child_seasons/ffs_pirecec_demo_1990_apr2sep.R")
    source("R-script/run_1c_mean_child_seasons/ffs_pirecec_demo_2020_apr2sep.R")
    # October to March
    source("R-script/run_1c_mean_child_seasons/ffs_pirecec_demo_1990_oct2mar.R")
    source("R-script/run_1c_mean_child_seasons/ffs_pirecec_demo_2020_oct2mar.R")
}
if (2 %in% ar_it_results_set) {
    # regional results, 24 hours
    source("R-script/run_4a_mean_child_all24_by_region/ffs_pirecec_demo_loc_region_1990_par.R")
    source("R-script/run_4a_mean_child_all24_by_region/ffs_pirecec_demo_loc_region_2020_par.R")
    # provincial results, 24 hours
    source("R-script/run_4b_mean_child_all24_by_province/ffs_pirecec_demo_loc_prov_1990_par.R")
    source("R-script/run_4b_mean_child_all24_by_province/ffs_pirecec_demo_loc_prov_2020_par.R")
}

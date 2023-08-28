#' Generate file prefix
#'
#' @description Common prefix generator for file names.
#'
#' @param snm_new_file_name_prefix string file name prefix for csv and img
#' @param st_time_stats string for type of within year stats to compute, "mean" or "share"
#' @param ar_temp_bound array for temperature bound if `st_time_stats` is share of days
#' @param bl_greater boolean if to compute larger or smaller than `fl_temp_bound`
#' @param verbose boolean print progress and key results
#' @author fan wang, \url{http://fanwangecon.github.io}
#'
#' @return an array of tax-liabilities for particular kids count and martial
#'   status along an array of income levels
#' @references
#' \url{https://fanwangecon.github.io/prjenvdemo/articles/fv_rda_simu_loc_demo.html}
#' @export
#' @import readr dplyr tidyr ggplot2
#'
ffp_demo_file_prefix <- function(
    snm_new_file_name_prefix = "ineq",
    st_time_stats = "share", fl_temp_bound = 20, bl_greater = TRUE,
    verbose = FALSE) {

    if (tolower(st_time_stats) == tolower("mean")) {
        snm_new_file_name_prefix <- paste0("ineq_", st_time_stats)
        st_rela <- ""
    } else if (tolower(st_time_stats) == tolower("share")) {
        if (bl_greater) {
            st_rela <- paste0(st_time_stats, "_gr")
            snm_new_file_name_prefix <- paste0("ineq_", st_rela, fl_temp_bound)
        } else {
            st_rela <- paste0(st_time_stats, "_ls")
            snm_new_file_name_prefix <- paste0("ineq_",  st_rela, fl_temp_bound)
        }
    }

    ls_snm_new_file_name_prefix <- list(
        snm_new_file_name_prefix = snm_new_file_name_prefix,
        st_rela = st_rela
    )

    return(ls_snm_new_file_name_prefix) 
}
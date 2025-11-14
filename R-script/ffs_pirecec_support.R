#' Local development script parameter control.
#'
#' We have a number of scripts, we want to control some core parameters
#' in those scripts. The scripts are calling core functions, we just
#' want some parameters to control the flow of scripts across multiple scripts.
#'
#' - Sets the number of cores to use for parallel processing
#' - Sets whether to generate individual threshold files or aggregate
#'
#' @description Local development path support
#'
#' @return A list of parameters to control scripts
#' @examples
#' ls_script_controls <- ffs_run_set_params()
#' it_nnodes <- ls_script_controls$it_nnodes
#' ls_run <- ls_script_controls$ls_run
#' @export
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#'
#' 

# NOTE THAT CURRENT CONFIGURATION IS SET TO MARCO'S DROPBOX FOR REPLICATION/EXTENSION

ffs_run_set_params <- function() {
    # The number of nodes to be forked.
    # https://www.rdocumentation.org/packages/parallel/versions/3.6.2/topics/makeCluster
    # 10 on precision
    it_nnodes <- 4

    # 1 = genereate individual threshold files, 2 aggregate threshold individual files jointsly
    #ls_run <- c(1, 2)
     ls_run <- c(1,2)
     #ls_run <- c(2)

    # Temperature array to evaluate
    # ar_temp_bound <- seq(1, 2, by = 1)
    ar_temp_bound <- seq(-40, 40, by = 1)

    return(list(
        it_nnodes = it_nnodes,
        ls_run = ls_run,
        ar_temp_bound = ar_temp_bound
    ))
}

#' Local developemnt path support
#'
#' @description Local development path support
#'
#' @return A list of local paths
#' @export
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#'
ffs_pirecec_path_run <- function() {
    # 1. Get dropbox path
    spt_root_prj_main_dropbox_xps15 <- file.path(
        "/Users", "mlaghi", "Dropbox", "PIRE", "team", "marco_laghi",
        "PrjCECReplicate",
        fsep = .Platform$file.sep
    ) 
    spt_root_prj_main_dropbox_vostro <- file.path(
        "/Users", "mlaghi", "Dropbox", "PIRE", "team", "marco_laghi",
        "PrjCECReplicate",
        fsep = .Platform$file.sep
    )
    spt_root_prj_main_dropbox  <- file.path(
        "C:", "Users", "fan", "UH-ECON Dropbox", "Fan Wang",
        "PIRE", "team", "marco_laghi",
        "PrjCECReplicate",
        fsep = .Platform$file.sep
    ) 

    # Checking if file exists
    if (file.exists(spt_root_prj_main_dropbox_xps15)) {
        spt_root_prj_main_dropbox <- spt_root_prj_main_dropbox_xps15
        st_print <- paste(spt_root_prj_main_dropbox_xps15, "exists")
    } else if (file.exists(spt_root_prj_main_dropbox_vostro)) {
        spt_root_prj_main_dropbox <- spt_root_prj_main_dropbox_vostro
        st_print <- paste(spt_root_prj_main_dropbox_vostro, "exists", spt_root_prj_main_dropbox_xps15, "does not exist")
    } else {
        print(glue::glue("F-955325, S1c"))
        st_print <- paste(spt_root_prj_main_dropbox_xps15, spt_root_prj_main_dropbox_vostro, "both do does not exist")
    }
    print(glue::glue("F-955325, S1a"))
    print(glue::glue("path: {st_print}"))

    # 2. get r functions and source (outside of package usage)
    spt_cec_sandbox_r_func_fan <- file.path(
        spt_root_prj_main_dropbox,
        # "sandbox", "r", "function",
        fsep = .Platform$file.sep
    )
    spt_cec_sandbox_r_func <- spt_cec_sandbox_r_func_fan
    spt_cec_sandbox_r_func_fan <- file.path(
        spt_root_prj_main_dropbox,
        # "sandbox", "r", "function",
        fsep = .Platform$file.sep
    )

    spt_path_func <- file.path(spt_cec_sandbox_r_func, "ffp_cec_aux.R", fsep = .Platform$file.sep)
    source(spt_path_func)
    spt_path_func <- file.path(spt_cec_sandbox_r_func, "ffp_cec_inequality_func.R", fsep = .Platform$file.sep)
    source(spt_path_func)
    spt_path_func <- file.path(spt_cec_sandbox_r_func, "ffp_cec_ineq_full_dist_func.R", fsep = .Platform$file.sep)
    source(spt_path_func)
    spt_path_func <- file.path(spt_cec_sandbox_r_func, "ffp_cec_thres_combine.R", fsep = .Platform$file.sep)
    source(spt_path_func)

    # 3. kf folders
    spt_pire_team_kf <- file.path(
        spt_root_prj_main_dropbox,
        fsep = .Platform$file.sep
    )

    # # Run function in code folder
    # spt_root <- "C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox"
    # # 3.A Activate program
    # spt_gpp <- "/PIRE/team/marco_laghi/PrjCEC-main/sandbox/R/function"

    # spn_dataraw_nonpub <- spt_root_prj_main_dropbox
    # spn_data

    return(list(
        spt_root_prj_main_dropbox = spt_root_prj_main_dropbox,
        spt_cec_sandbox_r_func = spt_cec_sandbox_r_func,
        spt_pire_team_kf = spt_pire_team_kf
    ))
}


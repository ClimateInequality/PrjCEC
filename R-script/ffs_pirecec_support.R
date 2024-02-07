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
        "C:", "Users", "fan",
        "Dropbox (UH-ECON)",
        fsep = .Platform$file.sep
    )
    spt_root_prj_main_dropbox_vostro <- file.path(
        "C:", "Users", "fan",
        "Documents",
        "Dropbox (UH-ECON)", 
        fsep = .Platform$file.sep
    )

    # Checking if file exists
    if (file.exists(spt_root_prj_main_dropbox_xps15)) {
        spt_root_prj_main_dropbox <- spt_root_prj_main_dropbox_xps15
        print(paste(spt_root_prj_main_dropbox_xps15, "exists"))
    } else if (file.exists(spt_root_prj_main_dropbox_vostro)) {
        spt_root_prj_main_dropbox <- spt_root_prj_main_dropbox_vostro
        print(paste(spt_root_prj_main_dropbox_vostro, "exists", spt_root_prj_main_dropbox_xps15, "does not exist"))
    } else {
        print(paste(spt_root_prj_main_dropbox_xps15, spt_root_prj_main_dropbox_vostro, "both do does not exist"))
    }

    # 2. get r functions and source (outside of package usage)
    spt_cec_sandbox_r_func_fan <- file.path(
        spt_root_prj_main_dropbox, 
        "repos", "prjcec", 
        "r",
        # "sandbox", "r", "function",
        fsep = .Platform$file.sep
    )
    spt_cec_sandbox_r_func <- spt_cec_sandbox_r_func_fan
    spt_cec_sandbox_r_func_fan <- file.path(
        spt_root_prj_main_dropbox, 
        "repos", "prjcec", 
        "r",
        # "sandbox", "r", "function",
        fsep = .Platform$file.sep
    )

    spt_path_func <- file.path(spt_cec_sandbox_r_func, "ffp_cec_aux.R", fsep = .Platform$file.sep)
    source(spt_path_func)
    spt_path_func <- file.path(spt_cec_sandbox_r_func, "ffp_cec_inequality_func.R", fsep = .Platform$file.sep)
    source(spt_path_func)
    spt_path_func <- file.path(spt_cec_sandbox_r_func, "ffp_cec_thres_combine.R", fsep = .Platform$file.sep)
    source(spt_path_func)

    # 3. kf folders
    spt_pire_team_kf <- file.path(
        spt_root_prj_main_dropbox, "PIRE", "team", "kai_feng",
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

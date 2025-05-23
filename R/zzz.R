# Decalring global paths to silence the Rmd warnings
# Initialize globals

.microbs_env <- new.env(parent = emptyenv())
# .microbs_env$wd <- NULL
# .microbs_env$path_connector <- NULL
# .microbs_env$ddPCR_raw_path <- NULL
# .microbs_env$qPCR_raw_path <- NULL 
# .microbs_env$stdCurve_path <- NULL
# .microbs_env$flux_path <- NULL
# .microbs_env$check_data_path <- NULL

# The .onLoad function will load these global variables into the R env.
# Add any variables here that might be needed to be accessed when running the code

.onLoad <- function(libname, pkgname) {

    # Create the empty paths to store information
    .microbs_env <<- new.env(parent = emptyenv())
    .microbs_env$wd <- NULL
    .microbs_env$path_connector <- NULL
    .microbs_env$ddPCR_raw_path <- NULL
    .microbs_env$qPCR_raw_path <- NULL 
    .microbs_env$stdCurve_path <- NULL
    .microbs_env$flux_path <- NULL
    .microbs_env$checkData_path <- NULL
    .microbs_env$loaded_data_path <- NULL
    .microbs_env$calc_data_path <- NULL
    .microbs_env$created_data_path <- NULL
    .microbs_env$dashboard_data_path <- NULL

    # Create empty variables to store RAW data
    .microbs_env$df_empty_raw_ddPCR_data <- data.frame(matrix(ncol=9,nrow=0))
    colnames(.microbs_env$df_empty_raw_ddPCR_data) <- c('Sample',
                                    'Target_Name',
                                    'copies_uL',
                                    'Accepted_droplets',
                                    'Positive_droplets',
                                    'Negative_droplets',
                                    'PoissonConfMax',
                                    'PoissonConfMin',
                                    'dilution')

    .microbs_env$df_flu_ab_raw_ddPCR_data <- .microbs_env$df_empty_raw_ddPCR_data
    .microbs_env$df_flu_ab_raw_qPCR_data <- NULL

    .microbs_env$df_rsv_raw_ddPCR_data <- .microbs_env$df_empty_raw_ddPCR_data
    .microbs_env$df_rsc_raw_qPCR_data <- NULL

    .microbs_env$df_sars_raw_ddPCR_data <- .microbs_env$df_empty_raw_ddPCR_data
    .microbs_env$df_sars_raw_qPCR_data <- NULL

    # other data (flux, std. curve)
    .microbs_env$df_flux_data
    .microbs_env$df_std_curve_data

    # old data
    .microbs_env$df_old_raw_ddPCR_data <- NULL
    .microbs_env$df_old_raw_qPCR_data <- NULL

    # new raw data (loaded data)
    .microbs_env$df_new_raw_ddPCR_data
    .microbs_env$df_new_raw_qPCR_data

    # check data
    .microbs_env$df_old_check_ddPCR_data
    .microbs_env$df_new_check_ddPCR_data

    .microbs_env$df_old_check_qPCR_data
    .microbs_env$df_new_check_qPCR_data

    # calc data
    .microbs_env$df_old_calc_ddPCR_data
    .microbs_env$df_new_calc_ddPCR_data

    .microbs_env$df_old_calc_qPCR_data
    .microbs_env$df_new_calc_qPCR_data

    .microbs_env$sheet1_flu
    .microbs_env$sheet2_flu
    .microbs_env$sheet3_flu
    .microbs_env$sheet4_flu

}
# Decalring global paths to silence the Rmd warnings
# Initialize globals

# .microbs_env <- new.env(parent = emptyenv())
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
    .microbs_env$loaded_data_path <- NULL
    .microbs_env$calc_data_path <- NULL
    .microbs_env$created_data_path <- NULL

    # Create empty variables to store data
    .microbs_env$df_flu_raw_ddPCR_data <- NULL
}
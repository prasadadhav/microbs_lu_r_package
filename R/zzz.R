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

.onLoad <- function(libname, pkgname) {
    .microbs_env <<- new.env(parent = emptyenv())
    .microbs_env$wd <- NULL
    .microbs_env$path_connector <- NULL
    .microbs_env$ddPCR_raw_path <- NULL
    .microbs_env$qPCR_raw_path <- NULL 
    .microbs_env$stdCurve_path <- NULL
    .microbs_env$flux_path <- NULL
    .microbs_env$check_data_path <- NULL
}
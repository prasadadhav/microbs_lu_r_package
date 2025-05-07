#--------------------------------------------------------------------------------------------------------
# get Loaded ddPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get old raw loaded data for ddPCR
#' 
#' @description If it returns NULL, it means that the old data has not been loaded yet.
#' Call load_microbs_old_raw_ddPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `1_loaded_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_old_raw_ddPCR_Data()
#' 
#' @examples
#' result
#' 
#' @export 
get_microbs_old_raw_ddPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_raw_ddPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_raw_ddPCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_raw_ddPCR_Data()")
    }
    .microbs_env$df_old_raw_ddPCR_data
}

#--------------------------------------------------------------------------------------------------------
# get Loaded ddPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get new raw loaded data for ddPCR
#' 
#' @description If it returns NULL, it means that the new data has not been loaded yet.
#' Call load_microbs_old_raw_ddPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the new loaded RAW data located in `1_loaded_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_new_raw_ddPCR_Data()
#' 
#' @examples
#' result
#' 
#' @export 
get_microbs_new_raw_ddPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_raw_ddPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_raw_ddPCR_Data() function to set a path")
        message("or use the default by using load_microbs_raw_ddPCR_Data()")
    }
    .microbs_env$df_new_raw_ddPCR_data
}


#--------------------------------------------------------------------------------------------------------
# get Loaded qPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get old raw loaded data for qPCR
#' 
#' @description If it returns NULL, it means that the old data has not been loaded yet.
#' Call load_microbs_old_raw_qPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `1_loaded_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_old_raw_qPCR_Data()
#' 
#' @examples
#' result
#' 
#' @export 
get_microbs_old_raw_qPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_raw_qPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_raw_qPCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_raw_qPCR_Data()")
    }
    .microbs_env$df_old_raw_qPCR_data
}

#--------------------------------------------------------------------------------------------------------
# get checked ddPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get old calc data for ddPCR
#' 
#' @description If it returns NULL, it means that the old check data has not been loaded yet.
#' Call load_microbs_old_calc_ddPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `1_ckeck_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_old_check_ddPCR_Data()
#' 
#' @examples
#' result
#' 
#' @export 
get_microbs_old_check_ddPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_check_ddPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_check_ddPCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_check_ddPCR_Data()")
    }
    .microbs_env$df_old_check_ddPCR_data
}

#--------------------------------------------------------------------------------------------------------
# get checked qPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get old raw check data for qPCR
#' 
#' @description If it returns NULL, it means that the old calc data has not been loaded yet.
#' Call load_microbs_old_calc_qPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `1_ckeck_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_old_check_qPCR_Data()
#' 
#' @examples
#' result
#' 
#' @export 
get_microbs_old_check_ddPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_check_qPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_check_1PCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_check_qPCR_Data()")
    }
    .microbs_env$df_old_check_qPCR_data
}


#--------------------------------------------------------------------------------------------------------
# get calc ddPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get old calc data for ddPCR
#' 
#' @description If it returns NULL, it means that the old check data has not been loaded yet.
#' Call load_microbs_old_calc_ddPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `2_calc_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_old_calc_ddPCR_Data()
#' 
#' @examples
#' result
#' 
#' @export 
get_microbs_old_calc_ddPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_calc_ddPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_calc_ddPCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_calc_ddPCR_Data()")
    }
    .microbs_env$df_old_calc_ddPCR_data
}

#--------------------------------------------------------------------------------------------------------
# get calc qPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get old raw check data for qPCR
#' 
#' @description If it returns NULL, it means that the old calc data has not been loaded yet.
#' Call load_microbs_old_calc_qPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `2_calc_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_old_calc_qPCR_Data()
#' 
#' @examples
#' result
#' 
#' @export 
get_microbs_old_calc_qPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_calc_qPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_calc_qPCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_calc_qPCR_Data()")
    }
    .microbs_env$df_old_calc_qPCR_data
}

#--------------------------------------------------------------------------------------------------------
# get flux data
#--------------------------------------------------------------------------------------------------------
#' @title get the flux data
#' 
#' @description If it returns NULL, it means that the old calc data has not been loaded yet.
#' Call load_microbs_flux_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `2_calc_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_flux_Data()
#' 
#' @examples
#' result
#' 
#' @export 
get_microbs_flux_Data <- function() {
    if (is.null(.microbs_env$df_flux_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_flux_Data() function to set a path")
        message("or use the default by using load_microbs_flux_Data()")
    }
    .microbs_env$df_flux_data
}
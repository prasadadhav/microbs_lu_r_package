#--------------------------------------------------------------------------------------------------------
# get Loaded ddPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get path for the global working directory
#' 
#' @description If it returns NULL, it means that the path connector has not been set yet.
#' Call set_microbs_wdirectory() to set the path connector.
#' Add a string to set custom path connector.
#' Preferably use full path.
#' This function allows you to get the path connector between global directory & the data directories.
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
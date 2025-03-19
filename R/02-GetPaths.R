#--------------------------------------------------------------------------------------------------------
# get working direcotry
#--------------------------------------------------------------------------------------------------------
#' @title get path for the global working directory
#' 
#' @description If it returns NULL, it means that the path connector has not been set yet.
#' Call set_microbs_wdirectory() to set the path connector.
#' Add a string to set custom path connector.
#' Preferably use full path.
#' This function allows you to get the path connector between global directory & the data directories.
#' 
#' file ./microbs.lu/R/02_GetPaths.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_wd_dir()
#' 
#' @examples
#' result
#' 
#' @export 
get_microbs_wd_dir <- function() {
    if (is.null(.microbs_env$wd)) {
        message("[microbs Warning]: Did not yet set the path. Use the set_microbs_wdirectory(path=\"my/custom/path\") function to set a path")
        message("or use the default by using set_microbs_wdirectory()")
    }
    .microbs_env$wd
}


#--------------------------------------------------------------------------------------------------------
# get path connector
#--------------------------------------------------------------------------------------------------------
#' @title get path connector in which the data is saved
#' 
#' @description If it returns NULL, it means that the path connector has not been set yet.
#' Call set_microbs_connector_dir() to set the path connector.
#' Add a string to set custom path connector.
#' Only use the directory name, not the full path.
#' This function allows you to get the path connector between global directory & the data directories.
#' 
#' file ./microbs.lu/R/02_GetPaths.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_connector_dir()
#' 
#' @examples
#' result
#' 
#' @export 
get_microbs_connector_dir <- function() {
    if (is.null(.microbs_env$path_connector)) {
        message("[microbs Warning]: Did not yet set the path. Use the set_microbs_connector_dir(path=\"my/custom/path\") function to set a path")
        message("or use the default by using set_microbs_connector_dir()")
    }
    .microbs_env$path_connector
}

#--------------------------------------------------------------------------------------------------------
# get ddPCR raw data path
#--------------------------------------------------------------------------------------------------------
#' @title get ddPCR raw data path
#' 
#' @description If it returns NULL, it means that the path connector has not been set yet.
#' Call set_microbs_ddPCR_rawDataPath() to set the path connector.
#' Add a string to set custom path connector.
#' Only use the directory name, not the full path.
#' This function allows you to get the path connector between global directory & the data directories.
#' 
#' file ./microbs.lu/R/02_GetPaths.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_ddPCR_rawDataPath()
#' 
#' @examples
#' result
#' 
#' @export 
get_microbs_ddPCR_rawDataPath <- function() {
    if (is.null(.microbs_env$ddPCR_raw_path)) {
        message("[microbs Warning]: Did not yet set the path. Use the set_microbs_ddPCR_rawDataPath(path=\"my/custom/path\") function to set a path")
        message("or use the default by using set_microbs_ddPCR_rawDataPath()")
    }
    .microbs_env$ddPCR_raw_path
}

#--------------------------------------------------------------------------------------------------------
# get qPCR raw data path
#--------------------------------------------------------------------------------------------------------
#' @title get ddPCR raw data path
#' 
#' @description If it returns NULL, it means that the path connector has not been set yet.
#' Call set_microbs_qPCR_rawDataPath() to set the path connector.
#' Add a string to set custom path connector.
#' Only use the directory name, not the full path.
#' This function allows you to get the path connector between global directory & the data directories.
#' 
#' file ./microbs.lu/R/02_GetPaths.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_qPCR_rawDataPath()
#' result
#' 
#' @export 
get_microbs_qPCR_rawDataPath <- function() {
    if (is.null(.microbs_env$qPCR_raw_path)) {
        message("[microbs Warning]: Did not yet set the path. Use the set_microbs_qPCR_rawDataPath(path=\"my/custom/path\") function to set a path")
        message("or use the default by using set_microbs_qPCR_rawDataPath()")
    }
    .microbs_env$qPCR_raw_path
}

#--------------------------------------------------------------------------------------------------------
# get flux path
#--------------------------------------------------------------------------------------------------------
#' @title get flux data path
#' 
#' @description If it returns NULL, it means that the path connector has not been set yet.
#' Call set_microbs_flux_DataPath() to set the path connector.
#' Add a string to set custom path connector.
#' Only use the directory name, not the full path.
#' This function allows you to get the path connector between global directory & the data directories.
#' 
#' file ./microbs.lu/R/02_GetPaths.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_flux_DataPath()
#' result
#' 
#' @export 
get_microbs_flux_DataPath <- function() {
    if (is.null(.microbs_env$stdCurve_path)) {
        message("[microbs Warning]: Did not yet set the path. Use the set_microbs_flux_DataPath(path=\"my/custom/path\") function to set a path")
        message("or use the default by using set_microbs_flux_DataPath()")
    }
    .microbs_env$flux_path
}

#--------------------------------------------------------------------------------------------------------
# get standard curve path
#--------------------------------------------------------------------------------------------------------
#' @title get standard curve data path
#' 
#' @description If it returns NULL, it means that the path connector has not been set yet.
#' Call set_microbs_stdCurve_DataPath() to set the path connector.
#' Add a string to set custom path connector.
#' Only use the directory name, not the full path.
#' This function allows you to get the path connector between global directory & the data directories.
#' 
#' file ./microbs.lu/R/02_GetPaths.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_stdCurve_DataPath()
#' result
#' 
#' @export 
get_microbs_stdCurve_DataPath <- function() {
    if (is.null(.microbs_env$stdCurve_path)) {
        message("[microbs Warning]: Did not yet set the path. Use the set_microbs_stdCurve_DataPath(path=\"my/custom/path\") function to set a path")
        message("or use the default by using set_microbs_stdCurve_DataPath()")
    }
    .microbs_env$stdCurve_path
}

#--------------------------------------------------------------------------------------------------------
# get checked data path
#--------------------------------------------------------------------------------------------------------
#' @title get checked data path
#' 
#' @description If it returns NULL, it means that the path connector has not been set yet.
#' Call set_microbs_check_DataPath() to set the path connector.
#' Add a string to set custom path connector.
#' Only use the directory name, not the full path.
#' This function allows you to get the path connector between global directory & the data directories.
#' 
#' file ./microbs.lu/R/02_GetPaths.R
#' 
#' @examples
#' # Example usage
#' result <- get_microbs_check_DataPath()
#' result
#' 
#' @export 
get_microbs_check_DataPath <- function() {
    if (is.null(.microbs_env$stdCurve_path)) {
        message("[microbs Warning]: Did not yet set the path. Use the set_microbs_check_DataPath(path=\"my/custom/path\") function to set a path")
        message("or use the default by using set_microbs_check_DataPath()")
    }
    .microbs_env$stdCurve_path
}
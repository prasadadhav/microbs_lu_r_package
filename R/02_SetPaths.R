#' @title Process given paths
#' 
#' @param path A character string representing the path to be processed.
#' 
#' @details This function processes the given path and replaces the backslashes with forward slashes.
#' 
#' @return A character string representing the processed path.
#' @export 
process_win_path <- function(path) {
    if (grepl("\\\\", path)) {
        message("\033[31m[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\\'.\033[39m")
    }
    path <- gsub("\\\\", "/", path)
    return(path)
}

#' @title Set the Microbs Working Directory
#'
#' This function allows you to set the working directory for the package.
#'
#' @param path A character string representing the path to be set as the working directory. 
#' The default path is "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results".
#'
#' @details The working directory is the directory where the package will look for data files. 
#' If you want to use a custom path, make sure you have the folder structure & respective data in place.
#' **Note:** Use '/' instead of '\' in the path.
#'
#' @title Set Paths for microbs.lu Package
#' @description This script sets the paths for various directories used in the microbs.lu package.
#' @details The following directory structure is required for the package:
#' \itemize{
#'   \item \code{00_flux_data} - Directory for flux data.
#'   \item \code{0_raw_data_ddPCR} - Directory for raw ddPCR data.
#'   \item \code{0_raw_data_qPCR} - Directory for raw qPCR data.
#'   \item \code{1_loaded_data} - Directory for loaded data.
#'   \item \code{2_calc_data} - Directory for calculated data.
#'   \item \code{3_created_data} - Directory for created data.
#'   \item \code{4_data_4_dashboard} - Directory for data used in the dashboard.
#'   \item \code{microbs_runtime.R} - Script for runtime operations.
#' }
#' @file /d:/03_Workspace/01_R_Package/microbs.lu/R/02_SetPaths.R
#' @export 

set_microbs_wdirectory <- function(path = "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results") {
    # Check if the provide path is good.
    if (grepl("\\\\", path)) {
        message("\033[31m[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\\'.\033[39m")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results"
        message("\033[32m[microbs Report]: No path provided. Using default path: ", path, "\033[39m")
    }
    
    # Process the path and handle backslashes
    processed_path <- process_win_path(path)
    
    if (!dir.exists(processed_path)) {
        message("Warning: The provided path does not exist: ", processed_path)
    } else {
        message("Report: Using provided path: ", processed_path)
    }
    
    # Set the working directory
    wd <<- processed_path
    setwd(wd)
}


# set_microbs_wdirectory()
# set_microbs_wdirectory(path = "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results")
set_microbs_wdirectory(path = "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data")
# set_microbs_wdirectory(path = "D:\\03_Workspace\\01_R_Package\\microbs_lu_dummy_data")
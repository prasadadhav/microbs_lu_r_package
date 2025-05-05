# #--------------------------------------------------------------------------------------------------------
# #   Boilerplate code
# #--------------------------------------------------------------------------------------------------------
# #' @title Set Data base path
# #'
# #' @description This is an example function that demonstrates the structure of an R function.
# #' it adds two values and returns the result.
# #'
# #' @param param1 A number.
# #' @param param2 A second number.
# #' @return A description of the return value.
# #' @examples
# #' # Example usage
# #' result <- example_function(param1, param2)
# #' @export
# example_function <- function(param1 = 1, param2 = 2) {
#     # Function implementation goes here
#     result <- param1 + param2
#     return(result)
# }



#--------------------------------------------------------------------------------------------------------
#
#--------------------------------------------------------------------------------------------------------
#' @title Set the Microbs Working Directory
#'
#' @description 
#' This function allows you to set the working directory for the package. This funciton uses `setwd()`
#' to set the working directory. A default path is already provided for LIST.
#'
#' @param path A character string representing the path to be set as the working directory. 
#' The default path is "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results".
#' file ./microbs.lu/R/02_SetPaths.R
#'
#' @details The working directory is the directory where the package will look for data files. 
#' If you want to use a custom path, make sure you have the folder structure & respective data in place.
#' **Note:** Use '/' instead of '\' in the path.
#'
#' @title Set Paths for microbs.lu Package
#' @description This script sets the paths for various directories used in the microbs.lu package.
#' @details The following directory structure is required for the package, these directories are located inside :
#' .
#' ├── Data_Treatment
#' │   ├── 00_flux_data
#' │   ├── 00_standard_curve
#' │   ├── 0_raw_data_ddPCR
#' │   ├── 0_raw_data_qPCR
#' │   ├── 1_loaded_data
#' │   ├── 2_calc_data
#' │   ├── 3_created_data
#' │   └── 4_data_4_dashboard
#' ├── microbs_runtime.R
#' @export 

set_microbs_wdirectory <- function(path = "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results") {
    # Check if the provide path is good.
    if (grepl("\\\\", path)) {
        message("[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results"
        message("[microbs Report]: No path provided. Using default path: ", path)
    }
    
    if (!dir.exists(path)) {
        message("Error: The provided path does not exist: ", path)
    } else {
        message("Report: Using provided path: ", path)
    }
    
    # Set the working directory
    .microbs_env$wd <- path
    setwd(.microbs_env$wd)
}

#--------------------------------------------------------------------------------------------------------
#
#--------------------------------------------------------------------------------------------------------
#' @title Set path connector in which the data is saved
#' 
#' @description Within theteam there is a global directory, and within this directory there is another directory called "Data_Treatment".
#' And finally the data directories are within this directory.
#' This function allows you to set the path connector bwtween global directory & the data directories.
#' 
#' This is not really needed if you are using absolute paths (Complete path for example `/home/user/myWorkingDir/...`).
#' file ./microbs.lu/R/02_SetPaths.R
#'
#' @param path A character string representing the path connector. 
#' The default path is "Data_Treatment".
#' 
#' @examples
#' # Example usage
#' result <- set_microbs_connector_dir(path="Data_Treatment")
#' 
#' @examples
#' result
#' 
#' @export 
set_microbs_connector_dir <- function(path="Data_Treatment") {
    # Check if the provide path is good.
    if (grepl("\\\\", path)) {
        message("[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "Data_Treatment"
        .microbs_env$path_connector <- path
        message("[microbs Report]: No path provided. Using default path: ", path)
        return(invisible(path))  # Exit the function early
    }
    
    if (!dir.exists(path)) {
        message("Error: The provided path does not exist: ", path)
    } else {
        message("Report: Using provided path: ", path)
    }
    
    # Set the working directory
    # path_connector <- path
    .microbs_env$path_connector <- path
}

#--------------------------------------------------------------------------------------------------------
# qPCR paths
#--------------------------------------------------------------------------------------------------------
#' @title Set dPCR Raw Data Path
#'
#' @description The raw data from the qPCR tests are to be stored in the "0_raw_data_qPCR" directory.
#' It is assumed that the global directory is set using the set_microbs_wdirectory function.
#' Within this directory there is another director called "Data_Treatment", set using the function set_microbs_connector_dir.
#' This is just because the direectory structure within the team is like this.
#' It is not necessary to have a connector (in-between) directory.
#' This function allows you to set the path for the raw ddPCR data.
#' file ./microbs.lu/R/02_SetPaths.R
#'
#' @param path A character string representing the path to be set as the working directory. 
#' The default path is "0_raw_data_qPCR".
#' @param relative A Boolean to use relative path or not. Default is False.
#' 
#' @examples
#' # Example usage
#' set_microbs_wdirectory() # to set default working directory
#' set_microbs_connector_dir() # to set default connector directory
#' path <- "0_raw_data_qPCR"
#' result <- set_microbs_qPCR_rawDataPath(path)
#' result
#' 
#' @examples
#' path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_qPCR"
#' result <- set_microbs_qPCR_rawDataPath(path)
#' result
#' 
#' @export 
set_microbs_qPCR_rawDataPath <- function(path="0_raw_data_qPCR", relative=FALSE) {
    # Check if the provide path is good.
    if (grepl("\\\\", path)) {
        message("[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/0_raw_data_qPCR"
        message("[microbs Report]: No path provided. Using default path: ", path)
        .microbs_env$qPCR_raw_path <- path
        return(invisible(path))  # Exit the function early
    }
    
    if (relative) {
        path <- paste("./", path, sep="")
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided path does not exist: ", path)
        } else {
            .microbs_env$qPCR_raw_path <- path
            return(invisible(path))  # Exit the function early
            message("[microbs Report]: Using provided path: ", path)
        }
    }

    if (!dir.exists(path)) {
        message("Error: The provided path does not exist: ", path)
    } else {
        message("[microbs Report]: Using provided path: ", path)
    }

    path_connector <- get_microbs_connector_dir()
    wd <- get_microbs_wdirectory()
    
    # Set the working directory
    .microbs_env$qPCR_raw_path <- utils_microbs_path_builder(wd, path_connector, path)
}


#--------------------------------------------------------------------------------------------------------
# ddPCR paths
#--------------------------------------------------------------------------------------------------------
#' @title Set ddPCR Raw Data Path
#'
#' @description The raw data from the qPCR tests are to be stored in the "0_raw_data_ddPCR" directory.
#' It is assumed that the global directory is set using the set_microbs_wdirectory function.
#' Within this directory there is another director called "Data_Treatment", set using the function set_microbs_connector_dir.
#' This is just because the direectory structure within the team is like this.
#' It is not necessary to have a connector (in-between) directory.
#' This function allows you to set the path for the raw ddPCR data.
#' file ./microbs.lu/R/02_SetPaths.R
#'
#' @param path A character string representing the path to be set as the working directory. 
#' The default path is "0_raw_data_ddPCR".
#' @param relative A Boolean to use relative path or not. Default is False.
#' 
#' @examples
#' # Example usage
#' set_microbs_wdirectory() # to set default working directory
#' set_microbs_connector_dir() # to set default connector directory
#' path <- "0_raw_data_ddPCR"
#' result <- set_microbs_qPCR_rawDataPath(path)
#' result
#' 
#' @examples
#' path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_ddPCR"
#' result <- set_microbs_qPCR_rawDataPath(path)
#' result
#'
#' @export 
set_microbs_ddPCR_rawDataPath <- function(path="0_raw_data_ddPCR", relative=FALSE) {
    # Check if the provide path is good.
    if (grepl("\\\\", path)) {
        message("[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/0_raw_data_ddPCR"
        message("[microbs Report]: No path provided. Using default path: ", path)
        .microbs_env$ddPCR_raw_path <- path
        return(invisible(path))  # Exit the function early
    }
    
    # Check and set relative path to working directory
    if (relative) {
        path <- paste("./", path, sep="")
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided path does not exist: ", path)
        } else {
            .microbs_env$ddPCR_raw_path <- path
            return(invisible(path))  # Exit the function early
            message("[microbs Report]: Using provided path: ", path)
        }
    }

    if (!dir.exists(path)) {
        message("Error: The provided path does not exist: ", path)
    } else {
        message("[microbs Report]: Using provided path: ", path)
    }

    wd <- get_microbs_wdirectory()
    path_connector <- get_microbs_connector_dir()
    
    # Set the working directory
    .microbs_env$ddPCR_raw_path <- utils_microbs_path_builder(wd, path_connector, path)
}

#--------------------------------------------------------------------------------------------------------
# Flux info path
#--------------------------------------------------------------------------------------------------------
#' @title Set flux Data Path
#'
#' @description The data for the flux/flow is to be stored in the "00_flux_data" directory.
#' It is assumed that the global directory is set using the set_microbs_wdirectory function.
#' Within this directory there is another director called "Data_Treatment", set using the function set_microbs_connector_dir.
#' This is just because the direectory structure within the team is like this.
#' It is not necessary to have a connector (in-between) directory.
#' This function allows you to set the path for the raw ddPCR data.
#' file ./microbs.lu/R/02_SetPaths.R
#'
#' @param path A character string representing the path to be set as the working directory. 
#' The default path is "00_flux_data".
#' @param relative A Boolean to use relative path or not. Default is False.
#' 
#' @examples
#' # Example usage
#' set_microbs_wdirectory() # to set default working directory
#' set_microbs_connector_dir() # to set default connector directory
#' path <- "00_flux_data"
#' result <- set_microbs_flux_DataPath(path)
#' result
#' 
#' @examples
#' path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/00_flux_data"
#' result <- set_microbs_flux_DataPath(path)
#' result
#'
#' @export 
set_microbs_flux_DataPath <- function(path="00_flux_data", relative=FALSE) {
    # Check if the provide path is good.
    if (grepl("\\\\", path)) {
        message("[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/00_flux_data"
        message("[microbs Report]: No path provided. Using default path: ", path)
        .microbs_env$flux_path <- path
        return(invisible(path))  # Exit the function early
    }
    
    # Check and set relative path to working directory
    if (relative) {
        path <- paste("./", path, sep="")
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided path does not exist: ", path)
        } else {
            .microbs_env$flux_path <- path
            return(invisible(path))  # Exit the function early
            message("[microbs Report]: Using provided path: ", path)
        }
    }

    if (!dir.exists(path)) {
        message("Error: The provided path does not exist: ", path)
    } else {
        message("[microbs Report]: Using provided path: ", path)
    }

    wd <- get_microbs_wdirectory()
    path_connector <- get_microbs_connector_dir()
    
    # Set the working directory
    .microbs_env$flux_path <- utils_microbs_path_builder(wd, path_connector, path)
}

#--------------------------------------------------------------------------------------------------------
# SARS-CoV-2 paths
#--------------------------------------------------------------------------------------------------------
# Do we really to set this?
# If things are standardized we do not need to set this
# TODO: rotate back to this once you progress into the calculations




#--------------------------------------------------------------------------------------------------------
# Info Target paths (CT curve data)
#--------------------------------------------------------------------------------------------------------
#' @title Set CT curve data path
#'
#' @description The CT curve data are to be stored in the "00_standard_curve" directory.
#' It is assumed that the global directory is set using the set_microbs_wdirectory function.
#' Within this directory there is another director called "Data_Treatment", set using the function set_microbs_connector_dir.
#' This is just because the direectory structure within the team is like this.
#' It is not necessary to have a connector (in-between) directory.
#' This function allows you to set the path for the CT standard curve data.
#' file ./microbs.lu/R/02_SetPaths.R
#'
#' @param path A character string representing the path to be set as the working directory. 
#' The default path is "00_standard_curve".
#' @param relative A Boolean to use relative path or not. Default is False.
#' 
#' @examples
#' # Example usage
#' set_microbs_wdirectory() # to set default working directory
#' set_microbs_connector_dir() # to set default connector directory
#' path <- "00_standard_curve"
#' result <- set_microbs_stdCurve_DataPath(path)
#' result
#' 
#' @examples
#' path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/00_standard_curve"
#' result <- set_microbs_stdCurve_DataPath(path)
#' result
#'
#' @export 
set_microbs_stdCurve_DataPath <- function(path="00_standard_curve", relative=FALSE) {
    # Check if the provide path is good.
    if (grepl("\\\\", path)) {
        message("[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/00_standard_curve"
        message("[microbs Report]: No path provided. Using default path: ", path)
        .microbs_env$stdCurve_path <- path
        return(invisible(path))  # Exit the function early
    }
    
    # Check and set relative path to working directory
    if (relative) {
        path <- paste("./", path, sep="")
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided path does not exist: ", path)
        } else {
            .microbs_env$stdCurve_path <- path
            return(invisible(path))  # Exit the function early
            message("[microbs Report]: Using provided path: ", path)
        }
    }

    if (!dir.exists(path)) {
        message("Error: The provided path does not exist: ", path)
    } else {
        message("[microbs Report]: Using provided path: ", path)
    }

    wd <- get_microbs_wdirectory()
    path_connector <- get_microbs_connector_dir()
    
    # Set the working directory
    .microbs_env$stdCurve_path <- utils_microbs_path_builder(wd, path_connector, path)
}


#--------------------------------------------------------------------------------------------------------
# Check data
#--------------------------------------------------------------------------------------------------------
#' @title Set check data path
#'
#' @description The CT curve data are to be stored in the "1_ckeck_data" directory.
#' The raw data is loaded and saved into "1_loaded_data".
#' Then we need to check and report the loaded raw data.
#' file ./microbs.lu/R/02_SetPaths.R
#'
#' @param path A character string representing the path to be set as the working directory. 
#' The default path is "1_ckeck_data".
#' @param relative A Boolean to use relative path or not. Default is False.
#' 
#' @examples
#' # Example usage
#' set_microbs_wdirectory() # to set default working directory
#' set_microbs_connector_dir() # to set default connector directory
#' path <- "1_ckeck_data"
#' result <- set_microbs_stdCurve_DataPath(path)
#' result
#' 
#' @examples
#' path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_ckeck_data"
#' result <- set_microbs_check_DataPath(path)
#' result
#'
#' @export 
set_microbs_check_DataPath <- function(path="1_ckeck_data", relative=FALSE) {
    # Check if the provide path is good.
    if (grepl("\\\\", path)) {
        message("[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/1_ckeck_data"
        message("[microbs Report]: No path provided. Using default path: ", path)
        .microbs_env$checkData_path <- path
        return(invisible(path))  # Exit the function early
    }
    
    # Check and set relative path to working directory
    if (relative) {
        path <- paste("./", path, sep="")
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided path does not exist: ", path)
        } else {
            .microbs_env$checkData_path <- path
            return(invisible(path))  # Exit the function early
            message("[microbs Report]: Using provided path: ", path)
        }
    }

    if (!dir.exists(path)) {
        message("Error: The provided path does not exist: ", path)
    } else {
        message("[microbs Report]: Using provided path: ", path)
    }

    wd <- get_microbs_wdirectory()
    path_connector <- get_microbs_connector_dir()
    
    # Set the working directory
    .microbs_env$checkData_path <- utils_microbs_path_builder(wd, path_connector, path)
}


#--------------------------------------------------------------------------------------------------------
# Loaded data path 
#--------------------------------------------------------------------------------------------------------
#' @title Set path for loaded data for `SUPERVIR_RAW_DATA_ddPCR_*` & `SUPERVIR_RAW_DATA_qPCR_*`
#' @description Sets the path to store loaded data in the "1_loaded_data" directory.
#' This assumes the working directory was set with `set_microbs_wdirectory()` and optionally a connector directory with `set_microbs_connector_dir()`.
#' @param path Character string path to use. Default is "1_loaded_data".
#' @param relative Boolean. If TRUE, makes path relative to current working directory. Default is FALSE.
#' @param build_path Boolean. If TRUE, builds path using working and connector directories. Default is FALSE.
#' @return The constructed path invisibly.
#' @export
set_microbs_loaded_DataPath <- function(path = "1_loaded_data", relative = FALSE, build_path = FALSE) {

    # Warn about backslashes
    if (grepl("\\\\", path)) {
        message("[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\\'.")
    }

    # Use default hardcoded path if path is missing and build_path is FALSE
    if (missing(path) && !build_path) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/1_loaded_data"
        message("[microbs Report]: No path provided. Using default path: ", path)
        .microbs_env$loaded_data_path <- path
        return(invisible(path))
    }

    # If relative path
    if (relative) {
        path <- file.path(".", path)
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided relative path does not exist: ", path)
            return(invisible(NULL))
        }
        message("[microbs Report]: Using relative path: ", path)
        .microbs_env$loaded_data_path <- path
        return(invisible(path))
    }

    # If full path is provided
    if (!build_path) {
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided path does not exist: ", path)
            return(invisible(NULL))
        }
        message("[microbs Report]: Using provided path: ", path)
        .microbs_env$loaded_data_path <- path
        return(invisible(path))
    }

    # build_path = TRUE → Build from wd + connector + given path
    wd <- get_microbs_wdirectory()
    path_connector <- get_microbs_connector_dir()
    full_path <- utils_microbs_path_builder(wd, path_connector, path)
    if (!dir.exists(full_path)) {
        message("[microbs Error]: The built path does not exist: ", full_path)
        return(invisible(NULL))
    }
    message("[microbs Report]: Using built path: ", full_path)
    .microbs_env$loaded_data_path <- full_path
    return(invisible(full_path))
}



#--------------------------------------------------------------------------------------------------------
# Calc data path 
#--------------------------------------------------------------------------------------------------------
#' @title Set path for created data for SUPERVIR_CAL_DATA_*PCR_*` 
#'
#' @description The calculated data are to be stored in the "2_calc_data" directory.
#' It is assumed that the global directory is set using the set_microbs_wdirectory function.
#' Within this directory there is another director called "Data_Treatment", set using the function set_microbs_connector_dir.
#' This is just because the direectory structure within the team is like this.
#' It is not necessary to have a connector (in-between) directory.
#' This function allows you to set the path for the checked data.
#' file ./microbs.lu/R/02_SetPaths.R
#'
#' @param path A character string representing the path to be set as the working directory. 
#' The default path is "2_calc_data".
#' @param relative A Boolean to use relative path or not. Default is False.
#' 
#' @examples
#' # Example usage
#' set_microbs_wdirectory() # to set default working directory
#' set_microbs_connector_dir() # to set default connector directory
#' path <- "2_calc_data"
#' result <- set_microbs_calc_DataPath(path)
#' result
#' 
#' @examples
#' path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/2_calc_data"
#' result <- set_microbs_calc_DataPath(path)
#' result
#'
#' @export 
set_microbs_calc_DataPath <- function(path="2_calc_data", relative=FALSE) {
    # Check if the provide path is good.
    if (grepl("\\\\", path)) {
        message("[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/2_calc_data"
        message("[microbs Report]: No path provided. Using default path: ", path)
        .microbs_env$calc_data_path <- path
        return(invisible(path))  # Exit the function early
    }
    
    # Check and set relative path to working directory
    if (relative) {
        path <- paste("./", path, sep="")
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided path does not exist: ", path)
        } else {
            .microbs_env$calc_data_path <- path
            return(invisible(path))  # Exit the function early
            message("[microbs Report]: Using provided path: ", path)
        }
    }

    if (!dir.exists(path)) {
        message("Error: The provided path does not exist: ", path)
    } else {
        message("[microbs Report]: Using provided path: ", path)
    }

    wd <- get_microbs_wdirectory()
    path_connector <- get_microbs_connector_dir()
    
    # Set the working directory
    .microbs_env$calc_data_path <- utils_microbs_path_builder(wd, path_connector, path)
}





#--------------------------------------------------------------------------------------------------------
# Created data path 
#--------------------------------------------------------------------------------------------------------
#' @title Set path for created data for `SUPERVIR_*_AGG_*` 
#'
#' @description The created data are to be stored in the "3_created_data" directory.
#' It is assumed that the global directory is set using the set_microbs_wdirectory function.
#' Within this directory there is another director called "Data_Treatment", set using the function set_microbs_connector_dir.
#' This is just because the direectory structure within the team is like this.
#' It is not necessary to have a connector (in-between) directory.
#' This function allows you to set the path for the checked data.
#' file ./microbs.lu/R/02_SetPaths.R
#'
#' @param path A character string representing the path to be set as the working directory. 
#' The default path is "3_created_data".
#' @param relative A Boolean to use relative path or not. Default is False.
#' 
#' @examples
#' # Example usage
#' set_microbs_wdirectory() # to set default working directory
#' set_microbs_connector_dir() # to set default connector directory
#' path <- "3_created_data"
#' result <- set_microbs_created_DataPath(path)
#' result
#' 
#' @examples
#' path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/3_created_data"
#' result <- set_microbs_created_DataPath(path)
#' result
#'
#' @export 
set_microbs_created_DataPath <- function(path="3_created_data", relative=FALSE) {
    # Check if the provide path is good.
    if (grepl("\\\\", path)) {
        message("[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/3_created_data"
        message("[microbs Report]: No path provided. Using default path: ", path)
        .microbs_env$created_data_path <- path
        return(invisible(path))  # Exit the function early
    }
    
    # Check and set relative path to working directory
    if (relative) {
        path <- paste("./", path, sep="")
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided path does not exist: ", path)
        } else {
            .microbs_env$created_data_path <- path
            return(invisible(path))  # Exit the function early
            message("[microbs Report]: Using provided path: ", path)
        }
    }

    if (!dir.exists(path)) {
        message("Error: The provided path does not exist: ", path)
    } else {
        message("[microbs Report]: Using provided path: ", path)
    }

    wd <- get_microbs_wdirectory()
    path_connector <- get_microbs_connector_dir()
    
    # Set the working directory
    .microbs_env$created_data_path <- utils_microbs_path_builder(wd, path_connector, path)
}







#--------------------------------------------------------------------------------------------------------
# Dashboard data path 
#--------------------------------------------------------------------------------------------------------
#' @title Set path for dashboard data
#'
#' @description The final data for dashboard are to be stored in the "4_data_4_dashboard" directory.
#' It is assumed that the global directory is set using the set_microbs_wdirectory function.
#' Within this directory there is another director called "Data_Treatment", set using the function set_microbs_connector_dir.
#' This is just because the direectory structure within the team is like this.
#' It is not necessary to have a connector (in-between) directory.
#' This function allows you to set the path for the checked data.
#' file ./microbs.lu/R/02_SetPaths.R
#'
#' @param path A character string representing the path to be set as the working directory. 
#' The default path is "4_data_4_dashboard".
#' @param relative A Boolean to use relative path or not. Default is False.
#' 
#' @examples
#' # Example usage
#' set_microbs_wdirectory() # to set default working directory
#' set_microbs_connector_dir() # to set default connector directory
#' path <- "4_data_4_dashboard"
#' result <- set_microbs_dashboard_DataPath(path)
#' result
#' 
#' @examples
#' path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/4_data_4_dashboard"
#' result <- set_microbs_dashboard_DataPath(path)
#' result
#'
#' @export 
set_microbs_dashboard_DataPath <- function(path="4_data_4_dashboard", relative=FALSE) {
    # Check if the provide path is good.
    if (grepl("\\\\", path)) {
        message("[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/4_data_4_dashboard"
        message("[microbs Report]: No path provided. Using default path: ", path)
        .microbs_env$dashboard_data_path <- path
        return(invisible(path))  # Exit the function early
    }
    
    # Check and set relative path to working directory
    if (relative) {
        path <- paste("./", path, sep="")
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided path does not exist: ", path)
        } else {
            .microbs_env$dashboard_data_path <- path
            return(invisible(path))  # Exit the function early
            message("[microbs Report]: Using provided path: ", path)
        }
    }

    if (!dir.exists(path)) {
        message("Error: The provided path does not exist: ", path)
    } else {
        message("[microbs Report]: Using provided path: ", path)
    }

    wd <- get_microbs_wdirectory()
    path_connector <- get_microbs_connector_dir()
    
    # Set the working directory
    .microbs_env$dashboard_data_path <- utils_microbs_path_builder(wd, path_connector, path)
}
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
        message("\033[31m[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.\033[39m")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results"
        message("\033[32m[microbs Report]: No path provided. Using default path: ", path, "\033[39m")
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
        message("\033[31m[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.\033[39m")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "Data_Treatment"
        message("\033[32m[microbs Report]: No path provided. Using default path: ", path, "\033[39m")
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
        message("\033[31m[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.\033[39m")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/0_raw_data_qPCR"
        message("\033[32m[microbs Report]: No path provided. Using default path: ", path, "\033[39m")
        qPCR_raw_path <- path
        return(invisible(path))  # Exit the function early
    }
    
    if (relative) {
        path <- paste("./", path, sep="")
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided path does not exist: ", path)
        } else {
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
    wd <- get_microbs_wd_dir()
    
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
        message("\033[31m[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.\033[39m")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/0_raw_data_ddPCR"
        message("\033[32m[microbs Report]: No path provided. Using default path: ", path, "\033[39m")
        ddPCR_raw_path <- path
        return(invisible(path))  # Exit the function early
    }
    
    # Check and set relative path to working directory
    if (relative) {
        path <- paste("./", path, sep="")
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided path does not exist: ", path)
        } else {
            return(invisible(path))  # Exit the function early
            message("[microbs Report]: Using provided path: ", path)
        }
    }

    if (!dir.exists(path)) {
        message("Error: The provided path does not exist: ", path)
    } else {
        message("[microbs Report]: Using provided path: ", path)
    }

    wd <- get_microbs_wd_dir()
    path_connector <- get_microbs_connector_dir()
    
    # Set the working directory
    .microbs_env$ddPCR_raw_path <- utils_microbs_path_builder(wd, path_connector, path)
}

#--------------------------------------------------------------------------------------------------------
# 
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
        message("\033[31m[microbs Report]: Detected backslashes in the path. Please use forward slashes '/' instead of backslashes '\'.\033[39m")
    }

    # Check if path is missing, use default if it is
    if (missing(path)) {
        path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/0_raw_data_ddPCR"
        message("\033[32m[microbs Report]: No path provided. Using default path: ", path, "\033[39m")
        ddPCR_raw_path <- path
        return(invisible(path))  # Exit the function early
    }
    
    # Check and set relative path to working directory
    if (relative) {
        path <- paste("./", path, sep="")
        if (!dir.exists(path)) {
            message("[microbs Error]: The provided path does not exist: ", path)
        } else {
            return(invisible(path))  # Exit the function early
            message("[microbs Report]: Using provided path: ", path)
        }
    }

    if (!dir.exists(path)) {
        message("Error: The provided path does not exist: ", path)
    } else {
        message("[microbs Report]: Using provided path: ", path)
    }

    wd <- get_microbs_wd_dir()
    path_connector <- get_microbs_connector_dir()
    
    # Set the working directory
    .microbs_env$ddPCR_raw_path <- utils_microbs_path_builder(wd, path_connector, path)
}

#--------------------------------------------------------------------------------------------------------
# SARS-CoV-2 paths
#--------------------------------------------------------------------------------------------------------





#--------------------------------------------------------------------------------------------------------
# Info Target paths (CT curve data)
#--------------------------------------------------------------------------------------------------------






#--------------------------------------------------------------------------------------------------------
# Info Week path 
#--------------------------------------------------------------------------------------------------------






#--------------------------------------------------------------------------------------------------------
# R program paths
#--------------------------------------------------------------------------------------------------------






#--------------------------------------------------------------------------------------------------------
# 
#--------------------------------------------------------------------------------------------------------






#--------------------------------------------------------------------------------------------------------
# 
#--------------------------------------------------------------------------------------------------------

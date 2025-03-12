#--------------------------------------------------------------------------------------------------------
#   Path builder
#--------------------------------------------------------------------------------------------------------
#' @title Builds the complete path
#'
#' @description This is an example function that demonstrates the structure of an R function.
#'
#' @param global_dir_path A string that is the global directory path.
#' @param connector A string is the connector between the global directory path & the local directory path.
#' @param local_dir_path A string, that is usually the data directory paths, example `0_raw_data_qPCR`.
#' @return A string that is a complete path, rather than a relavtive path known in the repo.
#' @examples
#' # Example usage
#' global_dir_path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results"
#' connector <- "Data_Treatment"
#' local_dir_path <- "0_raw_data_qPCR"
#' result <- utils_path_builder(global_dir_path, connector, local_dir_path)
#' print(result) # "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/0_raw_data_qPCR"
#' @export
utils_microbs_path_builder <- function(global_dir_path, connector, local_dir_path) {
    # Function implementation goes here
    result <- paste(global_dir_path,"/",connector,"/",local_dir_path, sep="")
    return(result)
}
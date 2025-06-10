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
#' result <- utils_microbs_path_builder(global_dir_path, connector, local_dir_path)
#' print(result) 
#' @export
utils_microbs_path_builder <- function(global_dir_path, connector, local_dir_path) {
    # Function implementation goes here
    result <- paste(global_dir_path,"/",connector,"/",local_dir_path, sep="")
    return(result)
}









#--------------------------------------------------------------------------------------------------------
#   silent/quite mode
#--------------------------------------------------------------------------------------------------------
#' @title Run the code silently without any outputs
#'
#' @description This function suppersses all the warnings, messages, print, cat etc.
#'
#' @param expr Any code R code block
#' @param quiet A bool to switch the silent mode on or off

#' @examples
#' # Example usage
#' 
#' quiet_mode <- TRUE
#' 
#' run_microbs_silently({
#'      # ------------------------------------------------------
#'      # ----------------#  start Code block  # ---------------
#'      # ------------------------------------------------------
#' 
#'      print("this message won't be shown if quiet_mode TRUE")
#' 
#'      # ------------------------------------------------------
#'      # ----------------#   end Code block   # ---------------
#'      # ------------------------------------------------------
#' }, quiet = quiet_mode)
#' 
#' 
#' 
#' @export
run_microbs_silently <- function(expr, quiet = TRUE) {
  if (quiet) {
    invisible(
      capture.output(
        suppressMessages(
          suppressWarnings(
            eval(expr)
          )
        )
      )
    )
  } else {
    eval(expr)
  }
}



#--------------------------------------------------------------------------------------------------------
#   Extract WWTP 
#--------------------------------------------------------------------------------------------------------
#' @title Extracts the first few alphabets that signify the WWTP
#'
#' @description This is an example function that demonstrates the structure of an R function.
#'
#' @param column A column with strings
#' @return A column of strings with extract only first alphabets
#' 
#' ^([A-Za-z]+) → captures the letters at the beginning
#' [0-9]+.*$ → skips the rest (digits and any trailing letter like d)
#' 
#' @export
utils_extract_WWTP <- function(sample_column) {
  sub("^([A-Za-z]+)[0-9]+.*$", "\\1", sample_column)
}


#--------------------------------------------------------------------------------------------------------
#   Generate weeks
#--------------------------------------------------------------------------------------------------------
#' @title Generate the sequence of weeks for the given years
#'
#' @description This function generates yyyy_ww format for the given year
#'
#' @param column A column with strings
#' @return A column of strings with extract only first alphabets
#' 
#' ^([A-Za-z]+) → captures the letters at the beginning
#' [0-9]+.*$ → skips the rest (digits and any trailing letter like d)
#'
generate_weeks <- function(start_year, end_year) {
    # Créer une séquence de dates pour chaque lundi de chaque semaine
    dates <- seq.Date(from = as.Date(paste0(start_year, "-01-01")),
                      to = as.Date(paste0(end_year, "-12-31")),
                      by = "week")
    # Extraire l'année et la semaine en format "YYYY_WW"
    weeks <- format(dates, "%Y_%V")
    return(unique(weeks))
}
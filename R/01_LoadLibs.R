# #' Load required libraries for the package
# #'
# #' This function loads all the necessary libraries for the package.
# #' Add any new library used in the package to this function.
# #' 
# #' @return NULL
# #' @export
# load_required_libraries <- function() {
#     library(readxl)      # For reading Excel files
#     library(tidyverse)   # For data manipulation and visualization
#     library(utils)       # For various utility functions
#     library(janitor)     # For data cleaning
#     library(plyr)        # For data manipulation (must be loaded before dplyr)
#     library(dplyr)       # For data manipulation
#     library(writexl)     # For writing Excel files
#     library(lubridate)   # For working with dates and times
#     library(tidyr)       # For tidying data
#     return(NULL)
# }

# # Load the libraries when the script is sourced
# load_required_libraries()
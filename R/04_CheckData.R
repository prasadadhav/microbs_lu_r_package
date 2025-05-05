#--------------------------------------------------------------------------------------------------------
# Check the loaded ddPCR raw data that is saved in the latest SUPERVIR_RAW_DATA_ddPCR_*.xlxs
#--------------------------------------------------------------------------------------------------------
#' @title here we check the loaded dataset
#'
#' @description The loaded raw data needs to be checked once it is loaded.
#'
#' @param path_to_check_data A string to describe the path to the loaded ddPCR data. 
#' 
#' @return A character vector of archived file names (invisibly).
#' @examples
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_check_data <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_ddPCR"
#' df_raw_ddPCR_data <- archive_microbs_raw_ddPCR_Data(path_to_check_data)
#' @export
check_microbs_raw_ddPCR_Data <- function(path_to_check_data = .microbs_env$checkData_path) {
    # load the data 
    if (missing(path_to_check_data)) {
        path_to_check_data = get_microbs_loaded_DataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_check_data)
    }

    df_old_raw_ddPCR_data <- get_microbs_old_raw_ddPCR_Data()

    # Check if all the samples before 240 are only present once
    # May be it is for old testing method, don't know why this is the case
    df_mutable <- df_old_raw_ddPCR_data
    df_mutable$Sample2 <- as.numeric(gsub("\\D", "", x$Sample))
    df_mutable <- df_mutable %>% dplyr::filter(Sample2 <= 240)
    check_df_old_raw_ddPCR_data <- df_mutable[
        (duplicated(df_mutable[c("Sample", "Target_Name")]) |
        duplicated(df_mutable[c("Sample", "Target_Name")], fromLast = TRUE)), 
    ]

    check_df_old_raw_ddPCR_data <- check_df_old_raw_ddPCR_data[, c("Sample", 
                                                                    "Target_Name", 
                                                                    "copies_uL", 
                                                                    "Accepted_droplets", 
                                                                    "Positive_droplets", 
                                                                    "Negative_droplets", 
                                                                    "PoissonConfMax", 
                                                                    "PoissonConfMin", 
                                                                    "dilution"
                                                                    )]

    # check for samples lower than 10.000 droplets
    low_droplet <- subset(df_old_raw_ddPCR_data, Accepted_droplets < 10000)
    check_df_old_raw_ddPCR_data <- dplyr::bind_rows(check_df_old_raw_ddPCR_data, low_droplet)
    check_df_old_raw_ddPCR_data <- check_df_old_raw_ddPCR_data %>% dplyr::arrange(desc(Sample)) %>% dplyr::distinct()
    check_df_old_raw_ddPCR_data$Sample2 <-as.numeric(gsub("\\D", "", check_df_old_raw_ddPCR_data$Sample))

    # if the droplets are less than 10.000, we add a comment "to redo" or "A refaire"
    check_df_old_raw_ddPCR_data <- check_df_old_raw_ddPCR_data %>%
        dplyr::add_count(Sample2, Target_Name, name = "count") %>%  # Adds count column directly
        dplyr::mutate(Commentaire = if_else(count == 1, "A refaire", NA_character_)) %>%
        dplyr::select(-Sample2, -count)

    check_df_old_raw_ddPCR_data$Keep_delete <- ""
    check_df_old_raw_ddPCR_data$Date_check <- Sys.Date()

    # Comparison with an existing check_file
    







}
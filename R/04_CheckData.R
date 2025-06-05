#' @import magrittr
NULL

#--------------------------------------------------------------------------------------------------------
# Check the loaded ddPCR raw data that is saved in the latest SUPERVIR_RAW_DATA_ddPCR_*.xlxs
#--------------------------------------------------------------------------------------------------------
#' @title here we check the loaded dataset
#'
#' @description The loaded raw data needs to be checked once it is loaded.
#'
#' @param path_to_check_data A string to describe the path to the loaded ddPCR data. 
#' 
#' @return A tibble of checked data. # TODO
#' @examples
#' \dontrun{
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' 
#' path_to_old_raw_excel_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"
#' df_raw_ddPCR_data <- load_microbs_old_raw_ddPCR_Data(path_to_old_raw_excel_ddPCR)
#' 
#' path_to_check_data <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_ckeck_data"
#' df_check_ddPCR_data <- load_microbs_old_check_ddPCR_Data(path_to_check_data)
#' check_microbs_raw_ddPCR_Data(path_to_check_data)
#' }
#' @export
check_microbs_raw_ddPCR_Data <- function(path_to_check_data = .microbs_env$checkData_path) {
    # load the data 
    if (missing(path_to_check_data)) {
        path_to_check_data = get_microbs_check_DataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_check_data)
    }

    # Here we need to access the loaded data (the latest will ideally have the newly loaded data)
    df_loaded_raw_ddPCR_data <- get_microbs_old_raw_ddPCR_Data()
    df_old_check_ddPCR_data <- get_microbs_old_check_ddPCR_Data()

    # Check if all the samples before 240 are only present once
    # May be it is for old testing method, don't know why this is the case
    df_mutable <- df_loaded_raw_ddPCR_data
    df_mutable$Sample2 <- as.numeric(gsub("\\D", "", df_mutable$Sample))
    df_mutable <- df_mutable %>% dplyr::filter(Sample2 <= 240)
    check_df_loaded_raw_ddPCR_data <- df_mutable[
        (duplicated(df_mutable[c("Sample", "Target_Name")]) |
        duplicated(df_mutable[c("Sample", "Target_Name")], fromLast = TRUE)), 
    ]

    check_df_loaded_raw_ddPCR_data <- check_df_loaded_raw_ddPCR_data[, c("Sample", 
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
    low_droplet <- subset(df_loaded_raw_ddPCR_data, Accepted_droplets < 10000)
    check_df_loaded_raw_ddPCR_data <- dplyr::bind_rows(check_df_loaded_raw_ddPCR_data, low_droplet)
    check_df_loaded_raw_ddPCR_data <- check_df_loaded_raw_ddPCR_data %>% dplyr::arrange(desc(Sample)) %>% dplyr::distinct()
    check_df_loaded_raw_ddPCR_data$Sample2 <-as.numeric(gsub("\\D", "", check_df_loaded_raw_ddPCR_data$Sample))

    # if the droplets are less than 10.000, we add a comment "to redo" or "A refaire"
    check_df_loaded_raw_ddPCR_data <- check_df_loaded_raw_ddPCR_data %>%
        dplyr::add_count(Sample2, Target_Name, name = "count") %>%  # Adds count column directly
        dplyr::mutate(Commentaire = dplyr::if_else(count == 1, "A refaire", NA_character_)) %>%
        dplyr::select(-Sample2, -count)

    check_df_loaded_raw_ddPCR_data$Keep_delete <- ""
    check_df_loaded_raw_ddPCR_data$Date_check <- Sys.Date()

    # Comparison with an existing check_file
    file_info <- subset(utils::fileSnapshot(path_to_check_data)$info,
                        utils::fileSnapshot(path_to_check_data)$info$isdir==FALSE)
    if (all(!grepl("ddPCR",rownames(file_info)))){
        writexl::write_xlsx(check_df_loaded_raw_ddPCR_data, 
                            paste0(path_to_check_data, "/", "Check_data_ddPCR_", 
                            gsub(':','-',sub(' CEST','',substr(Sys.time(),start=1,stop=19))),
                            ".xlsx"),
                col_names = T)
    }

    if (is.null(df_old_check_ddPCR_data)) {
        df_old_check_ddPCR_data <- check_df_loaded_raw_ddPCR_data[0, ]  # empty df with same structure
    }

    diff_check <- dplyr::anti_join(check_df_loaded_raw_ddPCR_data, df_old_check_ddPCR_data, by = c("Sample","Target_Name"))
    if(nrow(diff_check) == 0){
        print("No sample (ddPCR) was added since the last update")
    }

    df_new_check_ddPCR_data <- NULL

    if(nrow(diff_check)!=0){
        df_new_check_ddPCR_data <- rbind(df_old_check_ddPCR_data, diff_check)
        df_new_check_ddPCR_data <- df_new_check_ddPCR_data[order(df_new_check_ddPCR_data$Target_Name, df_new_check_ddPCR_data$Sample),]

        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Sheet1")
        openxlsx::writeData(wb, "Sheet1", df_new_check_ddPCR_data)
        openxlsx::freezePane(wb, sheet = "Sheet1", firstRow = TRUE)
        xlxs_filename <- paste0(path_to_check_data, "/Check_data_ddPCR_",
                    gsub(":", "-", sub(" CEST", "", Sys.time())),
                    ".xlsx")
        openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)
    }

    .microbs_env$df_new_check_ddPCR_data <- df_new_check_ddPCR_data

    .microbs_env$df_new_check_ddPCR_data
}



#' @import magrittr
NULL

#--------------------------------------------------------------------------------------------------------
# Check the loaded qPCR raw data that is saved in the latest SUPERVIR_RAW_DATA_qPCR_*.xlxs
#--------------------------------------------------------------------------------------------------------
#' @title here we check the loaded dataset
#'
#' @description The loaded raw data needs to be checked once it is loaded.
#'
#' @param path_to_check_data A string to describe the path to the loaded qPCR data. 
#' 
#' @return A tibble of checked data. # TODO
#' @examples
#' \dontrun{
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' 
#' path_to_old_raw_excel_qPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"
#' df_raw_qPCR_data <- load_microbs_old_raw_qPCR_Data(path_to_old_raw_excel_qPCR)
#' 
#' path_to_check_data <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_ckeck_data"
#' df_check_qPCR_data <- load_microbs_old_check_qPCR_Data(path_to_check_data)
#' check_microbs_raw_qPCR_Data(path_to_check_data)
#' }
#' @export
check_microbs_raw_qPCR_Data <- function(path_to_check_data = .microbs_env$checkData_path) {
    # load the data 
    if (missing(path_to_check_data)) {
        path_to_check_data = get_microbs_check_DataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_check_data)
    }

    # TODO

    # # Here we need to access the loaded data (the latest will ideally have the newly loaded data)
    # df_loaded_raw_qPCR_data <- get_microbs_old_raw_qPCR_Data()
    # df_old_check_qPCR_data <- get_microbs_old_check_qPCR_Data()

    # # Check if all the samples before 240 are only present once
    # # May be it is for old testing method, don't know why this is the case
    # df_mutable <- df_loaded_raw_qPCR_data
    # df_mutable$Sample2 <- as.numeric(gsub("\\D", "", df_mutable$Sample))
    # df_mutable <- df_mutable %>% dplyr::filter(Sample2 <= 240)
    # check_df_loaded_raw_qPCR_data <- df_mutable[
    #     (duplicated(df_mutable[c("Sample", "Target_Name")]) |
    #     duplicated(df_mutable[c("Sample", "Target_Name")], fromLast = TRUE)), 
    # ]

    # check_df_loaded_raw_qPCR_data <- check_df_loaded_raw_qPCR_data[, c("Sample", 
    #                                                                 "Target_Name", 
    #                                                                 "copies_uL", 
    #                                                                 "Accepted_droplets", 
    #                                                                 "Positive_droplets", 
    #                                                                 "Negative_droplets", 
    #                                                                 "PoissonConfMax", 
    #                                                                 "PoissonConfMin", 
    #                                                                 "dilution"
    #                                                                 )]

    # # check for samples lower than 10.000 droplets
    # low_droplet <- subset(df_loaded_raw_qPCR_data, Accepted_droplets < 10000)
    # check_df_loaded_raw_qPCR_data <- dplyr::bind_rows(check_df_loaded_raw_qPCR_data, low_droplet)
    # check_df_loaded_raw_qPCR_data <- check_df_loaded_raw_qPCR_data %>% dplyr::arrange(desc(Sample)) %>% dplyr::distinct()
    # check_df_loaded_raw_qPCR_data$Sample2 <-as.numeric(gsub("\\D", "", check_df_loaded_raw_qPCR_data$Sample))

    # # if the droplets are less than 10.000, we add a comment "to redo" or "A refaire"
    # check_df_loaded_raw_qPCR_data <- check_df_loaded_raw_qPCR_data %>%
    #     dplyr::add_count(Sample2, Target_Name, name = "count") %>%  # Adds count column directly
    #     dplyr::mutate(Commentaire = dplyr::if_else(count == 1, "A refaire", NA_character_)) %>%
    #     dplyr::select(-Sample2, -count)

    # check_df_loaded_raw_qPCR_data$Keep_delete <- ""
    # check_df_loaded_raw_qPCR_data$Date_check <- Sys.Date()

    # # Comparison with an existing check_file
    # file_info <- subset(utils::fileSnapshot(path_to_check_data)$info,
    #                     utils::fileSnapshot(path_to_check_data)$info$isdir==FALSE)
    # if (all(!grepl("qPCR",rownames(file_info)))){
    #     writexl::write_xlsx(check_df_loaded_raw_qPCR_data, 
    #                         paste0(path_to_check_data, "/", "Check_data_qPCR_", 
    #                         gsub(':','-',sub(' CEST','',substr(Sys.time(),start=1,stop=19))),
    #                         ".xlsx"),
    #             col_names = T)
    # }

    # if (is.null(df_old_check_qPCR_data)) {
    #     df_old_check_qPCR_data <- check_df_loaded_raw_qPCR_data[0, ]  # empty df with same structure
    # }

    # diff_check <- dplyr::anti_join(check_df_loaded_raw_qPCR_data, df_old_check_qPCR_data, by = c("Sample","Target_Name"))
    # if(nrow(diff_check) == 0){
    #     print("No sample (qPCR) was added since the last update")
    # }

    # if(nrow(diff_check)!=0){
    #     df_new_check_qPCR_data <- rbind(df_old_check_qPCR_data, diff_check)
    #     df_new_check_qPCR_data <- df_new_check_qPCR_data[order(df_new_check_qPCR_data$Target_Name, df_new_check_qPCR_data$Sample),]

    #     wb <- openxlsx::createWorkbook()
    #     openxlsx::addWorksheet(wb, "Sheet1")
    #     openxlsx::writeData(wb, "Sheet1", df_new_check_qPCR_data)
    #     openxlsx::freezePane(wb, sheet = "Sheet1", firstRow = TRUE)
    #     xlxs_filename <- paste0(path_to_check_data, "/Check_data_qPCR_",
    #                 gsub(":", "-", sub(" CEST", "", Sys.time())),
    #                 ".xlsx")
    #     openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)
    # }

    # .microbs_env$df_new_check_qPCR_data
}
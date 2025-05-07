# In load data, we take the CSV files from RAW data directories and save it in SUPERVIR_RAW_DATA_ddPCR_** excel files.
# Here we need to load different datasets coming from the experiments

# Usually they were loaded as `ddPCR` and `qPCR`, however, this is incovenient as the calculations for each virus
# can be different given that is it `ddPCR` ir `qPCR`.
#--------------------------------------------------------------------------------------------------------
# Load the SUPERVIR_RAW_DATA_ddPCR_*
#--------------------------------------------------------------------------------------------------------
#' @title Load the RAW data ddPCR if it exists (or create one if absent)
#'
#' @description This function loads the old ddPCR data and stores for later usage.
#' This file contains the old ddPCR raw data. The new loaded data will be added to this data.
#'
#' @param path_to_old_raw_excel_ddPCR A string to describe the path to the RAW excel ddPCR data `SUPERVIR_RAW_DATA_ddPCR_*.xlxs`. 
#' 
#' @return A tibble with the containing the raw data
#' @examples
#' # Example usage
#' path_to_old_raw_excel_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"
#' df_raw_ddPCR_data <- load_microbs_old_raw_ddPCR_Data(path_to_old_raw_excel_ddPCR)
#' 
#' # If you want to use the default path
#' set_microbs_loaded_DataPath()
#' df_raw_ddPCR_data <- load_microbs_old_raw_ddPCR_Data() # use default path
#' ddPCR_df <- df_raw_ddPCR_data$data
#' ddPCR_latest_file <- df_raw_ddPCR_data$latest_ddPCR_file
#'
#' @export
load_microbs_old_raw_ddPCR_Data <- function(path_to_old_raw_excel_ddPCR = .microbs_env$loaded_data_path) {
    # load the path
    if ( missing(path_to_old_raw_excel_ddPCR) ) {
        path_to_old_raw_excel_ddPCR <- get_microbs_loaded_DataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_old_raw_excel_ddPCR)
    }

    # Validate if the provided path exists
    if (!dir.exists(path_to_old_raw_excel_ddPCR)) {
        stop("[microbs Error]: The provided path does not exist: ", path_to_old_raw_excel_ddPCR)
    }

    # Check if Archives exist, if not create a directory
    ifelse(!dir.exists(file.path(path_to_old_raw_excel_ddPCR, "Archives")), 
            dir.create(file.path(path_to_old_raw_excel_ddPCR, "Archives")), FALSE)

    # load the names of all files and folders in the given directory path
    file_info <- utils::fileSnapshot(path_to_old_raw_excel_ddPCR)$info
    # remove all the directories from the list
    file_info <- subset(file_info, file_info$isdir == FALSE)
    # only keep the file names containg "RAW_DATA_ddPCR"
    file_info <- subset(file_info,grepl("RAW_DATA_ddPCR",rownames(file_info)))
    
    # Check if we have atleast one file
    if (nrow(file_info) == 0) {
        stop("[microbs Error]: No matching files found in directory: ", path_to_old_raw_excel_ddPCR)
    }

    # re-order the files according to time
    file_info <- file_info[order(file_info$mtime,decreasing = TRUE),]
    # Loop through files to select the latest Excel file
    latest_file <- NULL
    for (file_name in rownames(file_info)) {
        latest_file <- rownames(file_info)[which.max(file_info$mtime)]
        if (grepl("\\.xlsx$|\\.xls$", file_name, ignore.case = TRUE)) { # Ensure it's an Excel file
            latest_file <- file_name
        }
    }
    message("[microbs Report]: Load the latest RAW_DATA_ddPCR_* file: ", latest_file)

    # create the file path
    latest_excel_file_path <- file.path(path_to_old_raw_excel_ddPCR, latest_file)

    # Load the latest excel file for ddPCR
    .microbs_env$df_old_raw_ddPCR_data <-  readxl::read_excel(latest_excel_file_path)

    # Return both the loaded dataframe and the latest file name
    return(list(
        data <- .microbs_env$df_old_raw_ddPCR_data,
        latest_ddPCR_file <- latest_file
    ))
}

#--------------------------------------------------------------------------------------------------------
# Move old Raw ddPCR (SUPERVIR_RAW_DATA_ddPCR_*) to archive
#--------------------------------------------------------------------------------------------------------
#' @title move ddPCR data
#'
#' @description The old loaded data needs to be moved to the archive once new file is available.
#'
#' @param path_to_old_raw_excel_ddPCR A string to describe the path to the loaded data. 
#' 
#' @return A character vector of archived file names (invisibly).
#' @examples
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_old_raw_excel_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"
#' list_of_loaded_ddPCR_files_archived <- archive_microbs_loaded_ddPCR_Data(path_to_old_raw_excel_ddPCR)
#' @export
archive_microbs_loaded_ddPCR_Data <- function(path_to_old_raw_excel_ddPCR = .microbs_env$loaded_data_path) {
    # load the data 
    if (missing(path_to_old_raw_excel_ddPCR)) {
        path_to_old_raw_excel_ddPCR = get_microbs_loaded_DataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_old_raw_excel_ddPCR)
    }

    # load all the names of the CSV files
    invisible(ifelse(!dir.exists(file.path(path_to_old_raw_excel_ddPCR, "Archives")), 
                        dir.create(file.path(path_to_old_raw_excel_ddPCR, "Archives")), 
                        FALSE))

    # TODO: Here use a specific format lookup such as "\\.csv$", because csv will pickup the file even if we have file.csv.txt
    file_info <- utils::fileSnapshot(path_to_old_raw_excel_ddPCR)$info
    file_info <- subset(file_info, file_info$isdir == FALSE)
    file_info <- subset(file_info,grepl("SUPERVIR_RAW_DATA_ddPCR_",rownames(file_info)))

    if (nrow(file_info) == 0) {
        stop("[microbs Error]: No matching files found in directory: ", path_to_old_raw_excel_ddPCR)
    }

    file_info <- file_info[order(file_info$mtime,decreasing = TRUE),]

    latest_file <- NULL
    for (file_name in rownames(file_info)) {
        latest_file <- rownames(file_info)[which.max(file_info$mtime)]
        if (grepl("\\.xlsx$|\\.xls$", file_name, ignore.case = TRUE)) { # Ensure it's an Excel file
            latest_file <- file_name
        }
    }

    files_list <- c() # create empty list to store the names of the files archived

    for (file in rownames(file_info)) {
        file_path <- file.path(path_to_old_raw_excel_ddPCR, file)

        if (file == latest_file) {
            next  # Skip the latest file
        } else {
            archive_path <- file.path(path_to_old_raw_excel_ddPCR, "Archives", file)
            file.copy(from = file_path, to = archive_path)
            file.remove(file_path)

            # Add archived file name to list
            files_list <- c(files_list, file)
        }
    }

    if (length(files_list) == 0) {
        message("[microbs Report]: No SUPERVIR_RAW_DATA_ddPCR_* files found to archive in ", path_to_old_raw_excel_ddPCR)
        return(invisible(character(0)))
    }

    invisible(files_list)
}


#--------------------------------------------------------------------------------------------------------
# Load the SUPERVIR_RAW_DATA_qPCR_*
#--------------------------------------------------------------------------------------------------------
#' @title Load the RAW data qPCR if it exists (or create one if absent)
#'
#' @description This function loads the old qPCR data and stores for later usage.
#' This file contains the old qPCR raw data. The new loaded data will be added to this data.
#'
#' @param path_to_old_raw_excel_qPCR A string to describe the path to the RAW excel qPCR data `SUPERVIR_RAW_DATA_qPCR_*.xlxs`. 
#' 
#' @return A tibble with the containing the raw data
#' @examples
#' # Example usage
#' path_to_old_raw_excel_qPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"
#' df_raw_qPCR_data <- load_microbs_old_raw_qPCR_Data(path_to_old_raw_excel_qPCR)
#' 
#' # If you want to use the default path
#' set_microbs_loaded_DataPath()
#' df_raw_qPCR_data <- load_microbs_old_raw_qPCR_Data() # use default path
#' qPCR_df <- df_raw_qPCR_data$data
#' qPCR_latest_file <- df_raw_qPCR_data$latest_qPCR_file
#' 
#' @export
load_microbs_old_raw_qPCR_Data <- function(path_to_old_raw_excel_qPCR = .microbs_env$loaded_data_path) {
    # load the path
    if ( missing(path_to_old_raw_excel_qPCR) ) {
        path_to_old_raw_excel_qPCR <- get_microbs_loaded_DataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_old_raw_excel_qPCR)
    }

    # Validate if the provided path exists
    if (!dir.exists(path_to_old_raw_excel_qPCR)) {
        stop("[microbs Error]: The provided path does not exist: ", path_to_old_raw_excel_qPCR)
    }

    # Check if Archives exist, if not create a directory
    ifelse(!dir.exists(file.path(path_to_old_raw_excel_qPCR, "Archives")), 
            dir.create(file.path(path_to_old_raw_excel_qPCR, "Archives")), FALSE)

    # load the names of all files and folders in the given directory path
    file_info <- utils::fileSnapshot(path_to_old_raw_excel_qPCR)$info
    # remove all the directories from the list
    file_info <- subset(file_info, file_info$isdir == FALSE)
    # only keep the file names containg "RAW_DATA_qPCR"
    file_info <- subset(file_info,grepl("RAW_DATA_qPCR",rownames(file_info)))
    
    # Check if we have atleast one file
    if (nrow(file_info) == 0) {
        stop("[microbs Error]: No matching files found in directory: ", path_to_old_raw_excel_qPCR)
    }

    # re-order the files according to time
    file_info <- file_info[order(file_info$mtime, decreasing = TRUE),]
    # Loop through files to select the latest Excel file
    latest_file <- NULL
    for (file_name in rownames(file_info)) {
        latest_file <- rownames(file_info)[which.max(file_info$mtime)]
        if (grepl("\\.xlsx$|\\.xls$", file_name, ignore.case = TRUE)) { # Ensure it's an Excel file
            latest_file <- file_name
        }
    }
    message("[microbs Report]: Load the latest RAW_DATA_qPCR_* file: ", latest_file)

    # create the file path
    latest_excel_file_path <- file.path(path_to_old_raw_excel_qPCR, latest_file)

    # Load the latest excel file for qPCR
    .microbs_env$df_old_raw_qPCR_data <-  readxl::read_excel(latest_excel_file_path)

    # Return both the loaded dataframe and the latest file name
    return(list(
        data <- .microbs_env$df_old_raw_qPCR_data,
        latest_qPCR_file <- latest_file
    ))
}


#--------------------------------------------------------------------------------------------------------
# Move old Raw qPCR (SUPERVIR_RAW_DATA_qPCR_*) to archive
#--------------------------------------------------------------------------------------------------------
#' @title move qPCR data
#'
#' @description The old loaded data needs to be moved to the archive once new file is available.
#'
#' @param path_to_old_raw_excel_qPCR A string to describe the path to the loaded data. 
#' 
#' @return A character vector of archived file names (invisibly).
#' @examples
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_old_raw_excel_qPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"
#' list_of_loaded_qPCR_files_archived <- archive_microbs_loaded_qPCR_Data(path_to_old_raw_excel_qPCR)
#' @export
archive_microbs_loaded_qPCR_Data <- function(path_to_old_raw_excel_qPCR = .microbs_env$loaded_data_path) {
    # load the data 
    if (missing(path_to_old_raw_excel_qPCR)) {
        path_to_old_raw_excel_qPCR = get_microbs_loaded_DataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_old_raw_excel_qPCR)
    }

    # load all the names of the CSV files
    invisible(ifelse(!dir.exists(file.path(path_to_old_raw_excel_qPCR, "Archives")), 
                        dir.create(file.path(path_to_old_raw_excel_qPCR, "Archives")), 
                        FALSE))

    # TODO: Here use a specific format lookup such as "\\.csv$", because csv will pickup the file even if we have file.csv.txt
    file_info <- utils::fileSnapshot(path_to_old_raw_excel_qPCR)$info
    file_info <- subset(file_info, file_info$isdir == FALSE)
    file_info <- subset(file_info,grepl("SUPERVIR_RAW_DATA_qPCR_",rownames(file_info)))

    if (nrow(file_info) == 0) {
        stop("[microbs Error]: No matching files found in directory: ", path_to_old_raw_excel_qPCR)
    }

    file_info <- file_info[order(file_info$mtime,decreasing = TRUE),]

    latest_file <- NULL
    for (file_name in rownames(file_info)) {
        latest_file <- rownames(file_info)[which.max(file_info$mtime)]
        if (grepl("\\.xlsx$|\\.xls$", file_name, ignore.case = TRUE)) { # Ensure it's an Excel file
            latest_file <- file_name
        }
    }

    files_list <- c() # create empty list to store the names of the files archived

    for (file in rownames(file_info)) {
        file_path <- file.path(path_to_old_raw_excel_qPCR, file)

        if (file == latest_file) {
            next  # Skip the latest file
        } else {
            archive_path <- file.path(path_to_old_raw_excel_qPCR, "Archives", file)
            file.copy(from = file_path, to = archive_path)
            file.remove(file_path)

            # Add archived file name to list
            files_list <- c(files_list, file)
        }
    }

    if (length(files_list) == 0) {
        message("[microbs Report]: No SUPERVIR_RAW_DATA_qPCR_* files found to archive in ", path_to_old_raw_excel_qPCR)
        return(invisible(character(0)))
    }

    invisible(files_list)
}

#--------------------------------------------------------------------------------------------------------
# Load new Raw ddPCR
#--------------------------------------------------------------------------------------------------------
#' @title Load ddPCR RAW data
#'
#' @description Usually the tests for Flu A and Flu b are done together. We also have hRSV data from ddPCR in same directory.
#' Hence the raw data is available in a single file. This functions needs the previous paths be described
#' as we are also using the old loaded raw data.
#'
#' @param path_to_raw_ddPCR A string to describe the path to the RAW ddPCR data. 
#' 
#' @return A tibble with the containing the raw data
#' @examples
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_old_raw_excel_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"
#' set_microbs_loaded_DataPath(path_to_old_raw_excel_ddPCR)
#' load_microbs_old_raw_ddPCR_Data(path_to_old_raw_excel_ddPCR)
#' path_to_raw_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_ddPCR"
#' df_raw_ddPCR_data <- load_microbs_raw_ddPCR_Data(path_to_raw_ddPCR)
#' @export
load_microbs_raw_ddPCR_Data <- function(path_to_raw_ddPCR = .microbs_env$ddPCR_raw_path) {
    # load the data 
    if (missing(path_to_raw_ddPCR)) {
        # path_to_raw_ddPCR <- get_microbs_ddPCR_rawDataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_raw_ddPCR)
    }

    # get the old data
    df_old_raw_ddPCR_data <- get_microbs_old_raw_ddPCR_Data()
    df_new_raw_ddPCR_data <- df_old_raw_ddPCR_data

    # if there is no old data we have to creat an empty df

    # load all the names of the CSV files
    files_list <- list.files(path_to_raw_ddPCR, pattern = "csv")
    for(file in files_list){
        file_name <- paste(path_to_raw_ddPCR, file, sep = "/")
        file <- suppressMessages(utils::read.csv(file_name, sep = ","))

        file <- dplyr::select(file, c( Sample.description.1,
                                Target,Conc.copies.µL.,
                                Accepted.Droplets,
                                Positives,
                                Negatives,
                                PoissonConfMax,
                                PoissonConfMin
                                ))

        colnames(file) <- c('Sample',
                            'Target_Name',
                            'copies_uL',
                            'Accepted_droplets',
                            'Positive_droplets',
                            'Negative_droplets',
                            'PoissonConfMax',
                            'PoissonConfMin')
        
        file <- subset(file,toupper(Target_Name) != "RPP30")
        patterns <- c('NTC',"IAV","IBV","RPP30","T+","INFLU")

        file <- file %>%
                    dplyr::filter(!('Sample' %in% patterns))

        file <- subset(file, !grepl("No Call", copies_uL))

        for(j in 1 : nrow(file)){
            file <- file %>%
                dplyr::mutate(dilution = case_when(
                    grepl("D$", Sample) ~ 2,      # if the name ends with "D", value = 2
                    grepl("T$", Sample) ~ 3,      # if the name ends with "D", value = 3
                    grepl("d$", Sample) ~ 10,
                    TRUE ~ 0                    # if not, value = 0
                ))

            # Module: Add new Virus name here
            file$Target_Name <- case_when(
                file$Target_Name == "InfluA" | file$Target_Name == "IAV" | file$Target_Name == "Influ A" ~ "FluA",
                file$Target_Name == "InfluB" | file$Target_Name == "IBV" | file$Target_Name == "Influ B" ~ "FluB",
                file$Target_Name == "RSV" | file$Target_Name == "hRSV" | file$Target_Name == "rsv" ~ "hRSV",
                .default = as.character(file$Target_Name)
            )
            df_new_raw_ddPCR_data <- plyr::rbind.fill(df_new_raw_ddPCR_data, file[j,])
        }
    }

    for (i in 1:nrow(df_new_raw_ddPCR_data)){
        if (grepl("d$", df_old_raw_ddPCR_data$Sample[i]) ){
            df_new_raw_ddPCR_data$Sample[i] <- substr(df_new_raw_ddPCR_data$Sample[i], 1, 7)
        } else {
            df_new_raw_ddPCR_data$Sample[i] <- substr(df_new_raw_ddPCR_data$Sample[i], 1, 6)
        }
    }

    # Keep only uniqe rows
    df_new_raw_ddPCR_data <- dplyr::distinct(df_new_raw_ddPCR_data)

    # Order the samples by Target name and Sample.
    df_new_raw_ddPCR_data <- df_new_raw_ddPCR_data[
                                            with(df_new_raw_ddPCR_data, 
                                                order(Target_Name, Sample)
                                                ),
                                            # leave empty for column, only operate on rows
                                            ]

    path_to_old_raw_excel_ddPCR <- get_microbs_loaded_DataPath()

    # write the new data into the file
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Sheet1")
    openxlsx::writeData(wb, "Sheet1", df_new_raw_ddPCR_data)
    openxlsx::freezePane(wb, sheet = "Sheet1", firstRow = TRUE)
    xlxs_filename <- paste0(path_to_old_raw_excel_ddPCR, "/SUPERVIR_RAW_DATA_qPCR_",
                   gsub(":", "-", sub(" CEST", "", Sys.time())),
                   ".xlsx")
    openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)

    # writexl::write_xlsx(df_new_raw_ddPCR_data, 
    #                     paste0(path_to_old_raw_excel_ddPCR,"/","SUPERVIR_RAW_DATA_ddPCR_", # build the filename with path
    #                     gsub(':','-',sub(' CEST','',Sys.time())), # substitute ":" with "-" in the time
    #                     ".xlsx"), # save as excel
    #                     firstRow = TRUE # freeze the first row
    #                     )  

    .microbs_env$df_new_raw_ddPCR_data <- df_new_raw_ddPCR_data

    return(.microbs_env$df_new_raw_ddPCR_data)
}


#--------------------------------------------------------------------------------------------------------
# Move old loaded ddPCR to archive
#--------------------------------------------------------------------------------------------------------
#' @title move ddPCR data
#'
#' @description The processed raw data needs to be moved to the archive once it is loaded.
#'
#' @param path_to_raw_ddPCR A string to describe the path to the RAW ddPCR data. 
#' 
#' @return A character vector of archived file names (invisibly).
#' @examples
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_raw_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_ddPCR"
#' df_raw_ddPCR_data <- archive_microbs_raw_ddPCR_Data(path_to_raw_ddPCR)
#' @export
archive_microbs_raw_ddPCR_Data <- function(path_to_raw_ddPCR = .microbs_env$ddPCR_raw_path) {
    # load the data 
    if (missing(path_to_raw_ddPCR)) {
        path_to_raw_ddPCR = get_microbs_ddPCR_rawDataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_raw_ddPCR)
    }

    # load all the names of the CSV files
    invisible(ifelse(!dir.exists(file.path(path_to_raw_ddPCR, "Archives")), 
                        dir.create(file.path(path_to_raw_ddPCR, "Archives")), 
                        FALSE))

    # TODO: Here use a specific format lookup such as "\\.csv$", because csv will pickup the file even if we have file.csv.txt
    files_list <- list.files(path_to_raw_ddPCR, pattern = "csv")
    for(file in files_list) {
        file_name <- paste(path_to_raw_ddPCR, file, sep = "/")
        file.copy(from = file_name, 
                    to = paste0(file.path(path_to_raw_ddPCR, "Archives","/"),
                                file
                    ))
        file.remove(from = file_name)
    }

    if (length(files_list) == 0) {
        message("[microbs Report]: No CSV files found to archive in ", path_to_raw_ddPCR)
        return(invisible(character(0)))
    }

    invisible(files_list)
}



#--------------------------------------------------------------------------------------------------------
# Load new Raw qPCR
#--------------------------------------------------------------------------------------------------------
#' @title Load qPCR RAW data
#'
#' @description Usually the tests for SARS-CoV-2 is done with qPCR.
#' Hence the raw data is available in a single file. This functions needs the previous paths be described
#' as we are also using the old loaded raw data.
#'
#' @param path_to_raw_qPCR A string to describe the path to the RAW qPCR data. 
#' 
#' @return A tibble with the containing the raw data
#' @examples
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' 
#' path_to_old_raw_excel_qPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"
#' set_microbs_loaded_DataPath(path_to_old_raw_excel_qPCR)
#' 
#' load_microbs_old_raw_qPCR_Data(path_to_old_raw_excel_qPCR)
#' 
#' path_to_raw_qPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_qPCR"
#' df_raw_qPCR_data <- load_microbs_raw_qPCR_Data(path_to_raw_qPCR)
#' @export
load_microbs_raw_qPCR_Data <- function(path_to_raw_qPCR = .microbs_env$qPCR_raw_path) {
    # load the data 
    if (missing(path_to_raw_qPCR)) {
        # path_to_raw_qPCR <- get_microbs_qPCR_rawDataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_raw_qPCR)
    }

    # get the old data
    df_old_raw_qPCR_data <- get_microbs_old_raw_qPCR_Data()
    df_new_raw_qPCR_data <- df_old_raw_qPCR_data

    # if there is no old data we have to creat an empty df

    # load all the names of the CSV files
    files_list <- list.files(path_to_raw_qPCR, pattern = "xls")
    for(file in files_list){
        file_name <- paste(path_to_raw_qPCR, file, sep = "/")
        file <- suppressMessages(readxl::read_excel(name_file, sheet=1,skip=0,col_names = FALSE))

        if(file[1,1]=="Block Type"){
            file_mod <- file %>% dplyr::filter(!is.na(...6))
            file_mod <- file_mod[, c("...1", "...2","...3", "...4", "...5", "...6")]

            names(file_mod) <- file_mod[1,]
            file_mod <- file_mod[-1,]

            file_mod <- file_mod[!is.na(file_mod$`Sample Name`),]
            if(ncol(file_mod) == 7){
                file_mod <- subset (file_mod, select = -c(Well,`Well Position`,Task,`Ct Threshold`)) # Drop some columns not needed
            }

            if(ncol(file_mod)==6){
                file_mod <- subset (file_mod, select = -c(Well,`Well Position`,`Ct Threshold`)) # Drop some columns not needed
            }

            file_mod <- file_mod[-contains(c('T-','T+', "T+ RSVA", "T+ SARSCOV2", "T- H2O"),
                                     vars = c(file_mod$`Sample Name`)),]

            colnames(file_mod) <- c('Sample','Target_Name','CT')
        }

        for(j in 1:nrow(file_mod)){
            if (nchar(file_mod$Sample[j])==4){
                file_mod$Sample[j] <- gsub("^(.{3})(.*)$","\\100\\2",file_mod$Sample[j])
            }

            if((nchar(file_mod$Sample[j]) < 6 | nchar(file_mod$Sample[j])==7) & substr(file_mod$Sample[j], 1, 3) %in% c("BET","BEG","PET","SCH", "BLE", "MER", "HES","ECH", "UEB", "GRE", "VIE", "BOE", "WIL")) {
                file_mod$Sample[j] <- gsub("^(.{3})(.*)$","\\10\\2",file_mod$Sample[j])
            }

            # Module: Add new Virus name here
            file_mod$Target_Name <- case_when(
                file_mod$Target_Name == "IAV" ~ "FluA",
                file_mod$Target_Name == "SARS-COV-2" | file_mod$Target_Name == "SARSCOV2" | file_mod$Target_Name == "E-gene" ~ "SARS-CoV-2",
                file_mod$Target_Name == "RSV" ~ "hRSV",
                .default = as.character(file_mod$Target_Name)
            )

            if(substr(file_mod$Sample[j], 1, 3)  %in% c("BET","BEG","PET","SCH", "BLE", "MER", "HES","ECH", "UEB", "GRE", "VIE", "BOE", "WIL")){
                    df_new_raw_qPCR_data <- plyr::rbind.fill(df_new_raw_qPCR_data , file_mod[j,])
                }
                else{
                    df_new_raw_qPCR_data  <- plyr::rbind.fill(df_new_raw_qPCR_data , file_mod[j,])
                }
        }
    }

    temp_df <- subset(df_new_raw_qPCR_data, CT != "Undetermined")
    temp_df_undeterminded <- subset(df_new_raw_qPCR_data, CT == "Undetermined")

    # Keep only uniqe rows
    df_new_raw_qPCR_data <- dplyr::distinct(temp_df)
    df_new_raw_qPCR_data <- plyr::rbind.fill(df_new_raw_qPCR_data, temp_df_undeterminded)

    # Order the samples by Target name and Sample.
    df_new_raw_qPCR_data <- df_new_raw_qPCR_data[
                                            with(df_new_raw_qPCR_data, 
                                                order(Target_Name, Sample)
                                                ),
                                            # leave empty for column, only operate on rows
                                            ]

    path_to_old_raw_excel_qPCR <- get_microbs_loaded_DataPath()

    # write the new data into the file
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Sheet1")
    openxlsx::writeData(wb, "Sheet1", df_new_raw_qPCR_data)
    openxlsx::freezePane(wb, sheet = "Sheet1", firstRow = TRUE)
    xlxs_filename <- paste0(path_to_old_raw_excel_qPCR, "/SUPERVIR_RAW_DATA_qPCR_",
                   gsub(":", "-", sub(" CEST", "", Sys.time())),
                   ".xlsx")
    openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)

    # writexl::write_xlsx(df_new_raw_qPCR_data, 
    #                     paste0(path_to_old_raw_excel_qPCR,"/","SUPERVIR_RAW_DATA_qPCR_", # build the filename with path
    #                     gsub(':','-',sub(' CEST','',Sys.time())), # substitute ":" with "-"
    #                     ".xlsx") # save as excel
    #                     )  

    .microbs_env$df_new_raw_qPCR_data <- df_new_raw_qPCR_data

    return(.microbs_env$df_new_raw_qPCR_data)
}


#--------------------------------------------------------------------------------------------------------
# Move new Raw qPCR to archive
#--------------------------------------------------------------------------------------------------------
#' @title move qPCR Raw data
#'
#' @description The processed raw data needs to be moved to the archive once it is loaded.
#'
#' @param path_to_raw_qPCR A string to describe the path to the RAW qPCR data. 
#' 
#' @return A character vector of archived file names (invisibly).
#' @examples
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_raw_qPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_qPCR"
#' df_raw_qPCR_data <- archive_microbs_raw_qPCR_Data(path_to_raw_qPCR)
#' @export
archive_microbs_raw_qPCR_Data <- function(path_to_raw_qPCR = .microbs_env$qPCR_raw_path) {
    # load the data 
    if (missing(path_to_raw_qPCR)) {
        path_to_raw_qPCR = get_microbs_qPCR_rawDataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_raw_qPCR)
    }

    # load all the names of the CSV files
    invisible(ifelse(!dir.exists(file.path(path_to_raw_qPCR, "Archives")), 
                        dir.create(file.path(path_to_raw_qPCR, "Archives")), 
                        FALSE))

    # TODO: Here use a specific format lookup such as "\\.xls$", because csv will pickup the file even if we have file.csv.txt
    files_list <- list.files(path_to_raw_qPCR, pattern = "xls")
    for(file in files_list) {
        file_name <- paste(path_to_raw_qPCR, file, sep = "/")
        file.copy(from = file_name, 
                    to = paste0(file.path(path_to_raw_qPCR, "Archives","/"),
                                file
                    ))
        file.remove(from = file_name)
    }

    if (length(files_list) == 0) {
        message("[microbs Report]: No XLS files found to archive in ", path_to_raw_qPCR)
        return(invisible(character(0)))
    }

    invisible(files_list)
}





#--------------------------------------------------------------------------------------------------------
# Load new virus data
#--------------------------------------------------------------------------------------------------------
# In this section load the necessary data for a given 







#--------------------------------------------------------------------------------------------------------
# Load the old check data ddPCR
#--------------------------------------------------------------------------------------------------------
#' @title Load the check data file 
#'
#' @description This function loads the old check data and stores for later usage.
#' This file contains the old checked ddPCR raw data. Columns will be added to this data later.
#'
#' @param path_to_check_data_ddPCR A string to describe the path to the checked excel ddPCR data `Check_data_ddPCR_*.xlxs`. 
#' 
#' @return A tibble with the containing the raw data
#' @examples
#' # Example usage
#' path_to_check_data_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_ckeck_data"
#' df_raw_ddPCR_data <- load_microbs_old_check_ddPCR_Data(path_to_check_data_ddPCR)
#' 
#' # If you want to use the default path
#' set_microbs_loaded_DataPath()
#' set_microbs_check_DataPath()
#' df_raw_ddPCR_data <- load_microbs_old_check_ddPCR_Data() # use default path
#'
#' @export
load_microbs_old_check_ddPCR_Data <- function(path_to_check_data_ddPCR = .microbs_env$checkData_path) {
    # load the path
    if ( missing(path_to_check_data_ddPCR) ) {
        path_to_check_data_ddPCR <- get_microbs_check_DataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_check_data_ddPCR)
    }

    # Validate if the provided path exists
    if (!dir.exists(path_to_check_data_ddPCR)) {
        stop("[microbs Error]: The provided path does not exist: ", path_to_check_data_ddPCR)
    }

    # Check if Archives exist, if not create a directory
    ifelse(!dir.exists(file.path(path_to_check_data_ddPCR, "Archives")), 
            dir.create(file.path(path_to_check_data_ddPCR, "Archives")), FALSE)

    # load the names of all files and folders in the given directory path
    file_info <- utils::fileSnapshot(path_to_check_data_ddPCR)$info
    # remove all the directories from the list
    file_info <- subset(file_info, file_info$isdir == FALSE)
    # only keep the file names containg "Check_data_ddPCR_"
    file_info <- subset(file_info,grepl("Check_data_ddPCR_",rownames(file_info)))
    
    # Check if we have atleast one file
    if (nrow(file_info) == 0) {
        stop("[microbs Error]: No matching files found in directory: ", path_to_check_data_ddPCR)
    }

    # re-order the files according to time
    file_info <- file_info[order(file_info$mtime,decreasing = TRUE),]
    # Loop through files to select the latest Excel file
    latest_file <- NULL
    for (file_name in rownames(file_info)) {
        latest_file <- rownames(file_info)[which.max(file_info$mtime)]
        if (grepl("\\.xlsx$|\\.xls$", file_name, ignore.case = TRUE)) { # Ensure it's an Excel file
            latest_file <- file_name
        }
    }
    message("[microbs Report]: Load the latest Check_data_ddPCR_* file: ", latest_file)

    # create the file path
    latest_excel_file_path <- file.path(path_to_check_data_ddPCR, latest_file)

    # Load the latest excel file for ddPCR
    .microbs_env$df_old_check_ddPCR_data <- readxl::read_excel(latest_excel_file_path)

    # Return both the loaded dataframe and the latest file name
    .microbs_env$df_old_check_ddPCR_data
}


#--------------------------------------------------------------------------------------------------------
# Move old check ddPCR to archive
#--------------------------------------------------------------------------------------------------------
#' @title move check ddPCR data to archive
#'
#' @description The old check data needs to be moved to the archive once a new check is performed.
#'
#' @param path_to_check_data_ddPCR A string to describe the path to the check data. 
#' 
#' @return A character vector of archived file names (invisibly).
#' @examples
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_check_data_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_ckeck_data"
#' df_raw_ddPCR_data <- archive_microbs_check_ddPCR_Data(path_to_check_data_ddPCR)
#' @export
archive_microbs_check_ddPCR_Data <- function(path_to_check_data_ddPCR = .microbs_env$checkData_path) {
    # load the data 
    if (missing(path_to_check_data_ddPCR)) {
        path_to_check_data_ddPCR = get_microbs_ddPCR_rawDataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_check_data_ddPCR)
    }

    # load all the names of the check files files
    invisible(ifelse(!dir.exists(file.path(path_to_check_data_ddPCR, "Archives")), 
                        dir.create(file.path(path_to_check_data_ddPCR, "Archives")), 
                        FALSE))

    file_info <- utils::fileSnapshot(path_to_check_data_ddPCR)$info
    file_info <- subset(file_info, file_info$isdir == FALSE)
    file_info <- subset(file_info,grepl("Check_data_ddPCR_",rownames(file_info)))

    file_info <- file_info[order(file_info$mtime,decreasing = TRUE),]

    latest_file <- NULL
    for (file_name in rownames(file_info)) {
        latest_file <- rownames(file_info)[which.max(file_info$mtime)]
        if (grepl("\\.xlsx$|\\.xls$", file_name, ignore.case = TRUE)) { # Ensure it's an Excel file
            latest_file <- file_name
        }
    }

    file_info <- file_info[rownames(file_info) != latest_file, ]

    # TODO: Here use a specific format lookup such as "\\.csv$", because csv will pickup the file even if we have file.csv.txt
    files_list <- list.files(path_to_check_data_ddPCR, pattern = "Check_data_ddPCR_")
    # for(file in files_list) {
    for(file in rownames(file_info)) {
        file_path <- paste(path_to_check_data_ddPCR, file, sep = "/")

        if (file == latest_file) {
            next  # Skip the latest file
        } else {
            archive_path <- file.path(path_to_check_data_ddPCR, "Archives", file)
            file.copy(from = file_path, to = archive_path)
            file.remove(file_path)

            # Add archived file name to list
            files_list <- c(files_list, file)
        }
    }

    if (length(files_list) == 0) {
        message("[microbs Report]: No Check_data_ddPCR_ files found to archive in ", path_to_check_data_ddPCR)
        return(invisible(character(0)))
    }

    invisible(files_list)
}


#--------------------------------------------------------------------------------------------------------
# Load the old check data qPCR
#--------------------------------------------------------------------------------------------------------
#' @title Load the check data file 
#'
#' @description This function loads the old check data and stores for later usage.
#' This file contains the old checked qPCR raw data. Columns will be added to this data later.
#'
#' @param path_to_check_data_qPCR A string to describe the path to the checked excel qPCR data `Check_data_qPCR_*.xlxs`. 
#' 
#' @return A tibble with the containing the raw data
#' @examples
#' # Example usage
#' path_to_check_data_qPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_ckeck_data"
#' df_raw_qPCR_data <- load_microbs_old_check_qPCR_Data(path_to_check_data_qPCR)
#' 
#' # If you want to use the default path
#' set_microbs_loaded_DataPath()
#' set_microbs_check_DataPath()
#' df_raw_qPCR_data <- load_microbs_old_check_qPCR_Data() # use default path
#'
#' @export
load_microbs_old_check_qPCR_Data <- function(path_to_check_data_qPCR = .microbs_env$checkData_path) {
    # load the path
    if ( missing(path_to_check_data_qPCR) ) {
        path_to_check_data_qPCR <- get_microbs_check_DataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_check_data_qPCR)
    }

    # Validate if the provided path exists
    if (!dir.exists(path_to_check_data_qPCR)) {
        stop("[microbs Error]: The provided path does not exist: ", path_to_check_data_qPCR)
    }

    # Check if Archives exist, if not create a directory
    ifelse(!dir.exists(file.path(path_to_check_data_qPCR, "Archives")), 
            dir.create(file.path(path_to_check_data_qPCR, "Archives")), FALSE)

    # load the names of all files and folders in the given directory path
    file_info <- utils::fileSnapshot(path_to_check_data_qPCR)$info
    # remove all the directories from the list
    file_info <- subset(file_info, file_info$isdir == FALSE)
    # only keep the file names containg "Check_data_qPCR_"
    file_info <- subset(file_info,grepl("Check_data_qPCR_",rownames(file_info)))
    
    # Check if we have atleast one file
    if (nrow(file_info) == 0) {
        stop("[microbs Error]: No matching files found in directory: ", path_to_check_data_qPCR)
    }

    # re-order the files according to time
    file_info <- file_info[order(file_info$mtime,decreasing = TRUE),]
    # Loop through files to select the latest Excel file
    latest_file <- NULL
    for (file_name in rownames(file_info)) {
        latest_file <- rownames(file_info)[which.max(file_info$mtime)]
        if (grepl("\\.xlsx$|\\.xls$", file_name, ignore.case = TRUE)) { # Ensure it's an Excel file
            latest_file <- file_name
        }
    }
    message("[microbs Report]: Load the latest Check_data_qPCR_* file: ", latest_file)

    # create the file path
    latest_excel_file_path <- file.path(path_to_check_data_qPCR, latest_file)

    # Load the latest excel file for qPCR
    .microbs_env$df_old_check_qPCR_data <- readxl::read_excel(latest_excel_file_path)

    # Return both the loaded dataframe and the latest file name
    .microbs_env$df_old_check_qPCR_data
}


#--------------------------------------------------------------------------------------------------------
# Move old check qPCR to archive
#--------------------------------------------------------------------------------------------------------
#' @title move check qPCR data to archive
#'
#' @description The old check data needs to be moved to the archive once a new check is performed.
#'
#' @param path_to_check_data_qPCR A string to describe the path to the check data. 
#' 
#' @return A character vector of archived file names (invisibly).
#' @examples
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_check_data_qPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_ckeck_data"
#' df_raw_qPCR_data <- archive_microbs_check_qPCR_Data(path_to_check_data_qPCR)
#' @export
archive_microbs_check_qPCR_Data <- function(path_to_check_data_qPCR = .microbs_env$checkData_path) {
    # load the data 
    if (missing(path_to_check_data_qPCR)) {
        path_to_check_data_qPCR = get_microbs_qPCR_rawDataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_check_data_qPCR)
    }

    # load all the names of the CSV files
    invisible(ifelse(!dir.exists(file.path(path_to_check_data_qPCR, "Archives")), 
                        dir.create(file.path(path_to_check_data_qPCR, "Archives")), 
                        FALSE))

    file_info <- utils::fileSnapshot(path_to_check_data_qPCR)$info
    file_info <- subset(file_info, file_info$isdir == FALSE)
    file_info <- subset(file_info,grepl("Check_data_qPCR_",rownames(file_info)))

    file_info <- file_info[order(file_info$mtime,decreasing = TRUE),]

    latest_file <- NULL
    for (file_name in rownames(file_info)) {
        latest_file <- rownames(file_info)[which.max(file_info$mtime)]
        if (grepl("\\.xlsx$|\\.xls$", file_name, ignore.case = TRUE)) { # Ensure it's an Excel file
            latest_file <- file_name
        }
    }

    file_info <- file_info[rownames(file_info) != latest_file, ]

    # TODO: Here use a specific format lookup such as "\\.csv$", because csv will pickup the file even if we have file.csv.txt
    files_list <- list.files(path_to_check_data_qPCR, pattern = "Check_data_qPCR_")
    # for(file in files_list) {
    for(file in rownames(file_info)) {
        file_path <- paste(path_to_check_data_qPCR, file, sep = "/")

        if (file == latest_file) {
            next  # Skip the latest file
        } else {
            archive_path <- file.path(path_to_check_data_qPCR, "Archives", file)
            file.copy(from = file_path, to = archive_path)
            file.remove(file_path)

            # Add archived file name to list
            files_list <- c(files_list, file)
        }
    }

    if (length(files_list) == 0) {
        message("[microbs Report]: No Check_data_qPCR_ files found to archive in ", path_to_check_data_qPCR)
        return(invisible(character(0)))
    }

    invisible(files_list)
}
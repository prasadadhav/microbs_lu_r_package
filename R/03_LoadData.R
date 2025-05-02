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
#' @param path_to_flu_ddPCR A string to describe the path to the RAW excel ddPCR data `SUPERVIR_RAW_DATA_ddPCR_*.xlxs`. 
#' 
#' @return A tibble with the containing the raw data
#' @examples
#' # Example usage
#' path_to_old_raw_excel_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"
#' df_flu_raw_ddPCR_data <- load_microbs_old_raw_ddPCR_Data(path_to_old_raw_excel_ddPCR)
#' 
#' # If you want to use the default path
#' set_microbs_loaded_DataPath()
#' df_flu_raw_ddPCR_data <- load_microbs_old_raw_ddPCR_Data() # use default path
#' ddPCR_df <- df_flu_raw_ddPCR_data$data
#' ddPCR_latest_file <- df_flu_raw_ddPCR_data$latest_ddPCR_file
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
# Load the SUPERVIR_RAW_DATA_qPCR_*
#--------------------------------------------------------------------------------------------------------
#' @title Load the RAW data qPCR if it exists (or create one if absent)
#'
#' @description This function loads the old qPCR data and stores for later usage.
#' This file contains the old qPCR raw data. The new loaded data will be added to this data.
#'
#' @param path_to_flu_qPCR A string to describe the path to the RAW excel qPCR data `SUPERVIR_RAW_DATA_qPCR_*.xlxs`. 
#' 
#' @return A tibble with the containing the raw data
#' @examples
#' # Example usage
#' path_to_old_raw_excel_qPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"
#' df_flu_raw_qPCR_data <- load_microbs_old_raw_qPCR_Data(path_to_old_raw_excel_qPCR)
#' 
#' # If you want to use the default path
#' set_microbs_loaded_DataPath()
#' df_flu_raw_qPCR_data <- load_microbs_old_raw_qPCR_Data() # use default path
#' qPCR_df <- df_flu_raw_qPCR_data$data
#' qPCR_latest_file <- df_flu_raw_qPCR_data$latest_qPCR_file
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
# Load new Raw Flu AB
#--------------------------------------------------------------------------------------------------------
#' @title Load Flu A,B data
#'
#' @description Usually the tests for Flu A and Flu b are done together.
#' Hence the raw data is available in a single file.
#'
#' @param path_to_flu_ddPCR A string to describe the path to the RAW Flu (A/B) ddPCR data. 
#' 
#' @return A tibble with the containing the raw data
#' @examples
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_old_raw_excel_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"
#' set_microbs_loaded_DataPath(path_to_old_raw_excel_ddPCR)
#' load_microbs_old_raw_ddPCR_Data(path_to_old_raw_excel_ddPCR)
#' path_to_flu_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_ddPCR"
#' df_flu_raw_ddPCR_data <- load_microbs_flu_raw_ddPCR_Data(path_to_flu_ddPCR)
#' @export
load_microbs_flu_raw_ddPCR_Data <- function(path_to_flu_ddPCR = .microbs_env$ddPCR_raw_path) {
    # load the data 
    if (missing(path_to_flu_ddPCR)) {
        # path_to_flu_ddPCR <- get_microbs_ddPCR_rawDataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_flu_ddPCR)
    }

    # get the old data
    df_old_raw_ddPCR_data <- get_microbs_old_raw_ddPCR_Data()
    df_new_raw_ddPCR_data <- df_old_raw_ddPCR_data

    # load all the names of the CSV files
    files_list <- list.files(path_to_flu_ddPCR, pattern = "csv")
    for(file in files_list){
        file_name <- paste(path_to_flu_ddPCR, file, sep = "/")
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
                    stats::filter(!('Sample' %in% patterns))

        file <- subset(file, !grepl("No Call", copies_uL))

        for(j in 1 : nrow(file)){
            file <- file %>%
                dplyr::mutate(dilution = case_when(
                    grepl("D$", Sample) ~ 2,      # Si le nom se termine par "D", valeur = 2
                    grepl("T$", Sample) ~ 3,      # Si le nom se termine par "T", valeur = 3
                    grepl("d$", Sample) ~ 10,
                    TRUE ~ 0                    # Sinon, valeur = 0
                ))

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
    writexl::write_xlsx(df_new_raw_ddPCR_data, 
                paste0(path_to_old_raw_excel_ddPCR,"/","SUPERVIR_RAW_DATA_ddPCR_", # build the path
                        gsub(':','-',sub(' CEST','',Sys.time())), # substitute ":" with "-"
                        ".xlsx"))   # save as excel

    .microbs_env$df_new_raw_ddPCR_data <- df_new_raw_ddPCR_data

    return(.microbs_env$df_new_raw_ddPCR_data)
}


#--------------------------------------------------------------------------------------------------------
# Move new Raw Flu AB to archive
#--------------------------------------------------------------------------------------------------------
#' @title move Flu A,B data
#'
#' @description The processed raw data needs to be moved to the archive once it is loaded.
#'
#' @param path_to_flu_ddPCR A string to describe the path to the RAW Flu (A/B) ddPCR data. 
#' 
#' @return A tibble with the containing the raw data
#' @examples
#' # Example usage
#' path_to_flu_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_ddPCR"
#' df_flu_raw_ddPCR_data <- archive_microbs_flu_raw_ddPCR_Data(path_to_flu_ddPCR)
#' @export
archive_microbs_flu_raw_ddPCR_Data <- function(path_to_flu_ddPCR = .microbs_env$ddPCR_raw_path) {
    # load the data 
    if (missing(path_to_flu_ddPCR)) {
        path_to_flu_ddPCR = get_microbs_ddPCR_rawDataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_flu_ddPCR)
    }

    # load all the names of the CSV files
    invisible(ifelse(!dir.exists(file.path(path_to_flu_ddPCR, "Archives")), 
                        dir.create(file.path(path_to_flu_ddPCR, "Archives")), 
                        FALSE))

    files_list <- list.files(path_to_flu_ddPCR, pattern = "csv")
    for(file in files_list) {
        file_name <- paste(path_to_flu_ddPCR, file, sep = "/")
        file.copy(from = file_name, 
                    to = paste0(file.path(path_to_flu_ddPCR, "Archives","/"),
                                file
                    ))
        file.remove(from = file_name)
    }
}





#--------------------------------------------------------------------------------------------------------
# Load hRSV
#--------------------------------------------------------------------------------------------------------










#--------------------------------------------------------------------------------------------------------
# Load Sars-CoV 2
#--------------------------------------------------------------------------------------------------------











#--------------------------------------------------------------------------------------------------------
# Load new virus data
#--------------------------------------------------------------------------------------------------------
# In this section load the necessary data for a given 






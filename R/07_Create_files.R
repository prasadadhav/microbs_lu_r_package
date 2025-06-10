#--------------------------------------------------------------------------------------------------------
# Do the Creation file step for Flu A/B
#--------------------------------------------------------------------------------------------------------
#' @title Do the Creation file for Flu A/B
#' 
#' @description The calculations step computes the RNA copies for inhabitants
#' and per 100.000 people. Now this file needs to be reformated and exported
#' for each virus. In this funcion we handle flu A/B virus data.
#' 
#' 
#' file ./microbs.lu/R/07_Create_files.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' 
#' 
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' set_microbs_connector_dir("Data_Treatment")
#' 
#' path_to_raw_ddPCR <- "0_raw_data_ddPCR"
#' set_microbs_ddPCR_rawDataPath(path_to_raw_ddPCR)
#' 
#' path_to_old_raw_excel_ddPCR <- "1_loaded_data"
#' set_microbs_loaded_DataPath(path_to_old_raw_excel_ddPCR, , build_path = TRUE)
#' 
#' df_raw_old_ddPCR_data <- load_microbs_old_raw_ddPCR_Data()
#' df_raw_ddPCR_data <- load_microbs_raw_ddPCR_Data()
#' 
#' load_microbs_old_raw_ddPCR_Data()
#' 
#' path_to_flux_data <- "00_flux_data"
#' set_microbs_flux_DataPath(path_to_flux_data)
#' load_microbs_flux_Data()
#' 
#' set_microbs_calc_DataPath("2_calc_data")
#' set_microbs_created_DataPath("3_created_data")
#' 
#' calculations_microbs_ddPCR()
#' create_microbs_flu_file()
#' }
#' 
#' @export 
create_microbs_flu_file <- function(path_to_create_data_ddPCR = .microbs_env$created_data_path) {
    if (is.null(path_to_loaded_raw_excel_ddPCR = .microbs_env$created_data_path)) {
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_loaded_DataPath(),")
        message("and then load_microbs_old_raw_ddPCR_Data() function to set a path")
    }

    path_to_create_data_ddPCR <- get_microbs_created_DataPath()

    # Load the latest calc indicator file
    df_new_calc_ddPCR_data <- get_microbs_new_calc_ddPCR_Data()
    if (is.null(df_new_calc_ddPCR_data) | is.null(.microbs_env$df_new_calc_ddPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_new_calc_ddPCR_Data() function to set a path")
        message("or use the default by using load_microbs_new_calc_ddPCR_Data()")
    }

    #  fluA
    df_new_calc_ddPCR_data_fluA <- subset(df_new_calc_ddPCR_data, Target_Name %in% c("FluA"))

    df_new_calc_ddPCR_data_fluA <- df_new_calc_ddPCR_data_fluA %>% dplyr::select(Sample,week_nb,copies_day_mean,inhab,copies_inhab_mean)

    names(df_new_calc_ddPCR_data_fluA)[names(df_new_calc_ddPCR_data_fluA) == "copies_day_mean"] <- "copies_day_ddPCR_FluA"
    names(df_new_calc_ddPCR_data_fluA)[names(df_new_calc_ddPCR_data_fluA) == "sign_mean"] <- "CT_sign_ddPCR_FluA"
    names(df_new_calc_ddPCR_data_fluA)[names(df_new_calc_ddPCR_data_fluA) == "copies_inhab_mean"] <- "copies_inhab_ddPCR_FluA"

    df_new_calc_ddPCR_data_fluA$WWTP <- utils_extract_WWTP(df_new_calc_ddPCR_data_fluA$Sample)

    # fluB
    df_new_calc_ddPCR_data_fluB <- subset(df_new_calc_ddPCR_data, Target_Name %in% c("FluB"))
    
    df_new_calc_ddPCR_data_fluB <- df_new_calc_ddPCR_data_fluB %>% dplyr::select(Sample, week_nb, copies_day_mean, inhab, copies_inhab_mean)

    names(df_new_calc_ddPCR_data_fluB)[names(df_new_calc_ddPCR_data_fluB) == "copies_day_mean"] <- "copies_day_ddPCR_FluB"
    names(df_new_calc_ddPCR_data_fluB)[names(df_new_calc_ddPCR_data_fluB) == "sign_mean"] <- "CT_sign_ddPCR_FluB"
    names(df_new_calc_ddPCR_data_fluB)[names(df_new_calc_ddPCR_data_fluB) == "copies_inhab_mean"] <- "copies_inhab_ddPCR_FluB"

    sheet_1_data <- dplyr::full_join(df_new_calc_ddPCR_data_fluA, df_new_calc_ddPCR_data_fluB, 
                            by = dplyr::join_by(Sample, week_nb, inhab),
                            relationship = "many-to-many")

    max_time <- pmax(as.Date(max(lubridate::parse_date_time(paste(as.integer(stringr::str_sub(sheet_1_data$week_nb, 1, 4)),
                                                                as.integer(stringr::str_sub(sheet_1_data$week_nb, 6, 8)), 
                                                                1, sep="/"),
                                                                'Y/W/w'),
                                                                na.rm = TRUE)),
                                                                na.rm = TRUE)

    weeks <- data.frame(week_nb = generate_weeks(2024, 2025))
    weeks <- weeks[weeks$week_nb >= "2020_14" & weeks$week_nb <= format(max_time,"%Y_%V"),]

    #----------------------
    # Sheet 1: Aggregate the data for all weekly per WWTP and nationwide.
    #----------------------
    # flu A
    mean_WWTP_week_FluA_ddPCR <- aggregate(sheet_1_data$copies_day_ddPCR_FluA,
                                        by = list(week_nb = sheet_1_data$week_nb, WWTP = sheet_1_data$WWTP),
                                        FUN = function(x) if (all(is.na(x))) 0 else mean(x, na.rm = TRUE))

    colnames(mean_WWTP_week_FluA_ddPCR)[3] <- "mean_WWTP_week"

    aggregate_ddPCR_FluA_num <- aggregate(mean_WWTP_week_FluA_ddPCR$mean_WWTP_week,
                                        by = list(week_nb = mean_WWTP_week_FluA_ddPCR$week_nb),
                                        FUN = function(x) sum(x[x != 0], na.rm = TRUE))

    aggregate_ddPCR_FluA_num <- aggregate_ddPCR_FluA_num %>% dplyr::rename(copies_day_sum_ddPCR_FluA := x)
    den_ddPCR <- sheet_1_data %>% dplyr::select(WWTP,week_nb,inhab) %>% dplyr::distinct()
    aggregate_ddPCR_den <- aggregate(den_ddPCR$inhab, by = list(den_ddPCR$week_nb),
                                    FUN = function(x) if(all(is.na(x))) 0 else sum(x, na.rm = TRUE))

    aggregate_ddPCR_den <- aggregate_ddPCR_den %>% dplyr::rename(inhab_sum= x, week_nb = Group.1)
    aggregate_ddPCR_FluA <- plyr::join(aggregate_ddPCR_FluA_num, aggregate_ddPCR_den, by = 'week_nb')
    aggregate_ddPCR_FluA$copies_days_inhab_ddPCR_FluA <- aggregate_ddPCR_FluA$copies_day_sum_ddPCR_FluA / aggregate_ddPCR_FluA$inhab_sum * 100000
    
    # flu b
    mean_WWTP_week_FluB_ddPCR <- aggregate(sheet_1_data$copies_day_ddPCR_FluB,
                                       by = list(week_nb = sheet_1_data$week_nb, WWTP = sheet_1_data$WWTP),
                                       FUN = function(x) if (all(is.na(x))) 0 else mean(x, na.rm = TRUE))

    colnames(mean_WWTP_week_FluB_ddPCR)[3] <- "mean_WWTP_week"

    aggregate_ddPCR_FluB_num <- aggregate(mean_WWTP_week_FluB_ddPCR$mean_WWTP_week,
                                        by = list(week_nb = mean_WWTP_week_FluB_ddPCR$week_nb),
                                        FUN = function(x) sum(x[x != 0], na.rm = TRUE))

    aggregate_ddPCR_FluB_num <- aggregate_ddPCR_FluB_num %>% dplyr::rename(copies_day_sum_ddPCR_FluB := x)
    aggregate_ddPCR_FluB <- plyr::join(aggregate_ddPCR_FluB_num,aggregate_ddPCR_den,by = 'week_nb')
    aggregate_ddPCR_FluB$copies_days_inhab_ddPCR_FluB <- aggregate_ddPCR_FluB$copies_day_sum_ddPCR_FluB / aggregate_ddPCR_FluB$inhab_sum * 100000

    ddPCR <- dplyr::full_join(aggregate_ddPCR_FluA, aggregate_ddPCR_FluB, by = 'week_nb')
    # ddPCR <- subset(ddPCR, select = -c(inhab_sum.x, inhab_sum.y))

    sheet1_flu <- ddPCR
    sheet1_flu <- dplyr::left_join(expand.grid(week_nb = weeks), ddPCR, by = "week_nb")
    sheet1_flu <- sheet1_flu %>% dplyr::arrange(week_nb) 

    #----------------------
    # Sheet 2: Weekly flu concentrations per WWTP
    #----------------------
    # flu a
    aggregate_FluA_WWTP_ddPCR <- aggregate(sheet_1_data$copies_day_ddPCR_FluA, by = list(sheet_1_data$WWTP,sheet_1_data$week_nb),
                                        FUN = function(x) if(all(is.na(x))) 0 else mean(x, na.rm = TRUE))

    aggregate_FluA_WWTP_ddPCR <- aggregate_FluA_WWTP_ddPCR %>% dplyr::rename(copies_ddPCR_FluA = x, WWTP = Group.1, week_nb = Group.2)
    aggregate_FluA_inhab_WWTP_ddPCR <- aggregate(sheet_1_data$copies_inhab_ddPCR_FluA, by = list(sheet_1_data$WWTP,sheet_1_data$week_nb),
                                                FUN = function(x) if(all(is.na(x))) 0 else mean(x, na.rm = TRUE))
    
    aggregate_FluA_inhab_WWTP_ddPCR <- aggregate_FluA_inhab_WWTP_ddPCR %>% dplyr::rename(copies_inhab_ddPCR_FluA = x, WWTP = Group.1, week_nb =  Group.2)
    aggregate_FluA_WWTP_ddPCR <- dplyr::full_join(aggregate_FluA_WWTP_ddPCR,aggregate_FluA_inhab_WWTP_ddPCR,by = c('WWTP', 'week_nb'))

    # flu b
    aggregate_FluB_WWTP_ddPCR <- aggregate(sheet_1_data$copies_day_ddPCR_FluB, by = list(sheet_1_data$WWTP,sheet_1_data$week_nb),
                                        FUN = function(x) if(all(is.na(x))) 0 else mean(x, na.rm = TRUE))

    aggregate_FluB_WWTP_ddPCR <- aggregate_FluB_WWTP_ddPCR %>% dplyr::rename(copies_ddPCR_FluB = x, WWTP = Group.1, week_nb = Group.2)
    aggregate_FluB_inhab_WWTP_ddPCR <- aggregate(sheet_1_data$copies_inhab_ddPCR_FluB, by = list(sheet_1_data$WWTP,sheet_1_data$week_nb),
                                                FUN = function(x) if(all(is.na(x))) 0 else mean(x, na.rm = TRUE))
    
    aggregate_FluB_inhab_WWTP_ddPCR <- aggregate_FluB_inhab_WWTP_ddPCR %>% dplyr::rename(copies_inhab_ddPCR_FluB = x, WWTP = Group.1, week_nb = Group.2)
    aggregate_FluB_WWTP_ddPCR <- dplyr::full_join(aggregate_FluB_WWTP_ddPCR,aggregate_FluB_inhab_WWTP_ddPCR,by = c('WWTP', 'week_nb'))

    ddPCR <- dplyr::full_join(aggregate_FluA_WWTP_ddPCR,aggregate_FluB_WWTP_ddPCR, by = c('WWTP', 'week_nb'))

    sheet2_flu <- expand.grid(week_nb = weeks, WWTP = c("SCH", "PET", "BEG", "BET", "BLE", "MER", "HES", "ECH", "UEB", "GRE", "VIE", "BOE", "WIL"))
    sheet2_flu <- dplyr::left_join(sheet2_flu, ddPCR, by = c('WWTP','week_nb'))
    sheet2_flu <- sheet2_flu %>% dplyr::distinct(WWTP, week_nb, .keep_all = TRUE)
    sheet2_flu <- sheet2_flu %>% dplyr::arrange(week_nb) #Redplyr::arrange the columns

    #----------------------
    # Sheet 3: Weekly positivity rates (percent of positive detections) for each virus (Sheet 3)
    #----------------------
    sheet_1_data$positive_ddPCR_FluA <- ((rowSums(sheet_1_data[, grepl("CT_sign_ddPCR_FluA",names(sheet_1_data))] == "positive", na.rm=T) > 0) * 1)
    sheet_1_data$positive_ddPCR_FluB <- ((rowSums(sheet_1_data[, grepl("CT_sign_ddPCR_FluB",names(sheet_1_data))] == "positive", na.rm=T) > 0) * 1)
    sheet_1_data$dummy <- 1
    sheet_2_data <- aggregate(sheet_1_data$dummy, by=list(sheet_1_data$week_nb), FUN = sum) #Nombre total d'échantillons
    sheet_2_data <- sheet_2_data %>% dplyr::rename(denominator = x, week_nb = Group.1)

    sheet_2_data_fluA <- aggregate(sheet_1_data$positive_ddPCR_FluA, by=list(sheet_1_data$week_nb),
                                FUN = function(x) sum(x, na.rm = TRUE)) #Nombre de positifs
    sheet_2_data_fluA <- sheet_2_data_fluA %>% dplyr::rename(numerator = x, week_nb = Group.1)
    sheet_2_data_tot_fluA <- dplyr::inner_join(sheet_2_data_fluA, sheet_2_data, by = dplyr::join_by(week_nb))
    sheet_2_data_tot_fluA$positive_rate_FluA_ddPCR <- sheet_2_data_tot_fluA$numerator / sheet_2_data_tot_fluA$denominator*100
    sheet_2_data_tot_fluA <- dplyr::select(sheet_2_data_tot_fluA, -c(numerator,denominator))

    sheet_2_data_fluB <- aggregate(sheet_1_data$positive_ddPCR_FluB, by = list(sheet_1_data$week_nb),
                                FUN = function(x) sum(x, na.rm = TRUE)) #Nombre de positifs
    sheet_2_data_fluB <- sheet_2_data_fluB %>% dplyr::rename(numerator = x, week_nb = Group.1)
    sheet_2_data_tot_fluB <- dplyr::inner_join(sheet_2_data_fluB, sheet_2_data, by = dplyr::join_by(week_nb))
    sheet_2_data_tot_fluB$positive_rate_FluB_ddPCR <- sheet_2_data_tot_fluB$numerator / sheet_2_data_tot_fluB$denominator*100
    sheet_2_data_tot_fluB <- dplyr::select(sheet_2_data_tot_fluB, -c(numerator,denominator))

    ddPCR <- dplyr::full_join(sheet_2_data_tot_fluA,sheet_2_data_tot_fluB, by = 'week_nb')

    sheet3_flu <- expand.grid(week_nb = weeks)
    sheet3_flu <- dplyr::left_join(sheet3_flu, ddPCR, by = 'week_nb')
    sheet3_flu <- sheet3_flu %>% dplyr::arrange(week_nb) #Redplyr::arrange the columns

    #----------------------
    # Sheet 4
    #----------------------
    sheet_2_data_fluA <- aggregate(sheet_1_data$positive_ddPCR_FluA, by=list(sheet_1_data$WWTP,sheet_1_data$week_nb),
                                FUN = function(x) if(all(is.na(x))) NA else sum(x, na.rm = TRUE)) #Nombre de positifs

    sheet_2_data_fluA <- sheet_2_data_fluA %>% dplyr::rename(numerator = x,WWTP=Group.1,week_nb=Group.2)

    sheet_2_data_fluB <- aggregate(sheet_1_data$positive_ddPCR_FluB, by=list(sheet_1_data$WWTP,sheet_1_data$week_nb),
                                FUN = function(x) if(all(is.na(x))) NA else sum(x, na.rm = TRUE)) #Nombre de positifs
    sheet_2_data_fluB <- sheet_2_data_fluB %>% dplyr::rename(numerator = x,WWTP=Group.1,week_nb=Group.2)

    test1_ddPCR <- aggregate(sheet_1_data$dummy, by = list(sheet_1_data$WWTP,sheet_1_data$week_nb), FUN=sum) #Nombre total d'échantillons
    test1_ddPCR <- test1_ddPCR %>% dplyr::rename(denominator = x,WWTP=Group.1,week_nb=Group.2)

    sheet_2_data_tot_fluA <- dplyr::inner_join(sheet_2_data_fluA,test1_ddPCR,by = dplyr::join_by(WWTP, week_nb))
    sheet_2_data_tot_fluA$positive_rate_FluA_ddPCR <- sheet_2_data_tot_fluA$numerator / sheet_2_data_tot_fluA$denominator*100
    sheet_2_data_tot_fluA <- dplyr::select(sheet_2_data_tot_fluA, -c(numerator,denominator))

    sheet_2_data_tot_fluB <- dplyr::inner_join(sheet_2_data_fluB,test1_ddPCR,by = dplyr::join_by(WWTP, week_nb))
    sheet_2_data_tot_fluB$positive_rate_FluB_ddPCR <- sheet_2_data_tot_fluB$numerator / sheet_2_data_tot_fluB$denominator*100
    sheet_2_data_tot_fluB <- dplyr::select(sheet_2_data_tot_fluB, -c(numerator,denominator))

    sheet4_flu <- expand.grid(week_nb = weeks, WWTP = c("SCH", "PET", "BEG", "BET", "BLE", "MER", "HES", "ECH", "UEB", "GRE", "VIE", "BOE", "WIL"))
    sheet4_flu <- dplyr::left_join(sheet4_flu, dplyr::full_join(sheet_2_data_tot_fluA,sheet_2_data_tot_fluB,
                                        c('WWTP', 'week_nb')), by = c('WWTP','week_nb'))
    sheet4_flu <- sheet4_flu %>% dplyr::arrange(week_nb) #Redplyr::arrange the columns

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "1.CT Aggregate data")
    openxlsx::addWorksheet(wb, "2.CT WWTP data")
    openxlsx::addWorksheet(wb, "3.Positivity aggregate")
    openxlsx::addWorksheet(wb, "4.Positivity WWTP")
    openxlsx::writeData(wb, "1.CT Aggregate data", sheet1_flu)
    openxlsx::writeData(wb, "2.CT WWTP data", sheet2_flu)
    openxlsx::writeData(wb, "3.Positivity aggregate", sheet3_flu)
    openxlsx::writeData(wb, "4.Positivity WWTP", sheet4_flu)

    openxlsx::freezePane(wb, sheet = "1.CT Aggregate data", firstRow = TRUE)
    openxlsx::freezePane(wb, sheet = "2.CT WWTP data", firstRow = TRUE)
    openxlsx::freezePane(wb, sheet = "3.Positivity aggregate", firstRow = TRUE)
    openxlsx::freezePane(wb, sheet = "4.Positivity WWTP", firstRow = TRUE)

    xlxs_filename <- paste0(path_to_create_data_ddPCR, "/SUPERVIR_WW_FLU_AGG_",
                   gsub(":", "-", sub(" CEST", "", Sys.time())),
                   ".xlsx")
    openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)

    .microbs_env$sheet1_flu <- sheet1_flu
    .microbs_env$sheet2_flu <- sheet2_flu
    .microbs_env$sheet3_flu <- sheet3_flu
    .microbs_env$sheet4_flu <- sheet4_flu
}


#--------------------------------------------------------------------------------------------------------
# Move old check created flu to archive
#--------------------------------------------------------------------------------------------------------
#' @title move check ddPCR data to archive
#'
#' @description The old check data needs to be moved to the archive once a new check is performed.
#'
#' @param path_to_create_data_ddPCR A string to describe the path to the created data. 
#' 
#' @return A character vector of archived file names (invisibly).
#' @examples
#' \dontrun{
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_create_data_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/3_created_data"
#' df_raw_ddPCR_data <- archive_microbs_created_flu_Data(path_to_create_data_ddPCR)
#' }
#' @export
archive_microbs_created_flu_Data <- function(path_to_create_data_ddPCR = .microbs_env$created_data_path) {
    # load the data 
    if (missing(path_to_create_data_ddPCR)) {
        path_to_create_data_ddPCR = get_microbs_ddPCR_rawDataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_create_data_ddPCR)
    }

    # load all the names of the check files files
    invisible(ifelse(!dir.exists(file.path(path_to_create_data_ddPCR, "Archives")), 
                        dir.create(file.path(path_to_create_data_ddPCR, "Archives")), 
                        FALSE))

    file_info <- utils::fileSnapshot(path_to_create_data_ddPCR)$info
    file_info <- subset(file_info, file_info$isdir == FALSE)
    file_info <- subset(file_info, grepl("SUPERVIR_WW_FLU_AGG_",rownames(file_info)))

    file_info <- file_info[order(file_info$mtime,decreasing = TRUE),]

    latest_file <- NULL
    for (file_name in rownames(file_info)) {
        latest_file <- rownames(file_info)[which.max(file_info$mtime)]
        if (grepl("\\.xlsx$ | \\.xls$", file_name, ignore.case = TRUE)) { # Ensure it's an Excel file
            latest_file <- file_name
        }
    }

    file_info <- file_info[rownames(file_info) != latest_file, ]

    # TODO: Here use a specific format lookup such as "\\.csv$", because csv will pickup the file even if we have file.csv.txt
    files_list <- list.files(path_to_create_data_ddPCR, pattern = "SUPERVIR_WW_FLU_AGG_")
    # for(file in files_list) {
    for(file in rownames(file_info)) {
        file_path <- paste(path_to_create_data_ddPCR, file, sep = "/")

        if (file == latest_file) {
            next  # Skip the latest file
        } else {
            archive_path <- file.path(path_to_create_data_ddPCR, "Archives", file)
            file.copy(from = file_path, to = archive_path)
            file.remove(file_path)

            # Add archived file name to list
            files_list <- c(files_list, file)
        }
    }

    if (length(files_list) == 0) {
        message("[microbs Report]: No SUPERVIR_WW_FLU_AGG_ files found to archive in ", path_to_create_data_ddPCR)
        return(invisible(character(0)))
    }

    invisible(files_list)
}


#--------------------------------------------------------------------------------------------------------
# Do the Creation file step for RSV ddPCR
#--------------------------------------------------------------------------------------------------------
#' @title Do the Creation file for RSV
#' 
#' @description The calculations step computes the RNA copies for inhabitants
#' and per 100.000 people. Now this file needs to be reformated and exported
#' for each virus. In this funcion we handle RSV data.
#' 
#' 
#' file ./microbs.lu/R/07_Create_files.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' 
#' 
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' set_microbs_connector_dir("Data_Treatment")
#' 
#' path_to_raw_ddPCR <- "0_raw_data_ddPCR"
#' set_microbs_ddPCR_rawDataPath(path_to_raw_ddPCR)
#' 
#' path_to_old_raw_excel_ddPCR <- "1_loaded_data"
#' set_microbs_loaded_DataPath(path_to_old_raw_excel_ddPCR, , build_path = TRUE)
#' 
#' df_raw_old_ddPCR_data <- load_microbs_old_raw_ddPCR_Data()
#' df_raw_ddPCR_data <- load_microbs_raw_ddPCR_Data()
#' 
#' load_microbs_old_raw_ddPCR_Data()
#' 
#' path_to_flux_data <- "00_flux_data"
#' set_microbs_flux_DataPath(path_to_flux_data)
#' load_microbs_flux_Data()
#' 
#' set_microbs_created_DataPath("3_created_data")
#' 
#' calculations_microbs_ddPCR()
#' create_microbs_rsv_file()
#' }
#' 
#' @export 
create_microbs_rsv_file <- function(path_to_create_data_ddPCR = .microbs_env$created_data_path) {
    if (is.null(path_to_create_data_ddPCR = .microbs_env$created_data_path)) {
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_created_DataPath(),")
        message("and then load_microbs_old_raw_ddPCR_Data() function to set a path")
    }

    path_to_create_data_ddPCR <- get_microbs_created_DataPath()

    # Load the latest calc indicator file
    data_hRSV_ddPCR <- get_microbs_new_calc_ddPCR_Data()
    if (is.null(data_hRSV_ddPCR) | is.null(.microbs_env$df_new_calc_ddPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_new_calc_ddPCR_Data() function to set a path")
        message("or use the default by using load_microbs_new_calc_ddPCR_Data()")
    }

    names(data_hRSV_ddPCR)[names(data_hRSV_ddPCR) == "copies_day_mean"] <- "copies_day_ddPCR_hRSV"
    names(data_hRSV_ddPCR)[names(data_hRSV_ddPCR) == "sign_mean"] <- "CT_sign_ddPCR_hRSV"
    names(data_hRSV_ddPCR)[names(data_hRSV_ddPCR) == "copies_inhab_mean"] <- "copies_inhab_ddPCR_hRSV"

    data_hRSV_ddPCR$WWTP <- utils_extract_WWTP(data_hRSV_ddPCR$Sample)

    #----------------------
    # Sheet 1: Aggregate the data for all weekly per WWTP and nationwide.
    #----------------------
    mean_WWTP_week_hRSV_ddPCR <- aggregate(data_hRSV_ddPCR$copies_day_ddPCR_hRSV,
                                       by = list(week_nb = data_hRSV_ddPCR$week_nb, WWTP = data_hRSV_ddPCR$WWTP),
                                       FUN = function(x) if (all(is.na(x))) 0 else mean(x, na.rm = TRUE))
    
    colnames(mean_WWTP_week_hRSV_ddPCR)[3] <- "mean_WWTP_week"
    
    aggregate_ddPCR_hRSV_num <- aggregate(mean_WWTP_week_hRSV_ddPCR$mean_WWTP_week,
                                        by = list(week_nb = mean_WWTP_week_hRSV_ddPCR$week_nb),
                                        FUN = function(x) sum(x[x != 0], na.rm = TRUE))
    
    aggregate_ddPCR_hRSV_num <- aggregate_ddPCR_hRSV_num %>% dplyr::rename(copies_day_sum_ddPCR_hRSV := x)
    
    den_ddPCR <- data_hRSV_ddPCR %>% dplyr::select(WWTP,week_nb,inhab) %>% dplyr::distinct()
    
    aggregate_ddPCR_den <- aggregate(den_ddPCR$inhab, by=list(den_ddPCR$week_nb),
                                    FUN = function(x) if(all(is.na(x))) 0 else sum(x, na.rm = TRUE))
    
    aggregate_ddPCR_den <- aggregate_ddPCR_den %>% dplyr::rename(inhab_sum = x, week_nb = Group.1)
    aggregate_ddPCR_hRSV <- plyr::join(aggregate_ddPCR_hRSV_num,aggregate_ddPCR_den, by = 'week_nb')
    aggregate_ddPCR_hRSV$copies_days_inhab_ddPCR_hRSV <- aggregate_ddPCR_hRSV$copies_day_sum_ddPCR_hRSV / aggregate_ddPCR_hRSV$inhab_sum * 100000

    sheet1_hRSV <- aggregate_ddPCR_hRSV %>% dplyr::arrange(week_nb) 

    #----------------------
    # Sheet 2: Weekly flu concentrations per WWTP
    #----------------------
    aggregate_hRSV_WWTP_ddPCR <- aggregate(data_hRSV_ddPCR$copies_day_ddPCR_hRSV, by = list(data_hRSV_ddPCR$WWTP, data_hRSV_ddPCR$week_nb),
                                       FUN = function(x) if(all(is.na(x))) 0 else mean(x, na.rm = TRUE))
    
    aggregate_hRSV_WWTP_ddPCR <- aggregate_hRSV_WWTP_ddPCR %>% dplyr::rename(copies_ddPCR_hRSV = x, WWTP = Group.1, week_nb = Group.2)
    
    aggregate_hRSV_inhab_WWTP_ddPCR <- aggregate(data_hRSV_ddPCR$copies_inhab_ddPCR_hRSV, by=list(data_hRSV_ddPCR$WWTP,data_hRSV_ddPCR$week_nb),
                                                FUN = function(x) if(all(is.na(x))) 0 else mean(x, na.rm = TRUE))
    
    aggregate_hRSV_inhab_WWTP_ddPCR <- aggregate_hRSV_inhab_WWTP_ddPCR %>% dplyr::rename(copies_inhab_ddPCR_hRSV = x, WWTP = Group.1, week_nb = Group.2)
    
    aggregate_hRSV_WWTP_ddPCR <- dplyr::full_join(aggregate_hRSV_WWTP_ddPCR,aggregate_hRSV_inhab_WWTP_ddPCR,by = c('WWTP','week_nb'))

    sheet2_hRSV <- expand.grid(week_nb = aggregate_hRSV_WWTP_ddPCR$week_nb, 
                        WWTP = c("SCH", "PET", "BEG", "BET", "BLE", "MER", "HES", "ECH", "UEB", "GRE", "VIE", "BOE", "WIL"))
    sheet2_hRSV <- dplyr::left_join(sheet2_hRSV, aggregate_hRSV_WWTP_ddPCR, by = c('WWTP','week_nb'))
    sheet2_hRSV <- sheet2_hRSV %>% dplyr::distinct(WWTP, week_nb, .keep_all = TRUE)                    
    sheet2_hRSV <- sheet2_hRSV %>% dplyr::arrange(week_nb)

    #----------------------
    # Sheet 3: Weekly positivity rates (percent of positive detections) for each virus (Sheet 3)
    #----------------------
    data_hRSV_ddPCR$positive_ddPCR_hRSV <- ((rowSums(data_hRSV_ddPCR[, grepl("CT_sign_ddPCR_hRSV", names(data_hRSV_ddPCR))] == "positive", na.rm=T) > 0) * 1)
    data_hRSV_ddPCR$dummy <- 1
    test2 <- aggregate(data_hRSV_ddPCR$dummy, by = list(data_hRSV_ddPCR$week_nb), FUN = sum)
    test2 <- test2 %>% dplyr::rename(denominator = x, week_nb = Group.1)

    test_hRSV_ddPCR <- aggregate(data_hRSV_ddPCR$positive_ddPCR_hRSV, by=list(data_hRSV_ddPCR$week_nb),
                                FUN=function(x) sum(x, na.rm = TRUE)) 
    test_hRSV_ddPCR <- test_hRSV_ddPCR %>% dplyr::rename(numerator = x, week_nb = Group.1)
    test_tot_hRSV_ddPCR <- dplyr::inner_join(test_hRSV_ddPCR,test2, by = dplyr::join_by(week_nb))
    test_tot_hRSV_ddPCR$positive_rate_hRSV_ddPCR <- test_tot_hRSV_ddPCR$numerator / test_tot_hRSV_ddPCR$denominator*100
    test_tot_hRSV_ddPCR <- dplyr::select(test_tot_hRSV_ddPCR,-c(numerator, denominator))

    sheet3_hRSV <- expand.grid(week_nb = test_tot_hRSV_ddPCR$week_nb)
    sheet3_hRSV <- dplyr::left_join(sheet3_hRSV, test_tot_hRSV_ddPCR, by = 'week_nb')
    sheet3_hRSV <- sheet3_hRSV %>% dplyr::arrange(week_nb)

    #----------------------
    # Sheet 4
    #----------------------
    test_hRSV_ddPCR <- aggregate(data_hRSV_ddPCR$positive_ddPCR_hRSV, by=list(data_hRSV_ddPCR$WWTP,data_hRSV_ddPCR$week_nb),
                             FUN=function(x) if(all(is.na(x))) NA else sum(x, na.rm = TRUE)) #Nombre de positifs
    test_hRSV_ddPCR <- test_hRSV_ddPCR %>% dplyr::rename(numerator = x,WWTP=Group.1,week_nb=Group.2)

    test1_ddPCR <- aggregate(data_hRSV_ddPCR$dummy, by=list(data_hRSV_ddPCR$WWTP,data_hRSV_ddPCR$week_nb), FUN=sum) #Nombre total d'échantillons
    test1_ddPCR <- test1_ddPCR %>% dplyr::rename(denominator = x,WWTP=Group.1,week_nb=Group.2)

    test_tot_hRSV_ddPCR <- dplyr::inner_join(test_hRSV_ddPCR,test1_ddPCR,by = dplyr::join_by(WWTP, week_nb))
    test_tot_hRSV_ddPCR$positive_rate_hRSV_ddPCR <- test_tot_hRSV_ddPCR$numerator / test_tot_hRSV_ddPCR$denominator*100
    test_tot_hRSV_ddPCR <- dplyr::select(test_tot_hRSV_ddPCR, -c(numerator,denominator))

    sheet4_hRSV <- expand.grid(week_nb = test_tot_hRSV_ddPCR$week_nb, WWTP = c("SCH", "PET", "BEG", "BET", "BLE", "MER", "HES", "ECH", "UEB", "GRE", "VIE", "BOE", "WIL"))
    sheet4_hRSV <- dplyr::left_join(sheet4_hRSV, test_tot_hRSV_ddPCR, by = c('WWTP','week_nb'))
    sheet4_hRSV <- sheet4_hRSV %>% dplyr::arrange(week_nb) 

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "1.CT Aggregate data")
    openxlsx::addWorksheet(wb, "2.CT WWTP data")
    openxlsx::addWorksheet(wb, "3.Positivity aggregate")
    openxlsx::addWorksheet(wb, "4.Positivity WWTP")
    openxlsx::writeData(wb, "1.CT Aggregate data", sheet1_hRSV)
    openxlsx::writeData(wb, "2.CT WWTP data", sheet2_hRSV)
    openxlsx::writeData(wb, "3.Positivity aggregate", sheet3_hRSV)
    openxlsx::writeData(wb, "4.Positivity WWTP", sheet4_hRSV)

    openxlsx::freezePane(wb, sheet = "1.CT Aggregate data", firstRow = TRUE)
    openxlsx::freezePane(wb, sheet = "2.CT WWTP data", firstRow = TRUE)
    openxlsx::freezePane(wb, sheet = "3.Positivity aggregate", firstRow = TRUE)
    openxlsx::freezePane(wb, sheet = "4.Positivity WWTP", firstRow = TRUE)

    xlxs_filename <- paste0(path_to_create_data_ddPCR, "/SUPERVIR_WW_hRSV_AGG_",
                   gsub(":", "-", sub(" CEST", "", Sys.time())),
                   ".xlsx")
    openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)

    .microbs_env$sheet1_hRSV <- sheet1_hRSV
    .microbs_env$sheet2_hRSV <- sheet2_hRSV
    .microbs_env$sheet3_hRSV <- sheet3_hRSV
    .microbs_env$sheet4_hRSV <- sheet4_hRSV
}


#--------------------------------------------------------------------------------------------------------
# Move old check created RSV to archive
#--------------------------------------------------------------------------------------------------------
#' @title move check ddPCR data to archive
#'
#' @description The old check data needs to be moved to the archive once a new check is performed.
#'
#' @param path_to_create_data_ddPCR A string to describe the path to the created data. 
#' 
#' @return A character vector of archived file names (invisibly).
#' @examples
#' \dontrun{
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_create_data_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/3_created_data"
#' df_raw_ddPCR_data <- archive_microbs_created_rsv_Data(path_to_create_data_ddPCR)
#' }
#' @export
archive_microbs_created_rsv_Data <- function(path_to_create_data_ddPCR = .microbs_env$created_data_path) {
    # load the data 
    if (missing(path_to_create_data_ddPCR)) {
        path_to_create_data_ddPCR = get_microbs_ddPCR_rawDataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_create_data_ddPCR)
    }

    # load all the names of the check files files
    invisible(ifelse(!dir.exists(file.path(path_to_create_data_ddPCR, "Archives")), 
                        dir.create(file.path(path_to_create_data_ddPCR, "Archives")), 
                        FALSE))

    file_info <- utils::fileSnapshot(path_to_create_data_ddPCR)$info
    file_info <- subset(file_info, file_info$isdir == FALSE)
    file_info <- subset(file_info, grepl("SUPERVIR_WW_hRSV_AGG_", rownames(file_info)))

    file_info <- file_info[order(file_info$mtime,decreasing = TRUE),]

    latest_file <- NULL
    for (file_name in rownames(file_info)) {
        latest_file <- rownames(file_info)[which.max(file_info$mtime)]
        if (grepl("\\.xlsx$ | \\.xls$", file_name, ignore.case = TRUE)) { # Ensure it's an Excel file
            latest_file <- file_name
        }
    }

    file_info <- file_info[rownames(file_info) != latest_file, ]

    # TODO: Here use a specific format lookup such as "\\.csv$", because csv will pickup the file even if we have file.csv.txt
    files_list <- list.files(path_to_create_data_ddPCR, pattern = "SUPERVIR_WW_hRSV_AGG_")
    # for(file in files_list) {
    for(file in rownames(file_info)) {
        file_path <- paste(path_to_create_data_ddPCR, file, sep = "/")

        if (file == latest_file) {
            next  # Skip the latest file
        } else {
            archive_path <- file.path(path_to_create_data_ddPCR, "Archives", file)
            file.copy(from = file_path, to = archive_path)
            file.remove(file_path)

            # Add archived file name to list
            files_list <- c(files_list, file)
        }
    }

    if (length(files_list) == 0) {
        message("[microbs Report]: No SUPERVIR_WW_hRSV_AGG_ files found to archive in ", path_to_create_data_ddPCR)
        return(invisible(character(0)))
    }

    invisible(files_list)
}


#--------------------------------------------------------------------------------------------------------
# Do the Creation file step for SARS-CoV-2 qPCR
#--------------------------------------------------------------------------------------------------------
#' @title Do the Creation file for SARS-CoV-2
#' 
#' @description The calculations step computes the RNA copies for inhabitants
#' and per 100.000 people. Now this file needs to be reformated and exported
#' for each virus. In this funcion we handle SARS-CoV-2 data.
#' 
#' 
#' file ./microbs.lu/R/07_Create_files.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' 
#' 
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' set_microbs_connector_dir("Data_Treatment")
#' 
#' path_to_raw_qPCR <- "0_raw_data_qPCR"
#' set_microbs_qPCR_rawDataPath(path_to_raw_qPCR)
#' 
#' path_to_old_raw_excel_qPCR <- "1_loaded_data"
#' set_microbs_loaded_DataPath(path_to_old_raw_excel_qPCR, , build_path = TRUE)
#' 
#' df_raw_old_qPCR_data <- load_microbs_old_raw_qPCR_Data()
#' df_raw_qPCR_data <- load_microbs_raw_qPCR_Data()
#' 
#' load_microbs_old_raw_qPCR_Data()
#' 
#' path_to_flux_data <- "00_flux_data"
#' set_microbs_flux_DataPath(path_to_flux_data)
#' load_microbs_flux_Data()
#' 
#' set_microbs_created_DataPath("3_created_data")
#' 
#' calculations_microbs_qPCR()
#' create_microbs_sars_file()
#' }
#' 
#' @export 
create_microbs_sars_file <- function(path_to_create_data_qPCR = .microbs_env$created_data_path) {
    if (is.null(path_to_create_data_qPCR = .microbs_env$created_data_path)) {
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_created_DataPath(),")
        message("and then load_microbs_old_raw_ddPCR_Data() function to set a path")
    }

    path_to_create_data_qPCR <- get_microbs_created_DataPath()

    # Load the latest calc indicator file
    data_sars_qPCR <- get_microbs_new_calc_qPCR_Data()
    data_qPCR <- subset(data_sars_qPCR, Target_Name %in% c("SARS-CoV-2"))
    if (is.null(data_sars_qPCR) | is.null(.microbs_env$df_new_calc_qPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_calc_qPCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_calc_qPCR_Data()")
    }

    names(data_qPCR)[names(data_qPCR) == "Flow Rate (m3/day)"] <- "Flow_rate"
    names(data_qPCR)[names(data_qPCR) == "copies/L"] <- "mean_copies_L"
    names(data_qPCR)[names(data_qPCR) == "copies/day"] <- "mean_copies_day"

    data_qPCR$inhab <- dplyr::case_when(
        substr(data_qPCR$Sample, 1, 3) %in% "BEG" ~ 139731,
        substr(data_qPCR$Sample, 1, 3) %in% "BET" ~ 53606,
        substr(data_qPCR$Sample, 1, 3) %in% "PET" ~ 59481,
        substr(data_qPCR$Sample, 1, 3) %in% "SCH" ~ 68143,
        substr(data_qPCR$Sample, 1, 3) %in% "BLE" ~ 30930,
        substr(data_qPCR$Sample, 1, 3) %in% "MER" ~ 30473,
        substr(data_qPCR$Sample, 1, 3) %in% "HES" ~ 15479,
        substr(data_qPCR$Sample, 1, 3) %in% "ECH" ~ 7499,
        substr(data_qPCR$Sample, 1, 3) %in% "UEB" ~ 18600, 
        substr(data_qPCR$Sample, 1, 3) %in% "GRE" ~ 9835,
        substr(data_qPCR$Sample, 1, 3) %in% "VIE" ~ 3411,
        substr(data_qPCR$Sample, 1, 3) %in% "BOE" ~ 7818,
        substr(data_qPCR$Sample, 1, 3) %in% "WIL" ~ 6944,
    )

    data_qPCR$mean_copies_day_inhab = data_qPCR$mean_copies_day/data_qPCR$inhab * 100000
    data_qPCR$Sample_Date = as.Date(data_qPCR$Sample_Date) 
    
    data_qPCR <- data_qPCR %>% dplyr::select(Sample, Target_Name, Sample_Date, week_nb, inhab, everything())
    data_qPCR$WWTP <- utils_extract_WWTP(data_qPCR$Sample)

    max_time <- pmax(as.Date(max(as.Date(data_qPCR$Sample_Date), na.rm=TRUE)), na.rm = TRUE)

    copies_days_inhab <- "copies_days_inhab_qPCR"
    var_name_num_1 <- "copies_day_sum_qPCR"
    message("debug1")
    str(data_qPCR)

    #----------------------
    # Sheet 1
    #----------------------
    mean_WWTP_week <- aggregate(data_qPCR$mean_copies_day,
                                       by = list(week_nb = data_qPCR$week_nb, lieu = data_qPCR$WWTP),
                                       FUN = function(x) if (all(is.na(x))) 0 else mean(x, na.rm = TRUE))
    
    colnames(mean_WWTP_week)[3] <- "mean_WWTP_week"

    aggregate_copies_num <- aggregate(mean_WWTP_week$mean_WWTP_week,
                                      by = list(week_nb = mean_WWTP_week$week_nb),
                                      FUN = function(x) sum(x[x != 0], na.rm = TRUE))

    aggregate_copies_num <- aggregate_copies_num %>% dplyr::rename(!!var_name_num_1 := x)

    den <- data_qPCR %>% dplyr::select(WWTP, week_nb, inhab) %>% dplyr::distinct()

    aggregate_copies_den <- aggregate(den$inhab, by = list(den$week_nb), FUN = sum)
    aggregate_copies_den <- aggregate_copies_den %>% dplyr::rename(inhab_sum := x, week_nb = Group.1)
    aggregate_copies <- plyr::join(aggregate_copies_num, aggregate_copies_den, by = 'week_nb')
    aggregate_copies[copies_days_inhab] <- aggregate_copies[var_name_num_1]/aggregate_copies$inhab_sum * 100000    
    # aggregate_copies <- subset(aggregate_copies, select = -inhab_sum)

    sheet1 <- expand.grid(week_nb = aggregate_copies$week_nb)
    sheet1 <- dplyr::left_join(sheet1, aggregate_copies, by = "week_nb")
    sheet1 <- sheet1 %>% dplyr::filter(sheet1$week_nb != "NA")
    sheet1 <- sheet1 %>% dplyr::arrange(week_nb)
    sheet1_sars <- sheet1[sheet1$week_nb >= "2020_14" & sheet1$week_nb <= format(max_time,"%Y_%V"),]
    message("debug2")
    str(sheet1_sars)

    #----------------------
    # Sheet 2
    #----------------------
    name_file <- data_qPCR
    var_name_num_2 <- "copies_day_sum_qPCR"
    final <- "aggregate_copies_qPCR"

    aggregate_copies_num <- aggregate(name_file$mean_copies_day, 
                                        by = list(name_file$WWTP, name_file$week_nb),
                                        FUN = function(x) if(all(is.na(x))) 0 else mean(x, na.rm = TRUE))
    aggregate_copies_num <- aggregate_copies_num %>% dplyr::rename(!!var_name_num_2 := x, WWTP = Group.1, week_nb = Group.2)

    den <- name_file %>% dplyr::select(WWTP, week_nb, inhab) %>% dplyr::distinct()
    aggregate_copies_den <- aggregate(den$inhab, by = list(den$WWTP,den$week_nb),
                                      FUN = function(x) if(all(is.na(x))) 0 else mean(x, na.rm = TRUE))
    aggregate_copies_den <- aggregate_copies_den %>% dplyr::rename(inhab_sum := x, WWTP = Group.1, week_nb = Group.2)
    aggregate_copies <- plyr::join(aggregate_copies_num, aggregate_copies_den, by = c('WWTP','week_nb'))
    aggregate_copies[copies_days_inhab] <- aggregate_copies[var_name_num_2]/aggregate_copies$inhab_sum * 100000
    aggregate_copies <- subset(aggregate_copies, select = -inhab_sum)
    assign(final, aggregate_copies)

    sheet2 <- expand.grid(week_nb = aggregate_copies$week_nb, WWTP = c("SCH", "PET", "BEG", "BET", "BLE", "MER", "HES", "ECH", "UEB", "GRE", "VIE", "BOE", "WIL"))
    sheet2 <- dplyr::left_join(sheet2, aggregate_copies_qPCR, by = c('WWTP','week_nb'))
    sheet2 <- sheet2 %>% dplyr::filter(sheet2$week_nb != "NA")
    # sheet2 <- sheet2 %>% dplyr::select(week_nb, WWTP) %>% dplyr::distinct()
    sheet2 <- sheet2 %>% dplyr::distinct(week_nb, WWTP, .keep_all = TRUE)
    sheet2 <- sheet2 %>% dplyr::arrange(week_nb, WWTP)
    sheet2_sars <- sheet2[sheet2$week_nb >= "2020_14" & sheet2$week_nb <= format(max_time,"%Y_%V"),]
    message("debug3")
    str(sheet2_sars)
    
    #----------------------
    # Sheet 3
    #----------------------
    final_3 <- "positivity_rate_qPCR"
    var_name <- "positive_rate_qPCR"

    name_file$positive <- ((rowSums(name_file[, grepl("sign",names(name_file))] == "Positive_non_quanti" |
                                        name_file[,grepl("sign",names(name_file))] == "Positive_quanti", na.rm = T) > 0) * 1)
    name_file$dummy <- 1
    test <- aggregate(name_file$positive, by = list(name_file$week_nb), FUN = sum) #Nombre de positifs
    test <- test %>% dplyr::rename(numerator = x, week_nb = Group.1)
    test1 <- aggregate(name_file$dummy, by = list(name_file$week_nb), FUN = sum) #Nombre total d'échantillons
    test1 <- test1 %>% dplyr::rename(denominator = x, week_nb = Group.1)
    test_tot <- dplyr::inner_join(test,test1, by = dplyr::join_by(week_nb))
    test_tot$positive_rate <- round(test_tot$numerator / test_tot$denominator*100, digits = 2)
    test_tot <- dplyr::select(test_tot, -c(numerator, denominator))
    test_tot <- test_tot %>% dplyr::rename(!!var_name := positive_rate)
    assign(final_3, test_tot)

    sheet3 <- expand.grid(week_nb = test_tot$week_nb)
    sheet3 <- dplyr::left_join(sheet3, positivity_rate_qPCR, by = "week_nb")
    sheet3 <- sheet3 %>% dplyr::filter(sheet3$week_nb != "NA")
    sheet2 <- sheet2 %>% dplyr::select(WWTP, week_nb) %>% dplyr::distinct()
    sheet3 <- sheet3 %>% dplyr::arrange(week_nb)
    sheet3_sars <- sheet3[sheet3$week_nb >= "2020_14" & sheet3$week_nb <= format(max_time,"%Y_%V"),]
    message("debug4")
    str(sheet3_sars)

    #----------------------
    # Sheet 4
    #----------------------
    final_4 <- "positivity_rate_WWTP_qPCR"

    name_file$positive <- ((rowSums(name_file[, grepl("sign",names(name_file))] == "Positive_non_quanti" |
                                        name_file[,grepl("sign",names(name_file))] == "Positive_quanti", na.rm = T) > 0) * 1)
    name_file$dummy <- 1
    test <- aggregate(name_file$positive, by = list(name_file$week_nb, name_file$WWTP), FUN = sum) #Nombre de positifs
    test <- test %>% dplyr::rename(numerator = x, week_nb = Group.1, WWTP = Group.2)
    test1 <- aggregate(name_file$dummy, by = list(name_file$week_nb, name_file$WWTP), FUN = sum) #Nombre total d'échantillons
    test1 <- test1 %>% dplyr::rename(denominator = x, week_nb = Group.1, WWTP = Group.2)
    test_tot <- dplyr::inner_join(test,test1, by = dplyr::join_by(week_nb, WWTP))
    test_tot$positive_rate <- round(test_tot$numerator / test_tot$denominator*100, digits=2)
    test_tot <- dplyr::select(test_tot, -c(numerator, denominator))
    test_tot <- test_tot %>% dplyr::rename(!!var_name := positive_rate)
    assign(final_4, test_tot)

    sheet4 <- expand.grid(week_nb = test_tot$week_nb, WWTP = c("SCH", "PET", "BEG", "BET", "BLE", "MER", "HES", "ECH", "UEB", "GRE", "VIE", "BOE", "WIL"))
    sheet4 <- dplyr::left_join(sheet4, positivity_rate_WWTP_qPCR, by = c("week_nb","WWTP"))
    sheet4 <- sheet4 %>% dplyr::filter(sheet4$week_nb != "NA")
    sheet2 <- sheet2 %>% dplyr::select(WWTP, week_nb) %>% dplyr::distinct()
    sheet4 <- sheet4 %>% dplyr::arrange(week_nb, WWTP)
    sheet4_sars <- sheet4[sheet4$week_nb >= "2020_14" & sheet4$week_nb <= format(max_time, "%Y_%V"),]
    message("debug5")
    str(sheet4_sars)
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "1.CT Aggregate data")
    openxlsx::addWorksheet(wb, "2.CT WWTP data")
    openxlsx::addWorksheet(wb, "3.Positivity aggregate")
    openxlsx::addWorksheet(wb, "4.Positivity WWTP")
    openxlsx::writeData(wb, "1.CT Aggregate data", sheet1_sars)
    openxlsx::writeData(wb, "2.CT WWTP data", sheet2_sars)
    openxlsx::writeData(wb, "3.Positivity aggregate", sheet3_sars)
    openxlsx::writeData(wb, "4.Positivity WWTP", sheet4_sars)

    openxlsx::freezePane(wb, sheet = "1.CT Aggregate data", firstRow = TRUE)
    openxlsx::freezePane(wb, sheet = "2.CT WWTP data", firstRow = TRUE)
    openxlsx::freezePane(wb, sheet = "3.Positivity aggregate", firstRow = TRUE)
    openxlsx::freezePane(wb, sheet = "4.Positivity WWTP", firstRow = TRUE)

    xlxs_filename <- paste0(path_to_create_data_qPCR, "/SUPERVIR_WW_SARSCOV2_AGG_",
                   gsub(":", "-", sub(" CEST", "", Sys.time())),
                   ".xlsx")
    openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)

    .microbs_env$sheet1_sars <- sheet1_sars 
    .microbs_env$sheet2_sars <- sheet2_sars 
    .microbs_env$sheet3_sars <- sheet3_sars 
    .microbs_env$sheet4_sars <- sheet4_sars 
}


#--------------------------------------------------------------------------------------------------------
# Move old check created SARS data to archive
#--------------------------------------------------------------------------------------------------------
#' @title move check qPCR data to archive
#'
#' @description The old check data needs to be moved to the archive once a new check is performed.
#'
#' @param path_to_create_data_qPCR A string to describe the path to the created data. 
#' 
#' @return A character vector of archived file names (invisibly).
#' @examples
#' \dontrun{
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_create_data_qPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/3_created_data"
#' df_raw_qPCR_data <- archive_microbs_created_rsv_Data(path_to_create_data_qPCR)
#' }
#' @export
archive_microbs_created_rsv_Data <- function(path_to_create_data_qPCR = .microbs_env$created_data_path) {
    # load the data 
    if (missing(path_to_create_data_qPCR)) {
        path_to_create_data_qPCR = get_microbs_qPCR_rawDataPath()
        message("[microbs Report]: No path provided. Using default path: ", path_to_create_data_qPCR)
    }

    # load all the names of the check files files
    invisible(ifelse(!dir.exists(file.path(path_to_create_data_qPCR, "Archives")), 
                        dir.create(file.path(path_to_create_data_qPCR, "Archives")), 
                        FALSE))

    file_info <- utils::fileSnapshot(path_to_create_data_qPCR)$info
    file_info <- subset(file_info, file_info$isdir == FALSE)
    file_info <- subset(file_info, grepl("SUPERVIR_WW_SARSCOV2_AGG_", rownames(file_info)))

    file_info <- file_info[order(file_info$mtime,decreasing = TRUE),]

    latest_file <- NULL
    for (file_name in rownames(file_info)) {
        latest_file <- rownames(file_info)[which.max(file_info$mtime)]
        if (grepl("\\.xlsx$ | \\.xls$", file_name, ignore.case = TRUE)) { # Ensure it's an Excel file
            latest_file <- file_name
        }
    }

    file_info <- file_info[rownames(file_info) != latest_file, ]

    # TODO: Here use a specific format lookup such as "\\.csv$", because csv will pickup the file even if we have file.csv.txt
    files_list <- list.files(path_to_create_data_qPCR, pattern = "SUPERVIR_WW_SARSCOV2_AGG_")
    # for(file in files_list) {
    for(file in rownames(file_info)) {
        file_path <- paste(path_to_create_data_qPCR, file, sep = "/")

        if (file == latest_file) {
            next  # Skip the latest file
        } else {
            archive_path <- file.path(path_to_create_data_qPCR, "Archives", file)
            file.copy(from = file_path, to = archive_path)
            file.remove(file_path)

            # Add archived file name to list
            files_list <- c(files_list, file)
        }
    }

    if (length(files_list) == 0) {
        message("[microbs Report]: No SUPERVIR_WW_SARSCOV2_AGG_ files found to archive in ", path_to_create_data_qPCR)
        return(invisible(character(0)))
    }

    invisible(files_list)
}
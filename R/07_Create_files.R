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
#' calculations_microbs_ddPCR()
#' create_microbs_flu_file()
#' 
#' 
#' @export 
create_microbs_flu_file <- function() {
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

    # max_time <- pmax(as.Date(max(lubridate::parse_date_time(paste(as.integer(stringr::str_sub(data_flu_ddPCR$week_nb, 1, 4)),
    #                                                           as.integer(stringr::str_sub(data_flu_ddPCR$week_nb, 6, 8)), 1, sep="/"),'Y/W/w'),na.rm=TRUE)),na.rm = TRUE)

    # generate_weeks <- function(start_year, end_year) {
    #     # Créer une séquence de dates pour chaque lundi de chaque semaine
    #     dates <- seq.Date(from = as.Date(paste0(start_year, "-01-01")),
    #                     to = as.Date(paste0(end_year, "-12-31")),
    #                     by = "week")
    #     # Extraire l'année et la semaine en format "YYYY_WW"
    #     weeks <- format(dates, "%Y_%V")
    #     return(unique(weeks))
    # }

    # weeks <- data.frame(week_nb=generate_weeks(2024, 2025))
    # weeks <- weeks[weeks$week_nb >= "2020_14" & weeks$week_nb <= format(max_time,"%Y_%V"),]

    #  fluA
    df_new_calc_ddPCR_data_fluA <- subset(df_new_calc_ddPCR_data, Target_Name %in% c("FluA"))

    df_new_calc_ddPCR_data_fluA <- df_new_calc_ddPCR_data_fluA %>% dplyr::select(Sample,week_nb,copies_day_mean,inhab,copies_inhab_mean)

    names(df_new_calc_ddPCR_data_fluA)[names(df_new_calc_ddPCR_data_fluA) == "copies_day_mean"] <- "copies_day_ddPCR_FluA"
    names(df_new_calc_ddPCR_data_fluA)[names(df_new_calc_ddPCR_data_fluA) == "sign_mean"] <- "CT_sign_ddPCR_FluA"
    names(df_new_calc_ddPCR_data_fluA)[names(df_new_calc_ddPCR_data_fluA) == "copies_inhab_mean"] <- "copies_inhab_ddPCR_FluA"

    df_new_calc_ddPCR_data_fluA$WWTP <- utils_extract_WWTP(df_new_calc_ddPCR_data_fluA$Sample)

    # fluB
    df_new_calc_ddPCR_data_fluB <- subset(df_new_calc_ddPCR_data, Target_Name %in% c("fluB"))
    
    df_new_calc_ddPCR_data_fluB <- df_new_calc_ddPCR_data_fluB %>% dplyr::select(Sample, week_nb, copies_day_mean, inhab, copies_inhab_mean)

    names(df_new_calc_ddPCR_data_fluB)[names(df_new_calc_ddPCR_data_fluB) == "copies_day_mean"] <- "copies_day_ddPCR_FluB"
    names(df_new_calc_ddPCR_data_fluB)[names(df_new_calc_ddPCR_data_fluB) == "sign_mean"] <- "CT_sign_ddPCR_FluB"
    names(df_new_calc_ddPCR_data_fluB)[names(df_new_calc_ddPCR_data_fluB) == "copies_inhab_mean"] <- "copies_inhab_ddPCR_FluB"

    sheet_1_data <- dplyr::full_join(df_new_calc_ddPCR_data_fluA, df_new_calc_ddPCR_data_fluB, 
                            by = dplyr::join_by(Sample, week_nb, inhab),
                            relationship = "many-to-many")

    str(sheet_1_data)

    # Sheet 1: Aggregate the data for all weekly per WWTP and nationwide.
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

    ddPCR <- dplyr::full_join(aggregate_ddPCR_FluA,aggregate_ddPCR_FluB,by = 'week_nb')
    ddPCR <- subset(ddPCR, select = -c(inhab_sum.x,inhab_sum.y))

    sheet1_flu <- ddPCR
    # sheet1_flu <- dplyr::left_join(expand.grid(week_nb = weeks), ddPCR, by = "week_nb")
    sheet1_flu <- sheet1_flu %>% dplyr::arrange(week_nb) 

    # Sheet 2: Weekly flu concentrations per WWTP
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

    sheet2_flu <- expand.grid(week_nb = ddPCR$week_nb, WWTP = c("SCH", "PET", "BEG", "BET", "BLE", "MER", "HES", "ECH", "UEB", "GRE", "VIE", "BOE", "WIL"))
    sheet2_flu <- dplyr::left_join(sheet2_flu, ddPCR, by = c('WWTP','week_nb'))
    sheet2_flu <- sheet2_flu %>% dplyr::arrange(week_nb) #Redplyr::arrange the columns

    # Sheet 3: Weekly positivity rates (percent of positive detections) for each virus (Sheet 3)
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

    str(ddPCR)
    sheet3_flu <- ddPCR #expand.grid(week_nb = weeks)
    # sheet3_flu <- dplyr::left_join(sheet3_flu, ddPCR, by='week_nb')
    sheet3_flu <- sheet3_flu %>% dplyr::arrange(week_nb) #Redplyr::arrange the columns

    # Sheet 4
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

    sheet4_flu <- expand.grid(week_nb = ddPCR$week_nb, WWTP = c("SCH", "PET", "BEG", "BET", "BLE", "MER", "HES", "ECH", "UEB", "GRE", "VIE", "BOE", "WIL"))
    sheet4_flu <- dplyr::left_join(sheet4_flu, dplyr::full_join(sheet_2_data_tot_fluA,sheet_2_data_tot_fluB,c('WWTP','week_nb')), by=c('WWTP','week_nb'))
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
#' # Example usage
#' 
#' \dontrun{
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
#' calculations_microbs_ddPCR()
#' }
#' 
#' @export 
create_microbs_rsv_file <- function() {


}
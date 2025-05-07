#--------------------------------------------------------------------------------------------------------
# Do the Calculations for ddPCR
#--------------------------------------------------------------------------------------------------------
#' @title Do the calculations for ddPCR
#' 
#' @description We only have the raw data from the ddPCR tests, and now we need to convert
#' them into copies of RNA (per liter, per day, per day per 100.000 inhabitants)
#' The calculations can be different for for different pathogens.
#' Preferably use full path.
#' 
#' file ./microbs.lu/R/06_Calculations.R
#' 
#' @examples
#' # Example usage
#' set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/")
#' path_to_old_raw_excel_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"
#' set_microbs_loaded_DataPath(path_to_old_raw_excel_ddPCR)
#' load_microbs_old_raw_ddPCR_Data(path_to_old_raw_excel_ddPCR)
#' 
#' path_to_raw_ddPCR <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_ddPCR"
#' df_raw_ddPCR_data <- load_microbs_raw_ddPCR_Data(path_to_raw_ddPCR)
#' 
#' path_to_flux_data <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/00_flux_data"
#' set_microbs_flux_DataPath(path_to_flux_data)
#' load_microbs_flux_Data()
#' 
#' calculations_microbs_ddPCR()
#' 
#' 
#' @export 
calculations_microbs_ddPCR <- function(path_to_loaded_raw_excel_ddPCR = .microbs_env$loaded_data_path, path_to_calc_data_ddPCR = .microbs_env$calc_data_path) {
    if (is.null(path_to_loaded_raw_excel_ddPCR = .microbs_env$loaded_data_path)) {
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_loaded_DataPath(),")
        message("and then load_microbs_old_raw_qPCR_Data() function to set a path")
    }

    df_new_raw_ddPCR_data <- get_microbs_new_raw_ddPCR_Data()

    if (is.null(path_to_calc_data_ddPCR = .microbs_env$calc_data_path)) {
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_loaded_DataPath(),")
        message("and then load_microbs_old_raw_qPCR_Data() function to set a path")
    }

    df_old_calc_ddPCR_data <- load_microbs_raw_ddPCR_Data()
    df_flux_data <- get_microbs_flux_Data()

    df_new_calc_ddPCR_data <- dplyr::left_join(df_old_calc_ddPCR_data, df_flux_data, by = c('Sample'))
    df_new_calc_ddPCR_data <- df_new_calc_ddPCR_data %>%
                                    mutate(
                                        sign = case_when(
                                        Target_Name == "FluA" & Positive_droplets >= 2 ~ "positive",
                                        Target_Name == "FluB" & Positive_droplets >= 2 ~ "positive",
                                        Target_Name == "hRSV" & Positive_droplets >= 3 ~ "positive",
                                        Target_Name == "RSV" & Positive_droplets >= 3 ~ "positive",
                                        Target_Name %in% c("FluA", "FluB", "hRSV", "RSV") ~ "negative",
                                        TRUE ~ NA_character_
                                        )
                                    )

    df_new_calc_ddPCR_data <- df_new_calc_ddPCR_data %>%
                            filter(df_new_calc_ddPCR_data$Accepted_droplets >= 10000)

    df_new_calc_ddPCR_data$inhab <- case_when(
        substr(data_file_ddPCR$Sample, 1, 3) %in% "BEG" ~ 139731,
        substr(data_file_ddPCR$Sample, 1, 3) %in% "BET" ~ 53606,
        substr(data_file_ddPCR$Sample, 1, 3) %in% "PET" ~ 59481,
        substr(data_file_ddPCR$Sample, 1, 3) %in% "SCH" ~ 68143,
        substr(data_file_ddPCR$Sample, 1, 3) %in% "BLE" ~ 30930,
        substr(data_file_ddPCR$Sample, 1, 3) %in% "MER" ~ 30473,
        substr(data_file_ddPCR$Sample, 1, 3) %in% "HES" ~ 15479,
        substr(data_file_ddPCR$Sample, 1, 3) %in% "ECH" ~ 7499,
        substr(data_file_ddPCR$Sample, 1, 3) %in% "UEB" ~ 18600, 
        substr(data_file_ddPCR$Sample, 1, 3) %in% "GRE" ~ 9835,
        substr(data_file_ddPCR$Sample, 1, 3) %in% "VIE" ~ 3411,
        substr(data_file_ddPCR$Sample, 1, 3) %in% "BOE" ~ 7818,
        substr(data_file_ddPCR$Sample, 1, 3) %in% "WIL" ~ 6944,
    )

    df_new_calc_ddPCR_data$copies_L <- NA
    df_new_calc_ddPCR_data$copies_day <- NA
    df_new_calc_ddPCR_data$copies_inhab <- NA

    df_new_calc_ddPCR_data$copies_uL <- as.numeric(df_new_calc_ddPCR_data$copies_uL)

    for (i in 1:nrow(df_new_calc_ddPCR_data)){
    if(df_new_calc_ddPCR_data$sign[i] == "positive"){
            df_new_calc_ddPCR_data$copies_L[i] <-((df_new_calc_ddPCR_data$copies_uL[i]*20/5)*80)*(1000/40)

            # The following conditions are added for dilution
            if(df_new_calc_ddPCR_data$dilution[i] == 2){
                df_new_calc_ddPCR_data$copies_L[i] = df_new_calc_ddPCR_data$copies_L[i]*2 
            } else if(df_new_calc_ddPCR_data$dilution[i] == 10){
                df_new_calc_ddPCR_data$copies_L[i] = df_new_calc_ddPCR_data$copies_L[i]*10
            }
            
            df_new_calc_ddPCR_data$copies_day[i] <- df_new_calc_ddPCR_data$copies_L[i]*df_new_calc_ddPCR_data$Flow_rate[i]*1000
            df_new_calc_ddPCR_data$copies_inhab[i] <- (df_new_calc_ddPCR_data$copies_day[i]/df_new_calc_ddPCR_data$inhab[i])*100000
        }
    }











    .microbs_env$df_new_calc_ddPCR_data
}
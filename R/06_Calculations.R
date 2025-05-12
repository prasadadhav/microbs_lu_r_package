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
#' load_microbs_flux_Data(path_to_flux_data)
#' 
#' calculations_microbs_ddPCR()
#' 
#' 
#' @export 
calculations_microbs_ddPCR <- function(path_to_loaded_raw_excel_ddPCR = .microbs_env$loaded_data_path, path_to_calc_data_ddPCR = .microbs_env$calc_data_path) {
    if (is.null(path_to_loaded_raw_excel_ddPCR = .microbs_env$loaded_data_path)) {
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_loaded_DataPath(),")
        message("and then load_microbs_old_raw_ddPCR_Data() function to set a path")
    }

    df_new_raw_ddPCR_data <- get_microbs_new_raw_ddPCR_Data()

    if (is.null(path_to_calc_data_ddPCR = .microbs_env$calc_data_path)) {
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_loaded_DataPath(),")
        message("and then load_microbs_old_raw_ddPCR_Data() function to set a path")
    }

    df_old_calc_ddPCR_data <- load_microbs_raw_ddPCR_Data()
    df_flux_data <- get_microbs_flux_Data()

    df_new_calc_ddPCR_data <- dplyr::left_join(df_old_calc_ddPCR_data, df_flux_data, by = c('Sample'))
    df_new_calc_ddPCR_data <- df_new_calc_ddPCR_data %>%
                                    dplyr::mutate(
                                        sign = dplyr::case_when(
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

    df_new_calc_ddPCR_data$inhab <- dplyr::case_when(
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "BEG" ~ 139731,
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "BET" ~ 53606,
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "PET" ~ 59481,
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "SCH" ~ 68143,
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "BLE" ~ 30930,
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "MER" ~ 30473,
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "HES" ~ 15479,
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "ECH" ~ 7499,
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "UEB" ~ 18600, 
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "GRE" ~ 9835,
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "VIE" ~ 3411,
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "BOE" ~ 7818,
        substr(df_new_calc_ddPCR_data$Sample, 1, 3) %in% "WIL" ~ 6944,
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

    df_new_calc_ddPCR_data <- df_new_calc_ddPCR_data[with(df_new_calc_ddPCR_data, order(Target_Name,Sample)),]
    df_new_calc_ddPCR_data$num <- sequence(rle(df_new_calc_ddPCR_data$Sample)$lengths)
    max_reliquat <- max(df_new_calc_ddPCR_data$num)
    df_new_calc_ddPCR_data <- tidyr::pivot_wider(df_new_calc_ddPCR_data, names_from = num, id_cols = c(Sample,Target_Name),
                        values_from = c(copies_uL,
                        Accepted_droplets, 
                        Positive_droplets, 
                        Negative_droplets, 
                        sign, 
                        PoissonConfMax, 
                        PoissonConfMin, 
                        dilution, 
                        copies_L, 
                        copies_day, 
                        copies_inhab))
    
    # in R code the ;ine 122 to 141 is repeated code
    # skipping here

    df_new_calc_ddPCR_data$sign_mean <- ifelse(
            !is.na(df_new_calc_ddPCR_data$sign_1) & df_new_calc_ddPCR_data$sign_1 == "positive" |
            !is.na(df_new_calc_ddPCR_data$sign_2) & df_new_calc_ddPCR_data$sign_2 == "positive",
            "positive",
        ifelse(
            !is.na(df_new_calc_ddPCR_data$sign_1) & df_new_calc_ddPCR_data$sign_1 == "negative" &
            !is.na(df_new_calc_ddPCR_data$sign_2) & df_new_calc_ddPCR_data$sign_2 == "negative",
            "negative",
        ifelse(
            !is.na(df_new_calc_ddPCR_data$sign_1) & df_new_calc_ddPCR_data$sign_1 == "negative" &
            is.na(df_new_calc_ddPCR_data$sign_2),
            "negative",
            NA
            )
        )
    )

    df_new_calc_ddPCR_data$copies_L_mean <- apply(df_new_calc_ddPCR_data[, c("copies_L_1", "copies_L_2")], 1, function(x) {
        if (sum(!is.na(x)) == 2) {
            mean(x, na.rm = TRUE) 
        } else if
            (sum(!is.na(x)) == 1) {
            x[!is.na(x)]
        } else {
            NA
            }
        })


    df_new_calc_ddPCR_data$copies_day_mean <- apply(df_new_calc_ddPCR_data[, c("copies_day_1", "copies_day_2")], 1, function(x) {
        if (sum(!is.na(x)) == 2) {
            mean(x, na.rm = TRUE)
        } else if
        (sum(!is.na(x)) == 1) {
            x[!is.na(x)]
        } else {
            NA
        }
    })

    df_new_calc_ddPCR_data$copies_inhab_mean <- apply(df_new_calc_ddPCR_data[, c("copies_inhab_1", "copies_inhab_2")], 1, function(x) {
        if (sum(!is.na(x)) == 2) {
            mean(x, na.rm = TRUE)
        } else if
        (sum(!is.na(x)) == 1) {
            x[!is.na(x)]
        } else {
            NA
        }
    })

    df_new_calc_ddPCR_data$Sample_Date <- as.Date(df_new_calc_ddPCR_data$Sample_Date, "%d-%m-%y")
    df_new_calc_ddPCR_data <- df_new_calc_ddPCR_data %>% select(Sample, Target_Name, Sample_Date, week_nb, Flow_rate, inhab, everything()) #Rearrange the columns

    ## Ajout d'information
    Metadata <- data.frame(
        "Dataset title"= c("SUPERVIR monitoring results"),
        "Data setting" = c("xxx"),
        Description = c("..."),
        Method = c("RT-ddPCR/Promega"),
        Tags = c("Luxembourg, wastewater"),
        "Temporal Start Date" = c("2024-xx-xx"),
        "Temporal End Date" = c("ongoing"),
        "Spatial Coverage" = c("Luxembourg, whole country"),
        "Update Frequency" = c("Weekly"),
        "Last update"=c("xxx"),
        "Public access level" = c("private?"),
        Licence = c("?"),
        "Data Provider - Name" = c("Environmental Microbiology and Biotechnology research group"),
        "Data Provider - Parent Organization" = c("Luxembourg Institute of Science and Technology (LIST)"),
        "Data Provider - Website" = c("https..."),
        "Data Provider - Contact person" = c("OGORZALY Leslie"),
        "Data Provider - Contact email" = c("leslie.ogorzaly@list.lu"),
        check.names = FALSE)

    sites <- data.frame(
        "Site ID" = c("BEG","BET","PET","SCH"),
        "Site Name" =c("BEGGEN","BETTEMBOURG","PETANGE","SCHIFFLANGE"),
        "Water Type" = rep("wastewater (WW)", times=4),
        "Site Matrix Type" = rep("influent at WWTP",times=4),
        "Admin0 (Country/Region/Sovereignty)" = rep("Luxembourg",times = 4),
        "Admin1 (Province/State/Dependency)" = c("Luxembourg","Esch-sur-Alzette","Esch-sur-Alzette","Esch-sur-Alzette"),
        "Admin2 (County/Commune/Municipality)" = c("Luxembourg City","Bettembourg","Pétange","Schifflange"),
        "Admin3 (City/City Area)" = c("Beggen","Bettembourg","Pétange","Schifflange"),
        "Address Line" = c("1 Rue du Pont, 7245 Bereldange, Luxembourg","1 route de Crauthem, 3390 Peppange, Luxembourg",
                        "6, rue du Stade, 4711 Pétange, Luxembourg","4149 Schifflange, Luxembourg"),
        "Postal Code" = c("L-7245","L-3390","L-4711","L-4149"),
        Latitude = c("49.6510432621253","49.5223130310986","49.5586191970552","49.5139116670777"),
        Longitude = c("6.13536742963523","6.11706254069125","5.85486184611902","6.01911357824348"),
        "Geodata Link" = rep("",times=4),
        "Contact Person" = rep("OGORZALY Leslie",times=4),
        Phone = rep("",times=4),
        "Email" = rep("leslie.ogorzaly@list.lu",times=4),
        "Sample Collection Method" = rep("composite",times=4),
        "Population Served" = c("139731","53606","59481","68143"),
        "Annual Average Daily Flow" = c("44000","28200","18000","18000"),
        "Flow Units" = rep("m3/day",times=4),
        "Sampling Frequency" = rep("weekly",times=4),
        Active = rep("yes",times=4),
        Notes = c("City, Ville du Luxembourg","Syndicat Intercommunal STEP","Syndicat Intercommunal SIACH","Syndicat Intercommunal SIVEC"),
        check.names = FALSE
    )

    # write the new data into the file
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "1.Metadata")
    openxlsx::addWorksheet(wb, "2.Sites")
    openxlsx::addWorksheet(wb, "3.Data")
    openxlsx::writeData(wb, "1.Metadata", Metadata)
    openxlsx::writeData(wb, "2.Sites", sites)
    openxlsx::writeData(wb, "3.Data", df_new_raw_ddPCR_data)
    openxlsx::freezePane(wb, sheet = "3.Data", firstRow = TRUE)
    xlxs_filename <- paste0(path_to_calc_data_ddPCR, "/SUPERVIR_CAL_DATA_ddPCR_",
                   gsub(":", "-", sub(" CEST", "", Sys.time())),
                   ".xlsx")
    openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)


    .microbs_env$df_new_calc_ddPCR_data
}
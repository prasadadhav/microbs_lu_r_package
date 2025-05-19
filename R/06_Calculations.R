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
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_calc_DataPath(),")
        message("and then load_microbs_old_calc_ddPCR_Data() function to set a path")
    }

    df_old_calc_ddPCR_data <- load_microbs_raw_ddPCR_Data()
    df_flux_data <- get_microbs_flux_Data()

    df_new_calc_ddPCR_data <- dplyr::left_join(df_old_calc_ddPCR_data, df_flux_data, by = c('Sample'))
    str(df_old_calc_ddPCR_data)
    str(df_flux_data)
    str(df_new_calc_ddPCR_data)

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
                             dplyr::filter(df_new_calc_ddPCR_data$Accepted_droplets >= 10000)

    str(df_new_calc_ddPCR_data)
    df_new_calc_ddPCR_data <- df_new_calc_ddPCR_data %>%  dplyr::mutate(
        inhab = dplyr::case_when(
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
    df_new_calc_ddPCR_data <- tidyr::pivot_wider(df_new_calc_ddPCR_data, names_from = num, 
                                                                        id_cols = c(Sample, 
                                                                        Target_Name, 
                                                                        Sample_Date, 
                                                                        week_nb, 
                                                                        Flow_rate, 
                                                                        inhab),
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
    
    # in R code the line 122 to 141 is repeated code
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
    df_new_calc_ddPCR_data <- df_new_calc_ddPCR_data %>% dplyr::select(Sample, Target_Name, Sample_Date, week_nb, Flow_rate, inhab, everything()) #Rearrange the columns
    
    .microbs_env$df_new_calc_ddPCR_data <- df_new_calc_ddPCR_data

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
    openxlsx::writeData(wb, "3.Data", df_new_calc_ddPCR_data)
    openxlsx::freezePane(wb, sheet = "3.Data", firstRow = TRUE)
    xlxs_filename <- paste0(path_to_calc_data_ddPCR, "/SUPERVIR_CAL_DATA_ddPCR_",
                   gsub(":", "-", sub(" CEST", "", Sys.time())),
                   ".xlsx")
    openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)

    return(.microbs_env$df_new_calc_ddPCR_data)
}


#--------------------------------------------------------------------------------------------------------
# Do the Calculations for qPCR
#--------------------------------------------------------------------------------------------------------
#' @title Do the calculations for qPCR
#' 
#' @description We only have the raw data from the qPCR tests, and now we need to convert
#' them into copies of RNA (per liter, per day, per day per 100.000 inhabitants)
#' The calculations can be different for for different pathogens.
#' Preferably use full path.
#' 
#' file ./microbs.lu/R/06_Calculations.R
#' 
#' @examples
#' # Example usage
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
#' calculations_microbs_qPCR()
#' 
#' 
#' @export 
#' 
#' 

calculations_microbs_qPCR <- function(path_to_loaded_raw_excel_qPCR = .microbs_env$loaded_data_path, path_to_calc_data_qPCR = .microbs_env$calc_data_path) {
    if (is.null(path_to_loaded_raw_excel_qPCR = .microbs_env$loaded_data_path)) {
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_loaded_DataPath(),")
        message("and then load_microbs_old_raw_qPCR_Data() function to set a path")
    }
    
    df_new_raw_qPCR_data <- get_microbs_new_raw_qPCR_Data()

    if (is.null(path_to_calc_data_qPCR = .microbs_env$calc_data_path)) {
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_calc_DataPath(),")
        message("and then load_microbs_old_calc_qPCR_Data() function to set a path")
    }

    # useless here be careful, see if need later
    # df_old_calc_qPCR_data <- load_microbs_raw_qPCR_Data()

    

    # PSA: Here if CT is undetermined or NA we Replace by 999
    df_new_raw_qPCR_data$CT <- ifelse(
        suppressWarnings(is.na(as.numeric(
            df_new_raw_qPCR_data$CT))) | df_new_raw_qPCR_data$CT == "Undetermined",
            999,
            as.numeric(df_new_raw_qPCR_data$CT
        )
    )

    df_new_raw_qPCR_data$CT_sign <- dplyr::case_when(
        as.numeric(df_new_raw_qPCR_data$CT) <= 37 ~ "Positive",
        as.numeric(df_new_raw_qPCR_data$CT) > 37 ~ "Negative",
        is.na(as.numeric(df_new_raw_qPCR_data$CT)) ~ "Negative",
        df_new_raw_qPCR_data$CT == "Undetermined" ~ "Negative"
    )

    df_new_raw_qPCR_data$positive <- dplyr::case_when(
        as.numeric(df_new_raw_qPCR_data$CT) <= 37 ~ 1,
        as.numeric(df_new_raw_qPCR_data$CT) > 37 ~ 0,
        is.na(as.numeric(df_new_raw_qPCR_data$CT)) ~ 0,
        df_new_raw_qPCR_data$CT == "Undetermined" ~ 0
    )

    # total <- df_new_raw_qPCR_data %>% 
    #             dplyr::group_by(Target_Name, Sample, .drop = FALSE) %>% 
    #             dplyr::count() 3

    df_new_raw_qPCR_data <- df_new_raw_qPCR_data[with(df_new_raw_qPCR_data, order(Target_Name, Sample)),]
    df_new_raw_qPCR_data_mutate <- df_new_raw_qPCR_data
    df_new_raw_qPCR_data_mutate$num <- sequence(rle(df_new_raw_qPCR_data_mutate$Sample)$lengths)
    max_reliquat <- max(df_new_raw_qPCR_data_mutate$num)
    df_new_raw_qPCR_data_mutate <- pivot_wider(df_new_raw_qPCR_data_mutate,
                                                    names_from = num,
                                                    id_cols = c(Sample,Target_Name),
                                                    values_from = c(CT,CT_sign,positive)
                                                )

    std_curve_path <- get_microbs_stdCurve_DataPath()
    info_target_data<- read.csv(paste0(std_curve_path,"/","SUPERVIR_LIST_RT-qPCR_Assay summary.csv"), header = TRUE)
    df_new_raw_qPCR_data_mutate <- dplyr::left_join(df_new_raw_qPCR_data_mutate, info_target_data, 
                                                    by = join_by(Target_Name == Assay)
                                                    )

    df_new_raw_qPCR_data_mutate <- df_new_raw_qPCR_data_mutate %>%
                    mutate(sumrange = rowSums(select(., starts_with("positive")), na.rm = TRUE))

    df_new_raw_qPCR_data_mutate <- df_new_raw_qPCR_data_mutate %>% select(-starts_with("positive"))
    df_new_raw_qPCR_data_mutate <- subset(df_new_raw_qPCR_data_mutate, select =- c(sumrange))

    df_new_raw_qPCR_data_mutate <- df_new_raw_qPCR_data_mutate %>%  dplyr::mutate(
        inhab = dplyr::case_when(
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "BEG" ~ 139731,
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "BET" ~ 53606,
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "PET" ~ 59481,
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "SCH" ~ 68143,
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "BLE" ~ 30930,
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "MER" ~ 30473,
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "HES" ~ 15479,
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "ECH" ~ 7499,
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "UEB" ~ 18600, 
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "GRE" ~ 9835,
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "VIE" ~ 3411,
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "BOE" ~ 7818,
            substr(df_new_raw_qPCR_data_mutate$Sample, 1, 3) %in% "WIL" ~ 6944,
        )
    )

    df_flux_data <- get_microbs_flux_Data()
    df_flux_data$`Sampling Dates` <- as.character(df_flux_data$`Sampling Dates`)
    df_flux_data <- df_flux_data[ , !names(df_flux_data) %in% c("Site", "Water type")]
    colnames(flux) <- c('Sample','Sample_Date','Flow_rate','week_nb')
    df_new_raw_qPCR_data_mutate <- dplyr::left_join(df_new_raw_qPCR_data_mutate, df_flux_data, by = c("Sample"))

    df_new_raw_qPCR_data_mutate$copies_reaction_1 = 10^((df_new_raw_qPCR_data_mutate$Intercept - as.numeric(df_new_raw_qPCR_data_mutate$CT_1)) / abs(df_new_raw_qPCR_data_mutate$Slope))
    df_new_raw_qPCR_data_mutate$copies_reaction_2 = 10^((df_new_raw_qPCR_data_mutate$Intercept - as.numeric(df_new_raw_qPCR_data_mutate$CT_2)) / abs(df_new_raw_qPCR_data_mutate$Slope))
    df_new_raw_qPCR_data_mutate$copies_reaction_3 = 10^((df_new_raw_qPCR_data_mutate$Intercept - as.numeric(df_new_raw_qPCR_data_mutate$CT_3)) / abs(df_new_raw_qPCR_data_mutate$Slope))
    df_new_raw_qPCR_data_mutate$copies_reaction_4 = 10^((df_new_raw_qPCR_data_mutate$Intercept - as.numeric(df_new_raw_qPCR_data_mutate$CT_4)) / abs(df_new_raw_qPCR_data_mutate$Slope))

    df_new_raw_qPCR_data_mutate <- df_new_raw_qPCR_data_mutate %>% mutate(
        copies_reaction_1 = ifelse(str_detect(Sample, "d$"), copies_reaction_1 * 10, copies_reaction_1),
        copies_reaction_2 = ifelse(str_detect(Sample, "d$"), copies_reaction_2 * 10, copies_reaction_2),
        copies_reaction_3 = ifelse(str_detect(Sample, "d$"), copies_reaction_3 * 10, copies_reaction_3),
        copies_reaction_4 = ifelse(str_detect(Sample, "d$"), copies_reaction_4 * 10, copies_reaction_4)
    )

    df_new_raw_qPCR_data_mutate$CT_sign_1 <- ifelse(
      df_new_raw_qPCR_data_mutate$CT_sign_1 == "Positive" & df_new_raw_qPCR_data_mutate$copies_reaction_1 >= df_new_raw_qPCR_data_mutate$LOQ..copie.reaction., "Positive_quanti",
            ifelse(df_new_raw_qPCR_data_mutate$CT_sign_1 =="Positive" & df_new_raw_qPCR_data_mutate$copies_reaction_1 < df_new_raw_qPCR_data_mutate$LOQ..copie.reaction.,
                    "Positive_non_quanti",df_new_raw_qPCR_data_mutate$CT_sign_1)
                )

    df_new_raw_qPCR_data_mutate$CT_sign_2 <- ifelse(
        df_new_raw_qPCR_data_mutate$CT_sign_2 == "Positive" & df_new_raw_qPCR_data_mutate$copies_reaction_2 >= df_new_raw_qPCR_data_mutate$LOQ..copie.reaction., "Positive_quanti",
            ifelse(df_new_raw_qPCR_data_mutate$CT_sign_2 =="Positive" & df_new_raw_qPCR_data_mutate$copies_reaction_2 < df_new_raw_qPCR_data_mutate$LOQ..copie.reaction.,
                    "Positive_non_quanti",df_new_raw_qPCR_data_mutate$CT_sign_2)
                    )
    
    df_new_raw_qPCR_data_mutate$CT_sign_3 <- ifelse(
        df_new_raw_qPCR_data_mutate$CT_sign_3 == "Positive" & df_new_raw_qPCR_data_mutate$copies_reaction_3 >= df_new_raw_qPCR_data_mutate$LOQ..copie.reaction., "Positive_quanti",
            ifelse(df_new_raw_qPCR_data_mutate$CT_sign_3 =="Positive" & df_new_raw_qPCR_data_mutate$copies_reaction_3 < df_new_raw_qPCR_data_mutate$LOQ..copie.reaction.,
                    "Positive_non_quanti",df_new_raw_qPCR_data_mutate$CT_sign_3)
                    )
    
    df_new_raw_qPCR_data_mutate$CT_sign_4 <- ifelse(
        df_new_raw_qPCR_data_mutate$CT_sign_4 == "Positive" & df_new_raw_qPCR_data_mutate$copies_reaction_4 >= df_new_raw_qPCR_data_mutate$LOQ..copie.reaction., "Positive_quanti",
            ifelse(df_new_raw_qPCR_data_mutate$CT_sign_4 =="Positive" & df_new_raw_qPCR_data_mutate$copies_reaction_4 < df_new_raw_qPCR_data_mutate$LOQ..copie.reaction.,
                    "Positive_non_quanti",df_new_raw_qPCR_data_mutate$CT_sign_4)
                    )

    # PSA: for qPCR, we directly have 20uL so we do not need to convert 5uL to 20uL, hence we do not multiply by 20
    df_new_raw_qPCR_data_mutate$copies_L_1 <- ifelse(
        df_new_raw_qPCR_data_mutate$CT_sign_1 == "Positive_quanti" & 
        !is.na(df_new_raw_qPCR_data_mutate$CT_sign_1) & 
        !is.na(df_new_raw_qPCR_data_mutate$copies_reaction_1), 
            ((df_new_raw_qPCR_data_mutate$copies_reaction_1/ 5) * 80) * (1000 / 40), NA
        )
    
    df_new_raw_qPCR_data_mutate$copies_L_2 <- ifelse(
        df_new_raw_qPCR_data_mutate$CT_sign_2 == "Positive_quanti" & 
        !is.na(df_new_raw_qPCR_data_mutate$CT_sign_2) & 
        !is.na(df_new_raw_qPCR_data_mutate$copies_reaction_2), 
            ((df_new_raw_qPCR_data_mutate$copies_reaction_2/ 5) * 80) * (1000 / 40), NA
        )
    
    df_new_raw_qPCR_data_mutate$copies_L_3 <- ifelse(
        df_new_raw_qPCR_data_mutate$CT_sign_3 == "Positive_quanti" & 
        !is.na(df_new_raw_qPCR_data_mutate$CT_sign_3) & 
        !is.na(df_new_raw_qPCR_data_mutate$copies_reaction_3), 
            ((df_new_raw_qPCR_data_mutate$copies_reaction_3/ 5) * 80) * (1000 / 40), NA
        )
    
    df_new_raw_qPCR_data_mutate$copies_L_4 <- ifelse(
        df_new_raw_qPCR_data_mutate$CT_sign_4 == "Positive_quanti" & 
        !is.na(df_new_raw_qPCR_data_mutate$CT_sign_4) & 
        !is.na(df_new_raw_qPCR_data_mutate$copies_reaction_4), 
            ((df_new_raw_qPCR_data_mutate$copies_reaction_4/ 5) * 80) * (1000 / 40), NA
        )

    df_new_raw_qPCR_data_mutate$copies_day_1 <- df_new_raw_qPCR_data_mutate$copies_L_1 * df_new_raw_qPCR_data_mutate$Flow_rate * 1000
    df_new_raw_qPCR_data_mutate$copies_day_2 <- df_new_raw_qPCR_data_mutate$copies_L_2 * df_new_raw_qPCR_data_mutate$Flow_rate * 1000
    df_new_raw_qPCR_data_mutate$copies_day_3 <- df_new_raw_qPCR_data_mutate$copies_L_3 * df_new_raw_qPCR_data_mutate$Flow_rate * 1000
    df_new_raw_qPCR_data_mutate$copies_day_4 <- df_new_raw_qPCR_data_mutate$copies_L_4 * df_new_raw_qPCR_data_mutate$Flow_rate * 1000

    df_new_raw_qPCR_data_mutate$copies_inhab_1 <- (df_new_raw_qPCR_data_mutate$copies_day_1 / df_new_raw_qPCR_data_mutate$inhab) * 100000
    df_new_raw_qPCR_data_mutate$copies_inhab_2 <- (df_new_raw_qPCR_data_mutate$copies_day_2 / df_new_raw_qPCR_data_mutate$inhab) * 100000
    df_new_raw_qPCR_data_mutate$copies_inhab_3 <- (df_new_raw_qPCR_data_mutate$copies_day_3 / df_new_raw_qPCR_data_mutate$inhab) * 100000
    df_new_raw_qPCR_data_mutate$copies_inhab_4 <- (df_new_raw_qPCR_data_mutate$copies_day_4 / df_new_raw_qPCR_data_mutate$inhab) * 100000

    df_new_raw_qPCR_data_mutate <- subset(df_new_raw_qPCR_data_mutate, dplyr::select = -c(rep2.LOD, rep3.LOD,rep4.LOD,rep5.LOD,rep8.LOD))
    df_new_calc_qPCR_data <- df_new_raw_qPCR_data_mutate

    df_new_calc_qPCR_data$mean_copies_L <- rowMeans(df_new_calc_qPCR_data[, c("copies_L_1", "copies_L_2", "copies_L_3", "copies_L_4")], na.rm = TRUE)
    df_new_calc_qPCR_data$mean_copies_day <- rowMeans(df_new_calc_qPCR_data[, c("copies_day_1", "copies_day_2", "copies_day_3", "copies_day_4")], na.rm = TRUE)
    df_new_calc_qPCR_data$mean_copies_day_inhab <- rowMeans(df_new_calc_qPCR_data[, c("copies_inhab_1", "copies_inhab_2", "copies_inhab_3", "copies_inhab_4")], na.rm = TRUE)

    df_new_calc_qPCR_data <- df_new_calc_qPCR_data %>% dplyr::select(Sample, Target_Name, Sample_Date, week_nb, inhab, everything())

    .microbs_env$df_new_calc_qPCR_data <- df_new_raw_qPCR_data

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
    openxlsx::writeData(wb, "3.Data", df_new_calc_qPCR_data)
    openxlsx::freezePane(wb, sheet = "3.Data", firstRow = TRUE)
    xlxs_filename <- paste0(path_to_calc_data_ddPCR, "/SUPERVIR_CAL_DATA_qPCR_",
                   gsub(":", "-", sub(" CEST", "", Sys.time())),
                   ".xlsx")
    openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)

    return(.microbs_env$df_new_calc_ddPCR_data)
}
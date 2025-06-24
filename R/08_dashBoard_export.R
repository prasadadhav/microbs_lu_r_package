#--------------------------------------------------------------------------------------------------------
# Export the flu files for dashboard
#--------------------------------------------------------------------------------------------------------
#' @title Do the export of flu for dashboard
#' 
#' @description The creation step export the calculated data 
#' for export to be used on the dashboard for each virus. 
#' In this funcion we handle all the excel files in the created data directory
#' 
#' 
#' file ./microbs.lu/R/08_dashBoard_export.R
#' 
#' @examples
#' # Example usage
#' 
#' @export

dashboard_microbs_flu_export <- function(sheet1_flu = .microbs_env$sheet1_flu, sheet2_flu = .microbs_env$sheet2_flu, path_to_create_data_ddPCR= .microbs_env$created_data_path){
    if (is.null(path_to_create_data_ddPCR)) {
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_created_DataPath(),")
        message("and then load_microbs_old_raw_ddPCR_Data() function to set a path")
    }

    path_to_dashboard_data_flu <- get_microbs_dashboard_DataPath()

    df_nat <- get_microbs_new_create_flu_sheet1()
    df_wwtp <- get_microbs_new_create_flu_sheet2()

    df_old <- get_microbs_old_dashboard_flu_Data()
    df_old_trimmed <- df_old %>%
                        dplyr::filter(`yyyy-w (Flu)` <= "2024_52")
    df_old_trimmed <- df_old_trimmed %>% dplyr::select(!starts_with(c("SAMPLES", "SITUATION")))

    df_samples <- df_old %>% dplyr::select(starts_with(c("SAMPLES", "SITUATION")))
    df_situation <- df_old %>% dplyr::select(starts_with("SITUATION"))
    df_situation <- df_situation[1,]
    df_samples <- df_samples[1,]

    # National level Flu A
    df_nat_fluA <- df_nat %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0)))
    df_nat_fluA_long <- df_nat_fluA %>%
                        dplyr::select(week_nb, copies_days_inhab_ddPCR_FluA) %>%
                        dplyr::rename(`FluA-Nat` = copies_days_inhab_ddPCR_FluA) %>%
                        dplyr::mutate(`Mov-FluA-Nat` = zoo::na.locf(
                            zoo::rollmean(`FluA-Nat`, k = 3, fill = NA, align = "right"),
                            na.rm = FALSE
                            )
                        )

    # National level Flu B
    df_nat_fluB <- df_nat %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0)))
    df_nat_fluB_long <- df_nat_fluB %>%
                        dplyr::select(week_nb, copies_days_inhab_ddPCR_FluB) %>%
                        dplyr::rename(`FluB-Nat` = copies_days_inhab_ddPCR_FluB) %>%
                        dplyr::mutate(`Mov-FluB-Nat` = zoo::na.locf(
                            zoo::rollmean(`FluB-Nat`, k = 3, fill = NA, align = "right"),
                            na.rm = FALSE
                            )
                        )

    # Per WWTP Flu A
    df_wwtp_fluA_wide <- df_wwtp %>%
                        dplyr::select(week_nb, WWTP, copies_inhab_ddPCR_FluA) %>%
                        tidyr::pivot_wider(
                            names_from = WWTP,
                            values_from = copies_inhab_ddPCR_FluA,
                            names_prefix = "FluA-"
                        ) %>%
                        dplyr::arrange(week_nb)        

    df_wwtp_fluA_mov <- df_wwtp_fluA_wide %>%
                        dplyr::mutate(dplyr::across(dplyr::starts_with("FluA-"),
                                                    ~ zoo::na.locf(zoo::rollmean(.x, k = 3, fill = NA, align = "right"), na.rm = FALSE),
                                                    .names = "Mov-{.col}"))

    # Per WWTP Flu B
    df_wwtp_fluB_wide <- df_wwtp %>%
                        dplyr::select(week_nb, WWTP, copies_inhab_ddPCR_FluB) %>%
                        tidyr::pivot_wider(
                            names_from = WWTP,
                            values_from = copies_inhab_ddPCR_FluB,
                            names_prefix = "FluB-"
                        ) %>%
                        dplyr::arrange(week_nb)        

    df_wwtp_fluB_mov <- df_wwtp_fluB_wide %>%
                        dplyr::mutate(dplyr::across(dplyr::starts_with("FluB-"),
                                                    ~ zoo::na.locf(zoo::rollmean(.x, k = 3, fill = NA, align = "right"), na.rm = FALSE),
                                                    .names = "Mov-{.col}"))

    # Combine all
    df_final <- df_nat_fluA_long %>%
                    dplyr::left_join(df_nat_fluB_long, by = "week_nb") %>%
                    dplyr::left_join(df_wwtp_fluA_mov, by = "week_nb") %>%
                    dplyr::left_join(df_wwtp_fluB_mov, by = "week_nb") %>%
                    dplyr::rename(`yyyy-w (Flu)` = week_nb) %>%
                    dplyr::arrange(`yyyy-w (Flu)`)

    df_final_trimmed <- df_final %>%
                            dplyr::filter(`yyyy-w (Flu)` > "2024_52")

    df_final_combined <- dplyr::bind_rows(df_old_trimmed, df_final_trimmed) %>%
                            dplyr::arrange(`yyyy-w (Flu)`)
    
    # recompute the rolling mean for the final combined data
    # fluA
    fluA_cols <- grep("^FluA-", names(df_final_combined), value = TRUE)
    mov_fluA_cols <- paste0("Mov-", fluA_cols)
    sample_fluA_cols <- sub("^FluA-", "SAMPLES-", fluA_cols)

    for (i in seq_along(fluA_cols)) {
        df_final_combined[[mov_fluA_cols[i]]] <- zoo::na.locf(zoo::rollmean(df_final_combined[[fluA_cols[i]]], 
                                                        k = 3, 
                                                        fill = NA, 
                                                        align = "right"), na.rm = FALSE)
    }

    # fluB
    fluB_cols <- grep("^FluB-", names(df_final_combined), value = TRUE)
    mov_fluB_cols <- paste0("Mov-", fluB_cols)
    sample_fluB_cols <- sub("^FluB-", "SAMPLES-", fluB_cols)

    for (i in seq_along(fluB_cols)) {
        df_final_combined[[mov_fluB_cols[i]]] <- zoo::na.locf(zoo::rollmean(df_final_combined[[fluB_cols[i]]], 
                                                        k = 3, 
                                                        fill = NA, 
                                                        align = "right"), na.rm = FALSE)
    }

    # df_final_combined[df_final_combined == 0] <- NA
    
    # Initialize an empty list to store sample counts
    sample_counts <- list()

    # Count non-NA values for each fluA column and store in corresponding SAMPLES- column
    # same values for fluA and fluB
    for (i in seq_along(fluA_cols)) {
        sample_counts[[sample_fluA_cols[i]]][1] <- sum(!is.na(df_final_combined[[fluA_cols[i]]]))
    }

    # Create a one-row data.frame from the sample counts
    df_samples_new <- as.data.frame(sample_counts, optional = TRUE)
    total_samples <- 0
    for (col in names(df_samples_new)) {
        if (col == "SAMPLES-Nat") {
            next
        } else {
            total_samples <- total_samples + df_samples_new[[col]][1]
        }
    }
    df_samples_new$`SAMPLES-Nat`[1] <- total_samples

    # df_final_combined <- df_final_combined %>% dplyr::bind_cols(df_samples)
            
    # styles
    sci_style <- openxlsx::createStyle(numFmt = "0.00E+00")
    header_st <- openxlsx::createStyle(textDecoration = "Bold")

    n_cols_main <- ncol(df_final_combined)
    n_rows_main <- nrow(df_final_combined)

    # write the new data into the file
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Data")
    
    # write the data
    openxlsx::writeData(wb, "Data", df_final_combined, startCol = 1, startRow = 1, headerStyle = header_st)

    # write the samples
    openxlsx::writeData(
        wb,
        sheet = "Data",
        x = df_samples_new,
        startCol = n_cols_main + 1,
        startRow = 1,
        headerStyle = header_st
    )

    n_cols_samples <- ncol(df_samples_new)
    openxlsx::writeData(
        wb,
        sheet = "Data",
        x = df_situation,
        startCol = n_cols_main + n_cols_samples + 1,
        startRow = 1,
        headerStyle = header_st
    )
        
    numeric_cols <- which(sapply(df_final_combined, is.numeric))
    for (col in numeric_cols) {
        openxlsx::addStyle(
            wb,
            sheet = "Data",
            style = sci_style,
            rows = 2:(n_rows_main + 1),  # +1 to account for header
            cols = col,
            gridExpand = TRUE,
            stack = TRUE
        )
    }
    
    openxlsx::freezePane(wb, sheet = "Data", firstRow = TRUE)

    max_cols <- ncol(df_samples_new) + ncol(df_final_combined) + 5
    openxlsx::setColWidths(
        wb,
        sheet = "Data",
        cols = 1:max_cols,
        widths = "auto"
    )

    openxlsx::setColWidths(
        wb,
        sheet = "Data",
        cols = 38:57,
        widths = 1,         
        hidden = TRUE       
    )

    # openxlsx::setColWidths(
    #     wb,
    #     sheet = "Data",
    #     cols = 39:43,
    #     widths = 1,         
    #     hidden = TRUE       
    # )

    xlxs_filename <- paste0(path_to_dashboard_data_flu, "/Data_Flu_",
                   gsub(":", "-", sub(" CEST", "", Sys.time())),
                   ".xlsx")
    openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)

    .microbs_env$df_new_dashboard_flu_data <- df_final_combined
    .microbs_env$df_new_dashboard_flu_data
}


#--------------------------------------------------------------------------------------------------------
# Export the RSV files for dashboard
#--------------------------------------------------------------------------------------------------------
#' @title Do the export of RSV for dashboard
#' 
#' @description The creation step export the calculated data 
#' for export to be used on the dashboard for each virus. 
#' In this funcion we handle all the excel files in the created data directory
#' 
#' 
#' file ./microbs.lu/R/08_dashBoard_export.R
#' 
#' @examples
#' # Example usage
#' 
#' @export

dashboard_microbs_hRSV_export <- function(sheet1_hRSV = .microbs_env$sheet1_hRSV, sheet2_hRSV = .microbs_env$sheet2_hRSV, path_to_create_data_ddPCR= .microbs_env$created_data_path){
    if (is.null(path_to_create_data_ddPCR)) {
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_created_DataPath(),")
        message("and then load_microbs_old_raw_ddPCR_Data() function to set a path")
    }

    path_to_dashboard_data_hRSV <- get_microbs_dashboard_DataPath()

    df_nat <- get_microbs_new_create_hRSV_sheet1()
    df_wwtp <- get_microbs_new_create_hRSV_sheet2()

    df_old <- get_microbs_old_dashboard_hRSV_Data()
    df_old_trimmed <- df_old %>%
                        dplyr::filter(`yyyy-w (RSV)` <= "2024_52")
    df_old_trimmed <- df_old_trimmed %>% dplyr::select(!starts_with(c("SAMPLES", "SITUATION")))

    df_samples <- df_old %>% dplyr::select(starts_with(c("SAMPLES", "SITUATION")))
    df_situation <- df_old %>% dplyr::select(starts_with("SITUATION"))
    df_situation <- df_situation[1,]
    df_samples <- df_samples[1,]

    # National level
    df_nat <- df_nat %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0)))
    df_nat_long <- df_nat %>%
                        dplyr::select(week_nb, copies_days_inhab_ddPCR_hRSV) %>%
                        dplyr::rename(`RSV-Nat` = copies_days_inhab_ddPCR_hRSV) %>%
                        dplyr::mutate(`Mov-RSV-Nat` = zoo::na.locf(
                            zoo::rollmean(`RSV-Nat`, k = 3, fill = NA, align = "right"),
                            na.rm = FALSE
                            )
                        )

    # Per WWTP
    df_wwtp_wide <- df_wwtp %>%
                        dplyr::select(week_nb, WWTP, copies_inhab_ddPCR_hRSV) %>%
                        tidyr::pivot_wider(
                            names_from = WWTP,
                            values_from = copies_inhab_ddPCR_hRSV,
                            names_prefix = "RSV-"
                        ) %>%
                        dplyr::arrange(week_nb)        

    df_wwtp_mov <- df_wwtp_wide %>%
                        dplyr::mutate(dplyr::across(dplyr::starts_with("RSV-"),
                                                    ~ zoo::na.locf(zoo::rollmean(.x, k = 3, fill = NA, align = "right"), na.rm = FALSE),
                                                    .names = "Mov-{.col}"))

    # Combine all
    df_final <- df_nat_long %>%
                    dplyr::left_join(df_wwtp_mov, by = "week_nb") %>%
                    dplyr::rename(`yyyy-w (RSV)` = week_nb) %>%
                    dplyr::arrange(`yyyy-w (RSV)`)

    df_final_trimmed <- df_final %>%
                            dplyr::filter(`yyyy-w (RSV)` > "2024_52")

    df_final_combined <- dplyr::bind_rows(df_old_trimmed, df_final_trimmed) %>%
                            dplyr::arrange(`yyyy-w (RSV)`)
    
    # recompute the rolling mean for the final combined data
    rsv_cols <- grep("^RSV-", names(df_final_combined), value = TRUE)
    mov_rsv_cols <- paste0("Mov-", rsv_cols)
    sample_cols <- sub("^RSV-", "SAMPLES-", rsv_cols)

    for (i in seq_along(rsv_cols)) {
        df_final_combined[[mov_rsv_cols[i]]] <- zoo::na.locf(zoo::rollmean(df_final_combined[[rsv_cols[i]]], 
                                                        k = 3, 
                                                        fill = NA, 
                                                        align = "right"), na.rm = FALSE)
    }

    # df_final_combined[df_final_combined == 0] <- NA
    
    # Initialize an empty list to store sample counts
    sample_counts <- list()

    # Count non-NA values for each RSV column and store in corresponding SAMPLES- column
    for (i in seq_along(rsv_cols)) {
        sample_counts[[sample_cols[i]]][1] <- sum(!is.na(df_final_combined[[rsv_cols[i]]]))
    }

    # Create a one-row data.frame from the sample counts
    df_samples_new <- as.data.frame(sample_counts, optional = TRUE)
    total_samples <- 0
    for (col in names(df_samples_new)) {
        if (col == "SAMPLES-Nat") {
            next
        } else {
            total_samples <- total_samples + df_samples_new[[col]][1]
        }
    }
    df_samples_new$`SAMPLES-Nat`[1] <- total_samples

    # df_final_combined <- df_final_combined %>% dplyr::bind_cols(df_samples)
            
    # styles
    sci_style <- openxlsx::createStyle(numFmt = "0.00E+00")
    header_st <- openxlsx::createStyle(textDecoration = "Bold")

    n_cols_main <- ncol(df_final_combined)
    n_rows_main <- nrow(df_final_combined)

    # write the new data into the file
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Data")
    
    # write the data
    openxlsx::writeData(wb, "Data", df_final_combined, startCol = 1, startRow = 1, headerStyle = header_st)

    # write the samples
    openxlsx::writeData(
        wb,
        sheet = "Data",
        x = df_samples_new,
        startCol = n_cols_main + 1,
        startRow = 1,
        headerStyle = header_st
    )

    n_cols_samples <- ncol(df_samples_new)
    openxlsx::writeData(
        wb,
        sheet = "Data",
        x = df_situation,
        startCol = n_cols_main + n_cols_samples + 1,
        startRow = 1,
        headerStyle = header_st
    )
        
    numeric_cols <- which(sapply(df_final_combined, is.numeric))
    for (col in numeric_cols) {
        openxlsx::addStyle(
            wb,
            sheet = "Data",
            style = sci_style,
            rows = 2:(n_rows_main + 1),  # +1 to account for header
            cols = col,
            gridExpand = TRUE,
            stack = TRUE
        )
    }
    
    openxlsx::freezePane(wb, sheet = "Data", firstRow = TRUE)

    openxlsx::setColWidths(
        wb,
        sheet = "Data",
        cols = 20:29,
        widths = 1,         
        hidden = TRUE       
    )

    openxlsx::setColWidths(
        wb,
        sheet = "Data",
        cols = 39:43,
        widths = 1,         
        hidden = TRUE       
    )

    xlxs_filename <- paste0(path_to_dashboard_data_hRSV, "/Data_hRSV_",
                   gsub(":", "-", sub(" CEST", "", Sys.time())),
                   ".xlsx")
    openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)

    .microbs_env$df_new_dashboard_hRSV_data <- df_final_combined
    .microbs_env$df_new_dashboard_hRSV_data
}


#--------------------------------------------------------------------------------------------------------
# Export the SARS-CoV files for dashboard
#--------------------------------------------------------------------------------------------------------
#' @title Do the export of SARS-CoV for dashboard
#' 
#' @description The creation step export the calculated data 
#' for export to be used on the dashboard for each virus. 
#' In this funcion we handle all the excel files in the created data directory
#' 
#' 
#' file ./microbs.lu/R/08_dashBoard_export.R
#' 
#' @examples
#' # Example usage
#' 
#' @export

dashboard_microbs_SARS_CoV_export <- function(sheet1_SARS_CoV = .microbs_env$sheet1_sars, sheet2_SARS_CoV = .microbs_env$sheet2_sars, path_to_create_data_ddPCR= .microbs_env$created_data_path){
    if (is.null(path_to_create_data_ddPCR)) {
        message("[microbs Warning]: Did not yet access the loaded data. Use the set_microbs_created_DataPath(),")
        message("and then load_microbs_old_raw_ddPCR_Data() function to set a path")
    }

    path_to_dashboard_data_SARS_CoV <- get_microbs_dashboard_DataPath()

    df_nat <- get_microbs_new_create_sars_sheet1()
    df_wwtp <- get_microbs_new_create_sars_sheet2()

    df_old <- get_microbs_old_dashboard_sars_Data()
    df_old_trimmed <- df_old %>%
                        dplyr::filter(`yyyy-w (SARS-CoV)` <= "2024_52")
    df_old_trimmed <- df_old_trimmed %>% dplyr::select(!starts_with(c("SAMPLES", "SITUATION")))

    df_samples <- df_old %>% dplyr::select(starts_with(c("SAMPLES", "SITUATION")))
    df_situation <- df_old %>% dplyr::select(starts_with("SITUATION"))
    df_situation <- df_situation[1,]
    df_samples <- df_samples[1,]

    # National level
    df_nat <- df_nat %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0)))
    df_nat_long <- df_nat %>%
                        dplyr::select(week_nb, copies_days_inhab_qPCR) %>%
                        dplyr::rename(`SARS-CoV-Nat` = copies_days_inhab_qPCR) %>%
                        dplyr::mutate(`Mov-SARS-CoV-Nat` = zoo::na.locf(
                            zoo::rollmean(`SARS-CoV-Nat`, k = 3, fill = NA, align = "right"),
                            na.rm = FALSE
                            )
                        )

    # Per WWTP
    df_wwtp_wide <- df_wwtp %>%
                        dplyr::select(week_nb, WWTP, copies_days_inhab_qPCR) %>%
                        tidyr::pivot_wider(
                            names_from = WWTP,
                            values_from = copies_days_inhab_qPCR,
                            names_prefix = "SARS-CoV-"
                        ) %>%
                        dplyr::arrange(week_nb)        

    df_wwtp_mov <- df_wwtp_wide %>%
                        dplyr::mutate(dplyr::across(dplyr::starts_with("SARS-CoV-"),
                                                    ~ zoo::na.locf(zoo::rollmean(.x, k = 3, fill = NA, align = "right"), na.rm = FALSE),
                                                    .names = "Mov-{.col}"))

    # Combine all
    df_final <- df_nat_long %>%
                    dplyr::left_join(df_wwtp_mov, by = "week_nb") %>%
                    dplyr::rename(`yyyy-w (SARS-CoV)` = week_nb) %>%
                    dplyr::arrange(`yyyy-w (SARS-CoV)`)

    df_final_trimmed <- df_final %>%
                            dplyr::filter(`yyyy-w (SARS-CoV)` > "2024_52")

    df_final_combined <- dplyr::bind_rows(df_old_trimmed, df_final_trimmed) %>%
                            dplyr::arrange(`yyyy-w (SARS-CoV)`)
    
    # recompute the rolling mean for the final combined data
    sar_cols <- grep("^SARS-CoV-", names(df_final_combined), value = TRUE)
    mov_sar_cols <- paste0("Mov-", sar_cols)
    sample_cols <- sub("^SARS-CoV-", "SAMPLES-", sar_cols)

    for (i in seq_along(sar_cols)) {
        df_final_combined[[mov_sar_cols[i]]] <- zoo::na.locf(zoo::rollmean(df_final_combined[[sar_cols[i]]], 
                                                        k = 3, 
                                                        fill = NA, 
                                                        align = "right"), na.rm = FALSE)
    }

    # df_final_combined[df_final_combined == 0] <- NA
    
    # Initialize an empty list to store sample counts
    sample_counts <- list()

    # Count non-NA values for each SARS-CoV column and store in corresponding SAMPLES- column
    for (i in seq_along(sar_cols)) {
        sample_counts[[sample_cols[i]]][1] <- sum(!is.na(df_final_combined[[sar_cols[i]]]))
    }

    # Create a one-row data.frame from the sample counts
    df_samples_new <- as.data.frame(sample_counts, optional = TRUE)
    total_samples <- 0
    for (col in names(df_samples_new)) {
        if (col == "SAMPLES-Nat") {
            next
        } else {
            total_samples <- total_samples + df_samples_new[[col]][1]
        }
    }
    df_samples_new$`SAMPLES-Nat`[1] <- total_samples

    # df_final_combined <- df_final_combined %>% dplyr::bind_cols(df_samples)
            
    # styles
    sci_style <- openxlsx::createStyle(numFmt = "0.00E+00")
    header_st <- openxlsx::createStyle(textDecoration = "Bold")

    n_cols_main <- ncol(df_final_combined)
    n_rows_main <- nrow(df_final_combined)

    # write the new data into the file
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Data")
    
    # write the data
    openxlsx::writeData(wb, "Data", df_final_combined, startCol = 1, startRow = 1, headerStyle = header_st)

    # write the samples
    openxlsx::writeData(
        wb,
        sheet = "Data",
        x = df_samples_new,
        startCol = n_cols_main + 1,
        startRow = 1,
        headerStyle = header_st
    )

    n_cols_samples <- ncol(df_samples_new)
    openxlsx::writeData(
        wb,
        sheet = "Data",
        x = df_situation,
        startCol = n_cols_main + n_cols_samples + 1,
        startRow = 1,
        headerStyle = header_st
    )
        
    numeric_cols <- which(sapply(df_final_combined, is.numeric))
    for (col in numeric_cols) {
        openxlsx::addStyle(
            wb,
            sheet = "Data",
            style = sci_style,
            rows = 2:(n_rows_main + 1),  # +1 to account for header
            cols = col,
            gridExpand = TRUE,
            stack = TRUE
        )
    }
    
    openxlsx::freezePane(wb, sheet = "Data", firstRow = TRUE)

    # Hide columns
    openxlsx::setColWidths(
        wb,
        sheet = "Data",
        cols = 20:29,
        widths = 1,         
        hidden = TRUE       
    )

    # openxlsx::setColWidths(
    #     wb,
    #     sheet = "Data",
    #     cols = 39:43,
    #     widths = 1,         
    #     hidden = TRUE       
    # )

    xlxs_filename <- paste0(path_to_dashboard_data_SARS_CoV, "/Data_SARCoV_",
                   gsub(":", "-", sub(" CEST", "", Sys.time())),
                   ".xlsx")
    openxlsx::saveWorkbook(wb, xlxs_filename, overwrite = TRUE)

    .microbs_env$df_new_dashboard_SARS_CoV_data <- df_final_combined
    .microbs_env$df_new_dashboard_SARS_CoV_data
}
#--------------------------------------------------------------------------------------------------------
# get Loaded ddPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get old raw loaded data for ddPCR
#' 
#' @description If it returns NULL, it means that the old data has not been loaded yet.
#' Call load_microbs_old_raw_ddPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `1_loaded_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_old_raw_ddPCR_Data()
#' result
#' }
#' 
#' @export 
get_microbs_old_raw_ddPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_raw_ddPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_raw_ddPCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_raw_ddPCR_Data()")
    }
    .microbs_env$df_old_raw_ddPCR_data
}

#--------------------------------------------------------------------------------------------------------
# get Loaded ddPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get new raw loaded data for ddPCR
#' 
#' @description If it returns NULL, it means that the new data has not been loaded yet.
#' Call load_microbs_old_raw_ddPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the new loaded RAW data located in `1_loaded_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_raw_ddPCR_Data()
#' result
#' }
#' 
#' @export 
get_microbs_new_raw_ddPCR_Data <- function() {
    if (is.null(.microbs_env$df_new_raw_ddPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_raw_ddPCR_Data() function to set a path")
        message("or use the default by using load_microbs_raw_ddPCR_Data()")
    }
    .microbs_env$df_new_raw_ddPCR_data
}


#--------------------------------------------------------------------------------------------------------
# get Loaded qPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get old raw loaded data for qPCR
#' 
#' @description If it returns NULL, it means that the old data has not been loaded yet.
#' Call load_microbs_old_raw_qPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `1_loaded_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_old_raw_qPCR_Data()
#' result
#' }
#' 
#' @export 
get_microbs_old_raw_qPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_raw_qPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_raw_qPCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_raw_qPCR_Data()")
    }
    .microbs_env$df_old_raw_qPCR_data
}

#--------------------------------------------------------------------------------------------------------
# get Loaded qPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get new raw loaded data for qPCR
#' 
#' @description If it returns NULL, it means that the new data has not been loaded yet.
#' Call load_microbs_old_raw_qPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the new loaded RAW data located in `1_loaded_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_raw_qPCR_Data()
#' result
#' }
#' 
#' @export 
get_microbs_new_raw_qPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_raw_qPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_raw_qPCR_Data() function to set a path")
        message("or use the default by using load_microbs_raw_qPCR_Data()")
    }
    .microbs_env$df_new_raw_qPCR_data
}

#--------------------------------------------------------------------------------------------------------
# get checked ddPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get old calc data for ddPCR
#' 
#' @description If it returns NULL, it means that the old check data has not been loaded yet.
#' Call load_microbs_old_calc_ddPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `1_ckeck_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_old_check_ddPCR_Data()
#' result
#' }
#' 
#' @export 
get_microbs_old_check_ddPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_check_ddPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_check_ddPCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_check_ddPCR_Data()")
    }
    .microbs_env$df_old_check_ddPCR_data
}

#--------------------------------------------------------------------------------------------------------
# get checked qPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get old raw check data for qPCR
#' 
#' @description If it returns NULL, it means that the old calc data has not been loaded yet.
#' Call load_microbs_old_calc_qPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `1_ckeck_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_old_check_qPCR_Data()
#' result
#' }
#' 
#' @export 
get_microbs_old_check_qPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_check_qPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_check_1PCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_check_qPCR_Data()")
    }
    .microbs_env$df_old_check_qPCR_data
}


#--------------------------------------------------------------------------------------------------------
# get old calc ddPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get old calc data for ddPCR
#' 
#' @description If it returns NULL, it means that the old check data has not been loaded yet.
#' Call load_microbs_old_calc_ddPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `2_calc_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_old_calc_ddPCR_Data()
#' result
#' }
#' 
#' @export 
get_microbs_old_calc_ddPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_calc_ddPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_calc_ddPCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_calc_ddPCR_Data()")
    }
    .microbs_env$df_old_calc_ddPCR_data
}

#--------------------------------------------------------------------------------------------------------
# get new calc ddPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get new calc data for ddPCR
#' 
#' @description If it returns NULL, it means that the new check data has not been loaded yet.
#' Call load_microbs_new_calc_ddPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the new loaded RAW data located in `2_calc_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_calc_ddPCR_Data()
#' result
#' }
#' 
#' @export 
get_microbs_new_calc_ddPCR_Data <- function() {
    if (is.null(.microbs_env$df_new_calc_ddPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_new_calc_ddPCR_Data() function to set a path")
        message("or use the default by using load_microbs_new_calc_ddPCR_Data()")
    }
    .microbs_env$df_new_calc_ddPCR_data
}


#--------------------------------------------------------------------------------------------------------
# get new flu created sheet
#--------------------------------------------------------------------------------------------------------
#' @title get new created data for fku
#' 
#' @description If it returns NULL, it means that the new create data has not been run yet.
#' Call create_microbs_flu_file() to load the data.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_create_flu_sheet1()
#' result
#' }
#' 
#' @export 
get_microbs_new_create_flu_sheet1 <- function() {
    if (is.null(.microbs_env$sheet1_flu)) {
        message("[microbs Warning]: Use the create_microbs_flu_file() function to create flu data")
    }
    .microbs_env$sheet1_flu
}

#--------------------------------------------------------------------------------------------------------
# get new flu created sheet
#--------------------------------------------------------------------------------------------------------
#' @title get new created data for fku
#' 
#' @description If it returns NULL, it means that the new create data has not been run yet.
#' Call create_microbs_flu_file() to load the data.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_create_flu_sheet2()
#' result
#' }
#' 
#' @export 
get_microbs_new_create_flu_sheet2 <- function() {
    if (is.null(.microbs_env$sheet2_flu)) {
        message("[microbs Warning]: Use the create_microbs_flu_file() function to create flu data")
    }
    .microbs_env$sheet2_flu
}

#--------------------------------------------------------------------------------------------------------
# get new flu created sheet
#--------------------------------------------------------------------------------------------------------
#' @title get new created data for fku
#' 
#' @description If it returns NULL, it means that the new create data has not been run yet.
#' Call create_microbs_flu_file() to load the data.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_create_flu_sheet3()
#' result
#' }
#' 
#' @export 
get_microbs_new_create_flu_sheet3 <- function() {
    if (is.null(.microbs_env$sheet3_flu)) {
        message("[microbs Warning]: Use the create_microbs_flu_file() function to create flu data")
    }
    .microbs_env$sheet3_flu
}

#--------------------------------------------------------------------------------------------------------
# get new flu created sheet
#--------------------------------------------------------------------------------------------------------
#' @title get new created data for fku
#' 
#' @description If it returns NULL, it means that the new create data has not been run yet.
#' Call create_microbs_flu_file() to load the data.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_create_flu_sheet4()
#' result
#' }
#' 
#' @export 
get_microbs_new_create_flu_sheet4 <- function() {
    if (is.null(.microbs_env$sheet4_flu)) {
        message("[microbs Warning]: Use the create_microbs_flu_file() function to create flu data")
    }
    .microbs_env$sheet4_flu
}

#--------------------------------------------------------------------------------------------------------
# get new hRSV created sheet
#--------------------------------------------------------------------------------------------------------
#' @title get new created data for fku
#' 
#' @description If it returns NULL, it means that the new create data has not been run yet.
#' Call create_microbs_hRSV_file() to load the data.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_create_hRSV_sheet1()
#' result
#' }
#' 
#' @export 
get_microbs_new_create_hRSV_sheet1 <- function() {
    if (is.null(.microbs_env$sheet1_hRSV)) {
        message("[microbs Warning]: Use the create_microbs_hRSV_file() function to create hRSV data")
    }
    .microbs_env$sheet1_hRSV
}

#--------------------------------------------------------------------------------------------------------
# get new hRSV created sheet
#--------------------------------------------------------------------------------------------------------
#' @title get new created data for fku
#' 
#' @description If it returns NULL, it means that the new create data has not been run yet.
#' Call create_microbs_hRSV_file() to load the data.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_create_hRSV_sheet2()
#' result
#' }
#' 
#' @export 
get_microbs_new_create_hRSV_sheet2 <- function() {
    if (is.null(.microbs_env$sheet2_hRSV)) {
        message("[microbs Warning]: Use the create_microbs_hRSV_file() function to create hRSV data")
    }
    .microbs_env$sheet2_hRSV
}

#--------------------------------------------------------------------------------------------------------
# get new hRSV created sheet
#--------------------------------------------------------------------------------------------------------
#' @title get new created data for fku
#' 
#' @description If it returns NULL, it means that the new create data has not been run yet.
#' Call create_microbs_hRSV_file() to load the data.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_create_hRSV_sheet3()
#' result
#' }
#' 
#' @export 
get_microbs_new_create_hRSV_sheet3 <- function() {
    if (is.null(.microbs_env$sheet3_hRSV)) {
        message("[microbs Warning]: Use the create_microbs_hRSV_file() function to create hRSV data")
    }
    .microbs_env$sheet3_hRSV
}

#--------------------------------------------------------------------------------------------------------
# get new hRSV created sheet
#--------------------------------------------------------------------------------------------------------
#' @title get new created data for fku
#' 
#' @description If it returns NULL, it means that the new create data has not been run yet.
#' Call create_microbs_hRSV_file() to load the data.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_create_hRSV_sheet4()
#' result
#' }
#' 
#' @export 
get_microbs_new_create_hRSV_sheet4 <- function() {
    if (is.null(.microbs_env$sheet4_hRSV)) {
        message("[microbs Warning]: Use the create_microbs_hRSV_file() function to create hRSV data")
    }
    .microbs_env$sheet4_hRSV
}

#--------------------------------------------------------------------------------------------------------
# get new sars created sheet
#--------------------------------------------------------------------------------------------------------
#' @title get new created data for fku
#' 
#' @description If it returns NULL, it means that the new create data has not been run yet.
#' Call create_microbs_sars_file() to load the data.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_create_sars_sheet1()
#' result
#' }
#' 
#' @export 
get_microbs_new_create_sars_sheet1 <- function() {
    if (is.null(.microbs_env$sheet1_sars)) {
        message("[microbs Warning]: Use the create_microbs_sars_file() function to create sars data")
    }
    .microbs_env$sheet1_sars
}

#--------------------------------------------------------------------------------------------------------
# get new sars created sheet
#--------------------------------------------------------------------------------------------------------
#' @title get new created data for fku
#' 
#' @description If it returns NULL, it means that the new create data has not been run yet.
#' Call create_microbs_sars_file() to load the data.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_create_sars_sheet2()
#' result
#' }
#' 
#' @export 
get_microbs_new_create_sars_sheet2 <- function() {
    if (is.null(.microbs_env$sheet2_sars)) {
        message("[microbs Warning]: Use the create_microbs_sars_file() function to create sars data")
    }
    .microbs_env$sheet2_sars
}

#--------------------------------------------------------------------------------------------------------
# get new sars created sheet
#--------------------------------------------------------------------------------------------------------
#' @title get new created data for fku
#' 
#' @description If it returns NULL, it means that the new create data has not been run yet.
#' Call create_microbs_sars_file() to load the data.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_create_sars_sheet3()
#' result
#' }
#' 
#' @export 
get_microbs_new_create_sars_sheet3 <- function() {
    if (is.null(.microbs_env$sheet3_sars)) {
        message("[microbs Warning]: Use the create_microbs_sars_file() function to create sars data")
    }
    .microbs_env$sheet3_sars
}

#--------------------------------------------------------------------------------------------------------
# get new sars created sheet
#--------------------------------------------------------------------------------------------------------
#' @title get new created data for fku
#' 
#' @description If it returns NULL, it means that the new create data has not been run yet.
#' Call create_microbs_sars_file() to load the data.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_create_sars_sheet4()
#' result
#' }
#' 
#' @export 
get_microbs_new_create_sars_sheet4 <- function() {
    if (is.null(.microbs_env$sheet4_sars)) {
        message("[microbs Warning]: Use the create_microbs_sars_file() function to create sars data")
    }
    .microbs_env$sheet4_sars
}


#--------------------------------------------------------------------------------------------------------
# get old calc qPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get old raw check data for qPCR
#' 
#' @description If it returns NULL, it means that the old calc data has not been loaded yet.
#' Call load_microbs_old_calc_qPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `2_calc_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_old_calc_qPCR_Data()
#' result
#' }
#' 
#' @export 
get_microbs_old_calc_qPCR_Data <- function() {
    if (is.null(.microbs_env$df_old_calc_qPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_old_calc_qPCR_Data() function to set a path")
        message("or use the default by using load_microbs_old_calc_qPCR_Data()")
    }
    .microbs_env$df_old_calc_qPCR_data
}

#--------------------------------------------------------------------------------------------------------
# get new calc qPCR data
#--------------------------------------------------------------------------------------------------------
#' @title get new raw check data for qPCR
#' 
#' @description If it returns NULL, it means that the new calc data has not been loaded yet.
#' Call load_microbs_new_calc_qPCR_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the new loaded RAW data located in `2_calc_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_calc_qPCR_Data()
#' result
#' }
#' 
#' @export 
get_microbs_new_calc_qPCR_Data <- function() {
    if (is.null(.microbs_env$df_new_calc_qPCR_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_new_calc_qPCR_Data() function to set a path")
        message("or use the default by using load_microbs_new_calc_qPCR_Data()")
    }
    .microbs_env$df_new_calc_qPCR_data
}

#--------------------------------------------------------------------------------------------------------
# get flux data
#--------------------------------------------------------------------------------------------------------
#' @title get the flux data
#' 
#' @description If it returns NULL, it means that the old calc data has not been loaded yet.
#' Call load_microbs_flux_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old loaded RAW data located in `2_calc_data`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_flux_Data()
#' result
#' }
#' 
#' @export 
get_microbs_flux_Data <- function() {
    if (is.null(.microbs_env$df_flux_data)) {
        message("[microbs Warning]: Did not yet set the path. Use the load_microbs_flux_Data() function to set a path")
        message("or use the default by using load_microbs_flux_Data()")
    }
    .microbs_env$df_flux_data
}


#--------------------------------------------------------------------------------------------------------
# get old flu dashboard data
#--------------------------------------------------------------------------------------------------------
#' @title get the flux data
#' 
#' @description If it returns NULL, it means that the old dashboard data has not been loaded yet.
#' Call load_microbs_old_dashboard_flu_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old dashbpard data located in `4_data_4_dashboard`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_old_dashboard_flu_Data()
#' result
#' }
#' 
#' @export 
get_microbs_old_dashboard_flu_Data <- function() {
    if (is.null(.microbs_env$df_old_dashboard_flu_data)) {
        message("[microbs Warning]: Did not yet load the data. Use the load_microbs_old_dashboard_flu_Data() function to load the old data")
    }
    .microbs_env$df_old_dashboard_flu_data
}

#--------------------------------------------------------------------------------------------------------
# get old hRSV dashboard data
#--------------------------------------------------------------------------------------------------------
#' @title get the hRSVx data
#' 
#' @description If it returns NULL, it means that the old dashboard data has not been loaded yet.
#' Call load_microbs_old_dashboard_hRSV_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old dashbpard data located in `4_data_4_dashboard`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_old_dashboard_hRSV_Data()
#' result
#' }
#' 
#' @export 
get_microbs_old_dashboard_hRSV_Data <- function() {
    if (is.null(.microbs_env$df_old_dashboard_hRSV_data)) {
        message("[microbs Warning]: Did not yet load the data. Use the load_microbs_old_dashboard_hRSV_Data() function to load the old data")
    }
    .microbs_env$df_old_dashboard_hRSV_data
}

#--------------------------------------------------------------------------------------------------------
# get old sars dashboard data
#--------------------------------------------------------------------------------------------------------
#' @title get the sarsx data
#' 
#' @description If it returns NULL, it means that the old dashboard data has not been loaded yet.
#' Call load_microbs_old_dashboard_sars_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the old dashbpard data located in `4_data_4_dashboard`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_old_dashboard_sars_Data()
#' result
#' }
#' 
#' @export 
get_microbs_old_dashboard_sars_Data <- function() {
    if (is.null(.microbs_env$df_old_dashboard_sars_data)) {
        message("[microbs Warning]: Did not yet load the data. Use the load_microbs_old_dashboard_sars_Data() function to load the old data")
    }
    .microbs_env$df_old_dashboard_sars_data
}


#--------------------------------------------------------------------------------------------------------
# get new flu dashboard data
#--------------------------------------------------------------------------------------------------------
#' @title get the flux data
#' 
#' @description If it returns NULL, it means that the new dashboard data has not been loaded yet.
#' Call load_microbs_new_dashboard_flu_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the new dashbpard data located in `4_data_4_dashboard`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_dashboard_flu_Data()
#' result
#' }
#' 
#' @export 
get_microbs_new_dashboard_flu_Data <- function() {
    if (is.null(.microbs_env$df_new_dashboard_flu_data)) {
        message("[microbs Warning]: Did not yet load the data. Use the load_microbs_new_dashboard_flu_Data() function to load the new data")
    }
    .microbs_env$df_new_dashboard_flu_data
}

#--------------------------------------------------------------------------------------------------------
# get new hRSV dashboard data
#--------------------------------------------------------------------------------------------------------
#' @title get the hRSVx data
#' 
#' @description If it returns NULL, it means that the new dashboard data has not been loaded yet.
#' Call load_microbs_new_dashboard_hRSV_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the new dashbpard data located in `4_data_4_dashboard`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_dashboard_hRSV_Data()
#' result
#' }
#' 
#' @export 
get_microbs_new_dashboard_hRSV_Data <- function() {
    if (is.null(.microbs_env$df_new_dashboard_hRSV_data)) {
        message("[microbs Warning]: Did not yet load the data. Use the load_microbs_new_dashboard_hRSV_Data() function to load the new data")
    }
    .microbs_env$df_new_dashboard_hRSV_data
}

#--------------------------------------------------------------------------------------------------------
# get new sars dashboard data
#--------------------------------------------------------------------------------------------------------
#' @title get the sarsx data
#' 
#' @description If it returns NULL, it means that the new dashboard data has not been loaded yet.
#' Call load_microbs_new_dashboard_sars_Data() to load the data.
#' Need to set relevant paths.
#' Preferably use full path.
#' This function allows you to load the new dashbpard data located in `4_data_4_dashboard`.
#' 
#' file ./microbs.lu/R/03_DataGetters.R
#' 
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_microbs_new_dashboard_sars_Data()
#' result
#' }
#' 
#' @export 
get_microbs_new_dashboard_sars_Data <- function() {
    if (is.null(.microbs_env$df_new_dashboard_sars_data)) {
        message("[microbs Warning]: Did not yet load the data. Use the load_microbs_new_dashboard_sars_Data() function to load the new data")
    }
    .microbs_env$df_new_dashboard_sars_data
}
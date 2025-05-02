test_that("getting the NULL path for loaded Raw data as it is not set", {

    .microbs_env <- new.env(parent = emptyenv())
    .microbs_env$loaded_data_path <- NULL
    
    true_path <- NULL

    test_path <- get_microbs_loaded_DataPath()

    expect_equal(true_path, test_path)
})

test_that("getting the default path for loaded Raw data, as it is set by default", {
    true_path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/1_loaded_data"
    assign("qPCR_path", set_microbs_loaded_DataPath(), envir = .GlobalEnv)
    
    test_path <- get_microbs_loaded_DataPath()

    expect_equal(true_path, test_path)
    rm("qPCR_path", envir = .GlobalEnv)
})




test_that("setting the custom local path for loaded Raw data", {
    # Set the global variables as they would be set by other functions
    assign("wd", set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"), envir = .GlobalEnv)
    assign("path_connector", set_microbs_connector_dir(), envir = .GlobalEnv)

    test_local_path <- "1_loaded_data"
    true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"

    set_path <- set_microbs_loaded_DataPath(test_local_path, build_path = TRUE)
    test_path <- get_microbs_loaded_DataPath()

    # Ensure that the function correctly sets the working directory
    expect_equal(true_path, test_path)

    # Clean up the global variables
    rm("wd", envir = .GlobalEnv)
    rm("path_connector", envir = .GlobalEnv)
})

test_that("setting the relative path for loaded Raw data", {
    withr::with_dir("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment",
    {
        true_path <- "./1_loaded_data"

        test_local_path <- "1_loaded_data"

        set_path <- set_microbs_loaded_DataPath(test_local_path,  relative = TRUE)
        test_path <- get_microbs_loaded_DataPath()

        # Ensure that the function correctly sets the working directory
        expect_equal(true_path, test_path)
    })
})
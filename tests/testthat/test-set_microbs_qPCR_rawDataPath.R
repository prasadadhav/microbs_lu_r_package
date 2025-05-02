test_that("setting the default path for raw qPCR data", {
    true_path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data_Treatment/0_raw_data_qPCR"

    test_path <- set_microbs_qPCR_rawDataPath()

    # Ensure that the function correctly sets the working directory
    expect_equal(true_path, test_path)
})


test_that("setting the custom local path for raw qPCR data", {
    # Set the global variables as they would be set by other functions
    assign("wd", set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"), envir = .GlobalEnv)
    assign("path_connector", set_microbs_connector_dir(), envir = .GlobalEnv)

    test_local_path <- "0_raw_data_qPCR"
    true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_qPCR"

    test_path <- set_microbs_qPCR_rawDataPath(test_local_path)

    # Ensure that the function correctly sets the working directory
    expect_equal(true_path, test_path)

    # Clean up the global variables
    rm("wd", envir = .GlobalEnv)
    rm("path_connector", envir = .GlobalEnv)
})


test_that("setting the relative path for raw qPCR data", {
    withr::with_dir("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment",
    {
        true_path <- "./0_raw_data_qPCR"

        test_local_path <- "0_raw_data_qPCR"

        test_path <- set_microbs_qPCR_rawDataPath(test_local_path,  relative = TRUE)

        # Ensure that the function correctly sets the working directory
        expect_equal(true_path, test_path)
    })
})
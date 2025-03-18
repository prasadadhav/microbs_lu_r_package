test_that("setting the default path for raw ddPCR data", {
    true_path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results/Data Treatment/0_raw_data_ddPCR"

    test_path <- set_microbs_ddPCR_rawDataPath()

    # Ensure that the function correctly sets the working directory
    expect_equal(true_path, test_path)
})


test_that("setting the custom local path for raw ddPCR data", {
    # Set the global variables as they would be set by other functions
    assign("wd", "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data", envir = .GlobalEnv)
    assign("path_connector", "Data Treatment", envir = .GlobalEnv)

    test_local_path <- "0_raw_data_ddPCR"
    true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data Treatment/0_raw_data_ddPCR"

    test_path <- set_microbs_ddPCR_rawDataPath(test_local_path)

    # Ensure that the function correctly sets the working directory
    expect_equal(true_path, test_path)

    # Clean up the global variables
    rm("wd", envir = .GlobalEnv)
    rm("path_connector", envir = .GlobalEnv)
})


test_that("setting the relative path for raw ddPCR data", {
    assign("wd", "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data", envir = .GlobalEnv)
    assign("path_connector", "Data Treatment", envir = .GlobalEnv)

    true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data Treatment/0_raw_data_ddPCR"

    test_local_path <- "0_raw_data_ddPCR"

    test_path <- set_microbs_ddPCR_rawDataPath(test_local_path)

    # Ensure that the function correctly sets the working directory
    expect_equal(true_path, test_path)

    # Clean up the global variables
    rm("wd", envir = .GlobalEnv)
    rm("path_connector", envir = .GlobalEnv)
})
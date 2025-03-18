test_that("using path builder for qPCR raw data", {
  test_global_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"
  test_connnector <- "Data_Treatment"
  test_local_path <- "0_raw_data_qPCR"

  test_path <- utils_microbs_path_builder(test_global_path, test_connnector, test_local_path)

  true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_qPCR"

  # Ensure that the function correctly sets the working directory
  expect_equal(true_path, test_path)
})

test_that("using path builder for ddPCR raw data", {
  test_global_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"
  test_connnector <- "Data_Treatment"
  test_local_path <- "0_raw_data_ddPCR"

  test_path <- utils_microbs_path_builder(test_global_path, test_connnector, test_local_path)

  true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/0_raw_data_ddPCR"

  # Ensure that the function correctly sets the working directory
  expect_equal(true_path, test_path)
})

test_that("using path builder for loaded data", {
  test_global_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"
  test_connnector <- "Data_Treatment"
  test_local_path <- "1_loaded_data"

  test_path <- utils_microbs_path_builder(test_global_path, test_connnector, test_local_path)

  true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/1_loaded_data"

  # Ensure that the function correctly sets the working directory
  expect_equal(true_path, test_path)
})

test_that("using path builder for calculated data", {
  test_global_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"
  test_connnector <- "Data_Treatment"
  test_local_path <- "2_calc_data"

  test_path <- utils_microbs_path_builder(test_global_path, test_connnector, test_local_path)

  true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/2_calc_data"

  # Ensure that the function correctly sets the working directory
  expect_equal(true_path, test_path)
})

test_that("using path builder for created data", {
  test_global_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"
  test_connnector <- "Data_Treatment"
  test_local_path <- "3_created_data"

  test_path <- utils_microbs_path_builder(test_global_path, test_connnector, test_local_path)

  true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/3_created_data"

  # Ensure that the function correctly sets the working directory
  expect_equal(true_path, test_path)
})

test_that("using path builder for data processed for dashboard", {
  test_global_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"
  test_connnector <- "Data_Treatment"
  test_local_path <- "4_data_4_dashboard"

  test_path <- utils_microbs_path_builder(test_global_path, test_connnector, test_local_path)

  true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data_Treatment/4_data_4_dashboard"

  # Ensure that the function correctly sets the working directory
  expect_equal(true_path, test_path)
})
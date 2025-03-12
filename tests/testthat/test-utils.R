test_that("using path builder works well", {
  test_global_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"
  test_connnector <- "Data Treatment"
  test_local_path <- "0_raw_data_qPCR"

  test_path <- utils_microbs_path_builder(test_global_path, test_connnector, test_local_path)

  true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data Treatment/0_raw_data_qPCR"

  # Ensure that the function correctly sets the working directory
  expect_equal(true_path, test_path)
})

test_that("using path builder works well", {
  test_global_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"
  test_connnector <- "Data Treatment"
  test_local_path <- "0_raw_data_ddPCR"

  test_path <- utils_microbs_path_builder(test_global_path, test_connnector, test_local_path)

  true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data Treatment/0_raw_data_ddPCR"

  # Ensure that the function correctly sets the working directory
  expect_equal(true_path, test_path)
})

test_that("using path builder works well", {
  test_global_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"
  test_connnector <- "Data Treatment"
  test_local_path <- "1_loaded_data"

  test_path <- utils_microbs_path_builder(test_global_path, test_connnector, test_local_path)

  true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data Treatment/1_loaded_data"

  # Ensure that the function correctly sets the working directory
  expect_equal(true_path, test_path)
})

test_that("using path builder works well", {
  test_global_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"
  test_connnector <- "Data Treatment"
  test_local_path <- "2_calc_data"

  test_path <- utils_microbs_path_builder(test_global_path, test_connnector, test_local_path)

  true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data Treatment/2_calc_data"

  # Ensure that the function correctly sets the working directory
  expect_equal(true_path, test_path)
})

test_that("using path builder works well", {
  test_global_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"
  test_connnector <- "Data Treatment"
  test_local_path <- "3_created_data"

  test_path <- utils_microbs_path_builder(test_global_path, test_connnector, test_local_path)

  true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data Treatment/3_created_data"

  # Ensure that the function correctly sets the working directory
  expect_equal(true_path, test_path)
})

test_that("using path builder works well", {
  test_global_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"
  test_connnector <- "Data Treatment"
  test_local_path <- "4_data_4_dashboard"

  test_path <- utils_microbs_path_builder(test_global_path, test_connnector, test_local_path)

  true_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data/Data Treatment/4_data_4_dashboard"

  # Ensure that the function correctly sets the working directory
  expect_equal(true_path, test_path)
})
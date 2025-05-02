test_that("set_microbs_wdirectory works correctly with double backslashes", {
  test_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data" # use double \\

  expect_message(set_microbs_wdirectory("D:\\03_Workspace\\01_R_Package\\microbs_lu_dummy_data"), 
  "Report: Using provided path")

  # Ensure that the function correctly sets the working directory
  expect_equal(getwd(), test_path)
})

test_that("set_microbs_wdirectory works correctly with custom path", {
  test_path <- "D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"  # use a local path

  expect_message(set_microbs_wdirectory("D:/03_Workspace/01_R_Package/microbs_lu_dummy_data"), 
  "Report: Using provided path")

  # Ensure that the function correctly sets the working directory
  expect_equal(getwd(), test_path)
})

test_that("set_microbs_wdirectory works correctly", {
  test_path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results"

  expect_message(set_microbs_wdirectory("L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results"), 
  "Report: Using provided path")

  # Ensure that the function correctly sets the working directory
  expect_equal(getwd(), test_path)
})

test_that("set_microbs_wdirectory works correctly", {
  test_path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results"  # provide default directory

  set_microbs_wdirectory()

  # Ensure that the function correctly sets the working directory
  expect_equal(getwd(), test_path)
})

test_that("set_microbs_wdirectory handles non-existent paths", {
  non_existent_path <- "/path/to/nonexistent/directory"
  
  expect_error(set_microbs_wdirectory(non_existent_path), 
               regexp = "cannot change working directory")
})

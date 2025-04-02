test_that("getting the NULL path for global working directory as it is not set", {
    rm(list = ls(envir = .microbs_env), envir = .microbs_env)
    .microbs_env <- new.env(parent = emptyenv())
    .microbs_env$wd <- NULL
    
    true_path <- NULL

    test_path <- get_microbs_wdirectory()

    expect_equal(true_path, test_path)
})

test_that("getting the default path for global working directory, as it is set by default", {
    true_path <- "L:/Units & Programmes/BIOTECH/ENVMICRO/_Common/Projects/SUPERVIR/11-Results"
    assign("wd", set_microbs_wdirectory(), envir = .GlobalEnv)
    
    test_path <- get_microbs_wdirectory()

    expect_equal(true_path, test_path)
    rm("wd", envir = .GlobalEnv)
})
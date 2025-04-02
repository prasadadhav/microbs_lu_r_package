test_that("getting the NULL path for raw Data Treatment connector as it is not set", {
    rm(list = ls(envir = .microbs_env), envir = .microbs_env)
    .microbs_env <- new.env(parent = emptyenv())
    .microbs_env$path_connector <- NULL
    
    true_path <- NULL

    test_path <- get_microbs_connector_dir()

    expect_equal(true_path, test_path)
})

test_that("getting the default path for raw Data Treatment connector, as it is set by default", {
    true_path <- "Data_Treatment"
    assign("connector_path", set_microbs_connector_dir(), envir = .GlobalEnv)
    
    test_path <- get_microbs_connector_dir()

    expect_equal(true_path, test_path)
    rm("connector_path", envir = .GlobalEnv)
})
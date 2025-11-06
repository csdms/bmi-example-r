## test-bmi-control.R

config_file <- system.file("extdata", "heat_config.yaml", package = "bmiheatr")

test_that("BMI initialize requires config file", {
  model <- BmiHeat$new()
  expect_error(model$bmi_initialize())
})

test_that("BMI finalize works without errors", {
  model <- BmiHeat$new()
  model$bmi_initialize(config_file)
  expect_silent(model$bmi_finalize())
})

test_that("BMI update advances model time", {
  model <- BmiHeat$new()
  model$bmi_initialize(config_file)

  time_before <- model$get_current_time()
  model$update()
  time_after <- model$get_current_time()

  expect_gt(time_after, time_before)

  model$bmi_finalize()
})

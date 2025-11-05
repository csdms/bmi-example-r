## test-time.R

test_that("BMI time functions give expected values", {
  model <- BmiHeat$new()
  
  config_file <- system.file("extdata", "heat_config.yaml", package = "bmiheatr")
  model$bmi_initialize(config_file)

  current_time <- model$get_current_time()
  expect_equal(current_time, 0.0)
  start_time <- model$get_start_time()
  expect_equal(start_time, 0.0)
  end_time <- model$get_end_time()
  expect_equal(end_time, Inf)
  time_units <- model$get_time_units()
  expect_equal(time_units, "s")
  time_step <- model$get_time_step()
  expect_equal(time_step, 0.25)

  model$bmi_finalize()
})

## test-bmi-value.R

test_that("BMI get/set value functions behave as expected", {
  model <- BmiHeat$new()

  config_file <- system.file("extdata", "heat_config.yaml", package = "bmiheatr")
  model$bmi_initialize(config_file)

  val1 <- model$get_value("plate_surface__temperature")
  val2 <- model$get_value("plate_surface__temperature")
  expect_equal(val1, val2)

  src = 1:48
  model$set_value("plate_surface__temperature", src)
  val3 <- model$get_value("plate_surface__temperature")
  expect_equal(val3, src)

  model$bmi_finalize()
})

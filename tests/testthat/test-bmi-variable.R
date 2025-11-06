## test-bmi-variable.R

var_name <- "plate_surface__temperature"

test_that("BMI variable functions give expected values", {
  model <- BmiHeat$new()
  
  config_file <- system.file("extdata", "heat_config.yaml", package = "bmiheatr")
  model$bmi_initialize(config_file)

  grid_id <- model$get_var_grid(var_name)
  expect_equal(grid_id, 0L)
  var_type <- model$get_var_type(var_name)
  expect_equal(var_type, "double")
  var_units <- model$get_var_units(var_name)
  expect_equal(var_units, "K")
  var_itemsize <- model$get_var_itemsize(var_name)
  expect_equal(var_itemsize, 8L)
  var_nbytes <- model$get_var_nbytes(var_name)
  expect_equal(var_nbytes, 384L)
  var_location <- model$get_var_location(var_name)
  expect_equal(var_location, "node")

  model$bmi_finalize()
})

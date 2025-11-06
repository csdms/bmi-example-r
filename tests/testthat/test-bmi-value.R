## test-bmi-value.R

config_file <- system.file("extdata", "heat_config.yaml", package = "bmiheatr")
var_name <- "plate_surface__temperature"
var_size <- 48  # 6 rows x 8 cols

test_that("BMI get_value returns correct size", {
  model <- BmiHeat$new()
  model$bmi_initialize(config_file)

  val <- model$get_value(var_name)
  expect_equal(length(val), var_size)

  model$bmi_finalize()
})

test_that("BMI get_value returns a vector", {
  model <- BmiHeat$new()
  model$bmi_initialize(config_file)

  val <- model$get_value(var_name)
  expect_true(is.vector(val))

  model$bmi_finalize()
})

test_that("BMI get_value returns consistent results", {
  model <- BmiHeat$new()
  model$bmi_initialize(config_file)

  val1 <- model$get_value(var_name)
  val2 <- model$get_value(var_name)
  expect_equal(val1, val2)

  model$bmi_finalize()
})

test_that("BMI set_value works as expected", {
  model <- BmiHeat$new()
  model$bmi_initialize(config_file)

  src <- 1:var_size
  model$set_value(var_name, src)
  val <- model$get_value(var_name)
  expect_equal(val, src)

  model$bmi_finalize()
})

test_that("BMI get_value_at_indices works as expected", {
  model <- BmiHeat$new()
  model$bmi_initialize(config_file)

  inds <- c(7, 20, 34)
  val_full <- model$get_value(var_name)
  val_at_inds <- model$get_value_at_indices(var_name, inds)
  expect_equal(val_at_inds, val_full[inds])

  model$bmi_finalize()
})

test_that("BMI set_value_at_indices works as expected", {
  model <- BmiHeat$new()
  model$bmi_initialize(config_file)

  src <- c(100, 200, 300)
  inds <- c(17, 30, 24)

  model$set_value_at_indices(var_name, inds, src)
  val <- model$get_value(var_name)
  expect_equal(val[inds], src)

  model$bmi_finalize()
})

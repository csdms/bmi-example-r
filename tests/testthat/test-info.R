## test-info.R

test_that("BMI info functions give expected values", {
  model <- BmiHeat$new()
  
  config_file <- system.file("extdata", "heat_config.yaml", package = "bmiheatr")
  model$bmi_initialize(config_file)

  component_name <- model$get_component_name()
  expect_equal(component_name, "The 2D Heat Equation")
  input_count <- model$get_input_item_count()
  expect_equal(input_count, 1)
  output_count <- model$get_output_item_count()
  expect_equal(output_count, 1)
  input_names <- model$get_input_var_names()
  expect_equal(input_names, c("plate_surface__temperature"))
  output_names <- model$get_output_var_names()
  expect_equal(output_names, c("plate_surface__temperature"))

  model$bmi_finalize()
})

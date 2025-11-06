## test-bmi-grid.R

grid_id <- 0

test_that("BMI grid functions give expected values", {
  model <- BmiHeat$new()
  
  config_file <- system.file("extdata", "heat_config.yaml", package = "bmiheatr")
  model$bmi_initialize(config_file)

  grid_rank <- model$get_grid_rank(grid_id)
  expect_equal(grid_rank, 2L)
  grid_size <- model$get_grid_size(grid_id)
  expect_equal(grid_size, 48L)
  grid_type <- model$get_grid_type(grid_id)
  expect_equal(grid_type, "uniform_rectilinear")
  grid_shape <- model$get_grid_shape(grid_id)
  expect_equal(grid_shape, c(6L, 8L))
  grid_spacing <- model$get_grid_spacing(grid_id)
  expect_equal(grid_spacing, c(1.0, 1.0))
  grid_origin <- model$get_grid_origin(grid_id)
  expect_equal(grid_origin, c(0.0, 0.0))
  grid_node_count <- model$get_grid_node_count(grid_id)
  expect_equal(grid_node_count, 48L)

  model$bmi_finalize()
})

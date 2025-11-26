#
# Example code for calling the get_grid_shape BMI method.
#
library(devtools)
library(bmir)
load_all()

x <- BmiHeat$new()

config_file <- system.file("extdata", "heat_config.yaml", package = "bmiheatr")
x$bmi_initialize(config_file)
print(x$get_component_name())

grid_id <- x$get_var_grid("plate_surface__temperature")
rank <- x$get_grid_rank(grid_id)
shape <- vector(mode = "integer", length = rank)
the_shape <- x$get_grid_shape(grid_id, shape)

cat("shape:", shape, fill = TRUE)
cat("the_shape:", the_shape, fill = TRUE)

#
# Example code for running the Heat model through its BMI.
#
library(devtools)
library(bmir)
load_all()

var_name = "plate_surface__temperature"
var_size = 48
inds <- c(7, 20, 34)

x <- BmiHeat$new()

config_file <- system.file("extdata", "heat_config.yaml", package = "bmiheatr")
x$bmi_initialize(config_file)
print(x$get_component_name())

val1 <- x$get_value(var_name)
x$update()
val2 <- x$get_value(var_name)
expect_true(sum(val2) != sum(val1))

x$bmi_finalize()

#
# Example code for calling the BMI getter/setter methods.
#
library(devtools)
library(bmir)
library(pryr)
load_all()

var_name = "plate_surface__temperature"
var_size = 48
inds <- c(7, 20, 34)
src_at_inds <- c(100, 200, 300)

x <- BmiHeat$new()

config_file <- system.file("extdata", "heat_config.yaml", package = "bmiheatr")
x$bmi_initialize(config_file)
print(x$get_component_name())

val1 <- x$get_value(var_name)
val2 <- x$get_value(var_name)
expect_equal(val1, val2)

mem0 <- mem_used()
val <- x$get_value(var_name)
mem1 <- mem_used()
cat("Memory before get_value call:", mem0, fill = TRUE)
cat("Memory after get_value call:", mem1, fill = TRUE)
cat("Difference:", mem1 - mem0, fill = TRUE)
cat("Size of returned value:", object_size(val), fill = TRUE)

src1 = 1:var_size
x$set_value(var_name, src1)
val3 <- x$get_value(var_name)
expect_equal(val3, src1)

x$set_value_at_indices(var_name, inds, src_at_inds)
val <- x$get_value(var_name)
expect_equal(val[inds], src_at_inds)
val_at_inds1 <- x$get_value_at_indices(var_name, inds)
expect_equal(val_at_inds1, src_at_inds)

src2 <- matrix(1, nrow = 6, ncol = 8)
src2[1,2] <- src_at_inds[1]
src2[2,4] <- src_at_inds[2]
src2[4,6] <- src_at_inds[3]
src2_flat <- as.vector(src2)
x$set_value(var_name, src2_flat)
val_at_inds2 <- x$get_value_at_indices(var_name, inds)
expect_equal(val_at_inds2, src_at_inds)

x$bmi_finalize()

# Notes

A temporary place to store my notes while building the `bmiheatr` package.

Load my in-progress work into an R session.
```R
library(devtools, bmir)
load_all()
```

Make a new instance of `BmiHeat`.
```R
x = BmiHeat$new()
```

Call the (renamed) BMI `initialize` function.
```R
config_file = "./inst/extdata/heat_config.yaml"
x$bmi_initialize(config_file)
```

Call BMI methods.
```R
x$get_component_name()
x$get_current_time()
grid_id <- x$get_var_grid("plate_surface__temperature")
```

Try a helper method.
```R
x$display_model()
```

Experiment with flattening R matrices.
```R
temperature = x$get_value_ptr("plate_surface__temperature")
temperature_flat = vector(mode = "double", length = 6*8)
temperature_flat[] <- as.vector(temperature)
```

Need to import `bmir` into `bmiheatr` package.
This will be done with roxygen2 when I add doc code headers to the `BmiHeat` class.

Does `get_value_ptr` work correctly?
What about flattening its output?

I've chosen to do negligible error handling.

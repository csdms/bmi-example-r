
BmiHeat <- R6::R6Class(
    "BmiHeat",
    inherit = bmir::Bmi,

    private = list(
        model = NULL,
        values = list(),
        var_units = list(),
        var_loc = list(),
        grids = list(),
        grid_type = list(),
        input_var_names = c("plate_surface__temperature"),
        output_var_names = c("plate_surface__temperature"),
        component_name = "The 2D Heat Equation",
        start_time = 0.0,
        end_time = Inf,
        time_units = "s"
    ),

    public = list(
        initialize = function() {
            private$model <- NULL
            private$values <- list()
            private$var_units <- list()
            private$var_loc <- list()
            private$grids <- list()
            private$grid_type <- list()
        },

        bmi_initialize = function(config_file = NULL) {
            private$model <- Heat$from_yaml_file(config_file)
            private$values <- list(plate_surface__temperature = private$model$temperature)
            private$var_units <- list(plate_surface__temperature = "K")
            private$var_loc <- list(plate_surface__temperature = "node")
            private$grids <- list(`0` = c("plate_surface__temperature"))
            private$grid_type <- list(`0` = "uniform_rectilinear")
            invisible(NULL)
        },

        update = function() {
            private$model$advance_in_time()
            invisible(NULL)
        },

        bmi_finalize = function() {
            private$model <- NULL
            invisible(NULL)
        },

        get_component_name = function() {
            private$component_name
        },

        get_input_item_count = function() {
            length(private$input_var_names)
        },

        get_output_item_count = function() {
            length(private$output_var_names)
        },

        get_input_var_names = function() {
            private$input_var_names
        },

        get_output_var_names = function() {
            private$output_var_names
        },

        get_var_grid = function(name) {
            for (grid_id in names(private$grids)) {
                if (name %in% private$grids[[grid_id]]) {
                    return(as.integer(grid_id))
                }
            }
            return(NULL)
        },

        get_var_type = function(name) stop("Not implemented"),

        get_var_units = function(name) {
            private$var_units[[name]]
        },

        get_var_itemsize = function(name) stop("Not implemented"),

        get_var_nbytes = function(name) stop("Not implemented"),

        get_var_location = function(name) {
            private$var_loc[[name]]
        },

        get_current_time = function() {
            private$model$time
        },

        get_start_time = function() {
            private$start_time
        },

        get_end_time = function() {
            private$end_time
        },

        get_time_units = function() {
            private$time_units
        },

        get_time_step = function() {
            private$model$time_step
        },

        get_value = function(name, dest) {
            value <- self$get_value_ptr(name)
            dest[] <- as.vector(value)
            return(invisible(dest))
        },

        get_value_at_indices = function(name, inds, dest) stop("Not implemented"),

        get_value_ptr = function(name) {
            private$values[[name]]
        },

        set_value = function(name, src) stop("Not implemented"),
        set_value_at_indices = function(name, inds, src) stop("Not implemented"),

        get_grid_rank = function(grid) stop("Not implemented"),
        get_grid_size = function(grid) stop("Not implemented"),
        get_grid_type = function(grid) stop("Not implemented"),
        get_grid_shape = function(grid, shape) stop("Not implemented"),
        get_grid_spacing = function(grid, spacing) stop("Not implemented"),
        get_grid_origin = function(grid, origin) stop("Not implemented"),
        get_grid_node_count = function(grid) stop("Not implemented"),


        # A non-BMI helper method to expose the underlying model.
        display_model = function() {
            private$model
        }
    )
)

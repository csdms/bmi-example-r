
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

        get_var_type = function(name) {
            typeof(private$values[[name]])
        },

        get_var_units = function(name) {
            private$var_units[[name]]
        },

        get_var_itemsize = function(name) {
            switch(typeof(private$values[[name]]),
                   integer = 4L,
                   double = 8L,
                   stop("Unsupported variable type", call. = FALSE))
        },

        get_var_nbytes = function(name) {
            length(private$values[[name]]) * self$get_var_itemsize(name)
        },

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

        get_value = function(name, dest = NULL) {
            return(invisible(as.vector(private$values[[name]])))
        },

        get_value_at_indices = function(name, inds, dest) stop("Not implemented"),

        set_value = function(name, src) {
            dims = private$model$shape
            private$values[[name]] <- matrix(src, nrow = dims[1], ncol = dims[2])
            invisible(NULL)
        },

        set_value_at_indices = function(name, inds, src) stop("Not implemented"),

        get_grid_rank = function(grid) {
            length(private$model$shape)
        },

        get_grid_size = function(grid) {
            prod(private$model$shape)
        },

        get_grid_type = function(grid) {
            private$grid_type[[as.character(grid)]]
        },

        get_grid_shape = function(grid, shape = NULL) {
            private$model$shape
        },

        get_grid_spacing = function(grid, spacing = NULL) {
            private$model$spacing
        },

        get_grid_origin = function(grid, origin = NULL) {
            private$model$origin
        },

        get_grid_node_count = function(grid) {
            self$get_grid_size(grid)
        },

        #
        # Non-BMI convenience methods
        #

        get_value_as_matrix = function(name) {
            private$values[[name]]
        },

        to_matrix = function(src) {
            dims = private$model$shape
            matrix(src, nrow = dims[1], ncol = dims[2])
        },

        display_model = function() {
            private$model
        }
    )
)

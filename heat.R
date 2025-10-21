library(R6)
library(yaml)

#' Solve the 2D Heat Equation on a uniform mesh using base R functions.
#'
#' @param temp A matrix representing the temperature.
#' @param spacing A numeric vector of length 2 (dy, dx) for grid spacing.
#' @param alpha Thermal diffusivity (default is 1.0).
#' @param time_step Time step (default is 1.0).
#'
#' @return A matrix with the temperatures after time *time_step*.
#'
#' @examples
#' z0 <- matrix(0, nrow=5, ncol=5)
#' z0[3, 3] <- 100.0
#' solve_2d(z0, spacing=c(1.0, 1.0), alpha=0.25)
#' #      [,1] [,2] [,3] [,4] [,5]
#' # [1,]    0  0.0  0.0  0.0    0
#' # [2,]    0  0.0 12.5  0.0    0
#' # [3,]    0 12.5 50.0 12.5    0
#' # [4,]    0  0.0 12.5  0.0    0
#' # [5,]    0  0.0  0.0  0.0    0
solve_2d <- function(temp, spacing, alpha=1.0, time_step=1.0) {

  n_rows <- nrow(temp)
  n_cols <- ncol(temp)
  if (n_rows < 3 || n_cols < 3) {
    return(temp)
  }

  # R's matrix indexing is (row, col), so spacing[1] is dy
  dy2 <- spacing[1]^2
  dx2 <- spacing[2]^2

  # Define kernel coefficients
  k_up <- dy2
  k_down <- dy2
  k_left <- dx2
  k_right <- dx2
  k_center <- -2.0 * (dx2 + dy2)

  # Get "shifted" views of the interior, noting that R indexing is 1-based
  T_C <- temp[2:(n_rows-1), 2:(n_cols-1)] # Center
  T_U <- temp[1:(n_rows-2), 2:(n_cols-1)] # Up
  T_D <- temp[3:n_rows, 2:(n_cols-1)]       # Down
  T_L <- temp[2:(n_rows-1), 1:(n_cols-2)] # Left
  T_R <- temp[2:(n_rows-1), 3:n_cols]       # Right

  # Calculate the convolved interior
  convolved_interior <- (
    k_center * T_C +
    k_up * T_U +
    k_down * T_D +
    k_left * T_L +
    k_right * T_R
  )

  # Calculate the diffusion constant K
  K <- alpha * time_step / (2.0 * (dx2 * dy2))

  # Calculate the change in temperature
  delta_temp <- matrix(0.0, nrow=n_rows, ncol=n_cols)
  delta_temp[2:(n_rows-1), 2:(n_cols-1)] <- K * convolved_interior

  # Add the change to the original temperature
  result <- temp + delta_temp

  return(result)
}


#' Heat model
#'
#' Models temperature diffusion on a two-dimensional uniform grid.
#'
#' @details
#' Based on the Python version of the model in \url{https://github.com/csdms/bmi-example-python}.
#' 
#' @examples
#' heat <- Heat$new()
#' heat$time
#' # [1] 0.0
#' heat$time_step
#' # [1] 0.25
#' heat$advance_in_time()
#' heat$time
#' # [1] 0.25
#' 
#' heat <- Heat$new(shape=c(5, 5))
#' new_temp <- matrix(0, 5, 5)
#' new_temp[3, 3] <- 100.0
#' heat$temperature <- new_temp
#' heat$advance_in_time()
#' heat$temperature
#' #      [,1] [,2] [,3] [,4] [,5]
#' # [1,]    0  0.0  0.0  0.0    0
#' # [2,]    0  0.0 12.5  0.0    0
#' # [3,]    0 12.5 50.0 12.5    0
#' # [4,]    0  0.0 12.5  0.0    0
#' # [5,]    0  0.0  0.0  0.0    0
#' 
#' heat <- Heat$new(alpha=.5)
#' heat$time_step
#' # [1] 0.5
#' heat <- Heat$new(alpha=.5, spacing=c(2., 3.))
#' heat$time_step
#' # [1] 2
Heat <- R6Class("Heat",
  private = list(
    .shape = NULL,
    .spacing = NULL,
    .origin = NULL,
    .time = 0.0,
    .alpha = 1.0,
    .time_step = NULL,
    .temperature = NULL,
    .next_temperature = NULL
  ),
  
  public = list(
    #' Create a new Heat model.
    #'
    #' @param shape Numeric vector c(rows, cols).
    #' @param spacing Numeric vector c(dy, dx).
    #' @param origin Numeric vector c(x0, y0).
    #' @param alpha Numeric, thermal diffusivity.
    initialize = function(shape=c(10, 20), spacing=c(1.0, 1.0),
                          origin=c(0.0, 0.0), alpha=1.0) {
      private$.shape <- shape
      private$.spacing <- spacing
      private$.origin <- origin
      private$.time <- 0.0
      private$.alpha <- alpha

      private$.time_step <- min(spacing)^2 / (4.0 * private$.alpha)

      # Initialize temperature and buffer grids, noting matrix() takes (nrow, ncol)
      private$.temperature <- matrix(runif(shape[1] * shape[2]),
                                     nrow = shape[1],
                                     ncol = shape[2])
      private$.next_temperature <- matrix(NA,
                                         nrow = shape[1],
                                         ncol = shape[2])
    },
    
    #' Calculate new temperatures for the next time step.
    advance_in_time = function() {
      private$.next_temperature <- solve_2d(
        temp=private$.temperature,
        spacing=private$.spacing,
        alpha=private$.alpha,
        time_step=private$.time_step
      )

      private$.temperature <- private$.next_temperature
      private$.time <- private$.time + private$.time_step
    }
  ),
  
  active = list(
    #' @field time Current model time (read-only).
    time = function() {
      return(private$.time)
    },
    
    #' @field temperature The temperature matrix (read/write).
    temperature = function(new_temp) {
      if (missing(new_temp)) {
        return(private$.temperature)
      } else {
        private$.temperature <- new_temp
      }
    },
    
    #' @field time_step Model time step (read/write).
    time_step = function(time_step) {
      if (missing(time_step)) {
        return(private$.time_step)
      } else {
        private$.time_step <- time_step
      }
    },
    
    #' @field shape Shape of the model grid (read-only).
    shape = function() {
      return(private$.shape)
    },
    
    #' @field spacing Spacing of the model grid (read-only).
    spacing = function() {
      return(private$.spacing)
    },
    
    #' @field origin Origin coordinates of the model grid (read-only).
    origin = function() {
      return(private$.origin)
    }
  )
)

#' Create a Heat object from a file-like object (static method)
#'
#' @param file_like A file path or connection to a YAML config file.
#' @return A new instance of a Heat object.
Heat$set("public", "from_file_like",
  function(file_like) {
    config <- yaml::read_yaml(file_like)
    do.call(self$new, config)
  },
  overwrite = TRUE
)

# In the solution below, we only check arguments in the main "user facing" function.
# The other functions are called within this function and will usually
# not be user facing, therefore its sufficient to check the inputs once, instead
# of performing the same checks for each function.

### Calculate intercept and slope

### This function finds the intercept (a) and slope (b) of the line through a
# scatter plot that minimizes the quadratic difference between the line and the
#  observed values of y.
### Arguments:
### x: a vector. The x-coordinate of the points.
### y: a vector: The y-coordinate of the points.
### Returns: A vector of length two. The first entry is the intercept
### and the second one the slope of the "optimal" line.
line_solution <- function(x, y) {

  X      <- cbind(1, x)
  Xt     <- t(X)
  XtX    <- Xt %*% X
  invXtX <- solve(XtX)
  Xty    <- Xt %*% y

  unname(invXtX %*% Xty)

}

### Calculate prediction
###
### This function returns the value of the line on the y-axis, given  x and
### the intercept and slope of the line.
### Arguments:
### x: a vector. The x-coordinate of the points.
### a: A numeric of length 1. The intercept.
### b: A numeric of length 1. The slope.
### Returns: A vector of same length as x. This are the values of the line at
###           x-coordinates provided by vector x.
line_prediction <- function(x, a, b) {

  a + b * x

}

### Calculate the sum of squared differences between line and points.
###
### Given a line in the x/y coordinate system, this functions calculates the
### squared difference between the line and the y values
### (at corresponding x-values used to calculate the prediction).
###
### Arguments:
### y: A numeric vector: The y-coordinate of the points.
### x: A numeric vector. Same length as y.
### Returns: A numeric vector of length 1. The calculated error sum of squared differences
line_error <- function(y, prediction) {

  sum((prediction - y)^2)

}

### Line estimator
###
### Given x and y, this function calculates the optimal line (intercept and slope),
### the resulting prediction for y and the error (sum of squared differences)
### between line and y.
###
### Arguments:
### data: A data frame.
### x_name: A character of length 1. The name of the variable in data that stores
###         information about the x-coordinates of the points.
### y_name: A character of length 1. The name of the variable in data that stores
###         information about the y-coordinates of the points.
### Returns:
###         - A named list of length 3.
###           - The first entry (solution) contains the intercept and slope
###             (numeric vector of length 2).
###           - The second entry (prediction) contains the prediction
###             (numeric vector of same length as rows in the data set)
###           - The third entry (error) contains the sum of squared differences
###             (numeric vector of length 1)
line_estimator <- function(data, x_name, y_name) {

  stopifnot(is.data.frame(data))
  stopifnot(is.character(x_name) & length(x_name) == 1L)
  stopifnot(is.character(y_name) & length(y_name) == 1L)

  nrows_data <- nrow(data)
  if (nrows_data < 3L) {
    stop("Not enough observations in data to estimate line.")
  }
  if(x_name == y_name) {
    stop("x_name and y_name must be different.")
  }

  if(!(x_name %in% colnames(data))) {
    stop(paste0(x_name, " is not a variable in 'data'."))
  }
  if(!(y_name %in% colnames(data))) {
    stop(paste0(y_name, " is not a variable in 'data'."))
  }

  x <- data[[x_name]]
  if (!is.numeric(x)) stop(paste0("Variable ", x_name, " is not numeric."))
  y <- data[[y_name]]
    if (!is.numeric(y)) stop(paste0("Variable ", y_name, " is not numeric."))

  solution   <- line_solution(x, y)
  prediction <- line_prediction(x, a = solution[1], b = solution[2])
  error      <- line_error(y, prediction)

  if (nrows_data < 10L) {
    warning("The data set has fewer than 10 observations.
      Results might be unreliable")
  }
  list(solution = solution, prediction = prediction, error = error)

}

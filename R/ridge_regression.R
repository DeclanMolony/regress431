#' asdf
#'
#' @param
#'
ridge_regression_coefs <- function(x, y, lambda){

  results <- as.data.frame(t(solve(t(x) %*% x + lambda*diag(ncol(x))) %*% (t(x) %*% y)))

  return(results)

}



#' Implements ridge regression with many predictors
#'
#' This function computes coefficients for ridge regression
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import purrr
#'
#' @export
ridge_regression <- function(dat, response, lambda) {

  x <- dat %>% dplyr::select(-{{response}})
  y <- as.matrix(dat %>% dplyr::pull({{response}}))

  x <- scale(x)
  x <- as.matrix(cbind(1, x))

  results <- purrr::map_dfr(lambda, ~ridge_regression_coefs(x,y,.x))

  results <- results %>%
    dplyr::rename("Intercept" = 1)
  results <- cbind(results,lambda)


  ### This should be a data frame, with columns named
  ### "Intercept" and the same variable names as dat, and also a column
  ### called "lambda".

  return(results)

}

#' asdfsdf
#'
#' @param
#'
find_best_lambda_helper <- function(x,y,y_test,lambda) {

  coefs <- as.data.frame(t(solve(t(x) %*% x + lambda*diag(ncol(x))) %*% (t(x) %*% y)))

  coefs2 <- purrr::map_dfr(1:nrow(x), ~coefs[1,]) #repeat the coefficients the number of rows in x

  combined <- x * coefs2 #multiply the explanatory variables by their corresponding coefficients
  combined <- data.frame(rowSums(combined)) #find the row sums, or predicted values of each row
  combined <- cbind(y_test,combined)
  combined <- combined %>%
    dplyr::rename("predicted_value" = rowSums.combined.)
  combined <- combined %>%
    mutate(diff = ((y_test - predicted_value)^2)) %>%
    summarise(error = sum(diff))

  combined <- as.data.frame(cbind(lambda,combined))

  return(combined)

}

#' Determines the best penalty term from a set of options
#'
#' This function uses a randomly chosen test and training set
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of penalty terms and resulting errors
#'
#' @import dplyr
#'
#' @export
find_best_lambda <- function(train_dat, test_dat, response, lambda) {

  x <- train_dat %>% dplyr::select(-{{response}})
  y <- as.matrix(train_dat %>% dplyr::pull({{response}}))
  y_test <- as.matrix(test_dat %>% dplyr::pull({{response}}))

  x <- scale(x)
  x <- as.matrix(cbind(1, x))


  ### lambda_errors should be a data frame with two columns: "lambda" and "error"
  ### For each lambda, you should record the resulting Sum of Squared error
  ### (i.e., the predicted value minus the real value squared) from prediction
  ### on the test dataset. ie: sum((y-hat - yi)^2)

  return(lambda_errors)
}

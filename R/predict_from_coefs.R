#' Computes predicted values given coefficients
#'
#' This function takes a data frame of coefficients in the form outputted by
#' functions like \code{multiple_linear_regression} or \code{ridge_regression}.
#'
#' It calculates the predicted values of a response variable from these coefficients.
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param coefs A data frame of coefficient estimates
#'
#' @return A data frame of true and predicted values
#'
#' @import dplyr
#' @import purrr
#'
#' @export
predict_from_coefs <- function(dat, response, coefs){

  x <- dat %>% dplyr::select(-{{response}}) #explanatory variables
  true_value <- as.matrix(dat %>% dplyr::pull({{response}})) #response/true value will be compared to the predicted value

  x <- as.matrix(cbind(1, x)) #set up a ones matrix with explanatory variables

  coefs2 <- purrr::map_dfr(1:nrow(x), ~coefs[1,]) #repeat the coefficients the number of rows in x

  combined <- round(x * coefs2, 1) #multiply the explanatory variables by their corresponding coefficients
  combined <- data.frame(rowSums(combined)) #find the row sums, or predicted values of each row
  combined <- cbind(true_value,combined) #combine the dataframes

  combined <- combined %>%
    dplyr::rename("predicted_value" = rowSums.combined.)

  return(combined)

}

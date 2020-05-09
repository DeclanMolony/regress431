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

  x <- dat %>% dplyr::select(-{{response}})
  true_value <- as.matrix(dat %>% dplyr::pull({{response}}))

  x <- as.matrix(cbind(1, x))

  coefs2 <- purrr::map_dfr(1:nrow(x), ~coefs[1,])

  combined <- round(x * coefs2, 1)
  combined <- data.frame(rowSums(combined))
  combined <- cbind(true_value,combined)
  combined <- combined %>%
    dplyr::rename("predicted_value" = rowSums.combined.)

  return(combined)

}

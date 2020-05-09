#' Implements simple linear regression by hand
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
simple_linear_regression <- function(dat, response, explanatory){

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()

  ### Edit code after here

  x_matrix <- as.matrix(cbind(1,x))

  A <- solve(t(x_matrix) %*% x_matrix) %*% (t(x_matrix) %*% y)

  beta_0 <- A[1]
  beta_1 <- A[2]

  ### Stop editing

  results <- tibble::tibble(
    Intercept = beta_0,
    Slope = beta_1
  )

  names(results)[2] <- explan_name

  return(results)

}


#' Implements linear regression with many predictors by hand
#'
#' This function computes coefficients for multiple regression by hand.
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
multiple_linear_regression <- function(dat, response) {

  x <- dat %>% dplyr::select(-{{response}})
  y <- as.matrix(dat %>% dplyr::pull({{response}}))

  x <- as.matrix(cbind(1, x))

  results <- as.data.frame(t(solve(t(x) %*% x) %*% (t(x) %*% y)))
  results <- results %>%
    dplyr::rename("Intercept" = 1) ### This should be a data frame, with columns named
                                   ### "Intercept", and the same variable names as dat.

  return(results)

}







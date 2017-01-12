#' datafy
#'
#' Generate a data set
#'
#' This function simulates a data set for use in simulations of model
#' development, validation or updating.
#'
#' Currently supports the generation of correlated continuous, normally distributed
#' predictor variables and a binary outcome.
#' @importsFrom MASS mvrnorm
#'
#' @param obs        Number of observations in the data set.
#' @param means      A vector of predictor means.
#' @param covmat     A covariance matrix (see example).
#' @param var.names  A string of variable names (optional).
#' @param genmod     Vector of parameter values for a logistic regression model
#'                   used in the generation of binary outcomes.
#'
#' @return \code{datafy} returns a dataframe with m predictors and a binary outcome
#' @export
#'
#' @examples
#' ## Example 1: Generation of an untreated development set
#' covmat <- matrix(c(0.2,0,0,0.2), nrow=2)
#' d <- datafy(obs = 100, means = c(0,0), covmat = covmat,
#'      var.names = c("X1", "X2", "Y"), genmod = c(-1.5, 1, 1))

datafy <- function(obs, means, covmat, var.names, genmod) {

  if(missing(covmat)) stop("Please specify a corrlation matrix (cormat)")

  dmat <- cbind(1, mvrnorm(n = obs, mu = means, Sigma = covmat))

  LP0 <- dmat %*% genmod
  dY <- rbinom(obs, 1, 1 / (1 + exp(- LP0)))
  dmat <- data.frame(cbind(dmat[,-1], dY))
  names(dmat) <- var.names
  return(dmat)
}

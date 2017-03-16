#' coxregmulti
#'
#' Return a formatted output of a multiple cox regression model.
#'
#' @param mod a multiple cox proportional hazards model fit by \code{coxph}.
#' @return prints a table with hazard ratios, confidence intervals and p-values.
#' @author Jutta Gamper
#' @export

coxregmulti <- function(mod){

  hr <- summary(mod)$conf.int[,-2]
  pv <- summary(mod)$coefficients[,5]
  res <- cbind(hr, pv)
  res <- round(res, 4)
  res
}

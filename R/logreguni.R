#' logreguni
#'
#' Calculate simple logistic regression models and return a formatted output.
#'
#' @param x a data frame containing the independent variables.
#' @param y the response variable, typically 0 or 1.
#' @return prints tables with coefficients and confidence intervals.
#' @author Jutta Gamper
#' @keywords htest
#' @export
#'


logreguni <- function(x, y){
  covars <- x
  res.log <- matrix(ncol=4, nrow=ncol(covars))
  for(i in 1:ncol(covars)){
    res.log[i,] <- c(exp(summary(glm(y~covars[,i], family="binomial"))$coef[2,1]),
                     exp(confint(glm(y~covars[,i], family="binomial"))[2,]),
                     summary(glm(y~covars[,i], family="binomial"))$coef[2,4])
  }
  rownames(res.log) <- colnames(covars)
  colnames(res.log) <- c("OR", "lower CI", "upper CI", "p-value")
  res <- list("res"=res.log)
  invisible(res)
}

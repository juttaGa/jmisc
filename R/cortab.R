#' cortab
#'
#' Calculate correlation coefficients of many x with one y using the function \code{cor.test}.
#'
#' \code{cor.test} is used for calculation and a table with estimates, confidence intervals and p-values is returned.
#'
#' @param x a data frame.
#' @param y the response variable.
#' @param method one of "pearson", "kendall" or "spearman".
#' @param ... other parameters passed through.
#' @return Prints table with correlation coefficients, confidence intervals and p-values.
#' @author Jutta Gamper
#' @export


cortab <- function(x, y, method=c("pearson", "kendall", "spearman"), ...){
  tab <- matrix(ncol=4, nrow=ncol(x))
  for(i in 1:ncol(x)){
    xx <- cor.test(as.numeric(x[,i]), y, method=method, ...)
    tab[i,] <- cbind(round(xx$estimate,2), round(xx$conf.int[1],2), round(xx$conf.int[2],2), round(xx$p.value,4))
  }
  colnames(tab) <- c("R", "lower CI", "upper CI", "p-value")
  rownames(tab) <- colnames(x)
  tab
}

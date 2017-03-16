#' cortab
#'
#' Calculate Pearson correlation coefficients and return a table containing estimates and p-values.
#'
#' @param x a data frame.
#' @param y the response variable.
#' @return prints tables with correlation coefficients and p-values.
#' @author Jutta Gamper
#' @export
#'

cortab <- function(x,y){
  tab <- matrix(ncol=2, nrow=ncol(x))
  for(i in 1:ncol(x)){
    xx <- cor.test(as.numeric(x[,i]), y)
    tab[i,] <- cbind(xx$estimate, xx$p.value)
  }
  colnames(tab) <- c("R", "p-value")
  rownames(tab) <- colnames(x)
  tab
}

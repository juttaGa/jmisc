#' ttest
#'
#' Calculate t-tests and return a table containing estimates and p-values.
#'
#' @param x a data frame.
#' @param y the response variable.
#' @return prints tables with means, standard deviations and p-values.
#' @author Jutta Gamper
#' @export



ttest <- function(x,y){
  tab <- matrix(ncol=3, nrow=ncol(x))
  for(i in 1:ncol(x)){
    xx <- t.test(x[,i]~y)
    tab[i,] <- cbind(paste(round(xx$estimate[1],2), "(", round(sd(x[y==0,i], na.rm=T), 2), ")"),
                     paste(round(xx$estimate[2],2), "(", round(sd(x[y==1,i], na.rm=T), 2), ")"), round(xx$p.value, 4))
  }
  colnames(tab) <- c("Group 1", "Group 2", "p-value")
  rownames(tab) <- colnames(x)
  tab
}

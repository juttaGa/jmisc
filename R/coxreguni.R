#' coxreguni
#'
#' Calculate simple cox regression models and return a formatted output.
#'
#' @param event binary indicator of event, typically 0 or 1.
#' @param time follow up time
#' @param x a data frame containing the independent variable(s) for cox the regression.
#' @param etype type of event for cause specific hazard models (only for competing risks).
#' @return prints tables with hazard ratios and confidence intervals.
#' @author Jutta Gamper
#' @keywords htest
#' @import survival
#' @export
#'
#'


coxreguni <- function(event, time, x, etype=NULL){

  if(!is.null(etype))
    survobj <- Surv(time, event==etype)
  else
    survobj <- Surv(time, event)
  if(is.data.frame(x))
    pval <- numeric(ncol(x))
  else if(is.numeric(x))
    pval <- numeric(1)
  else if(is.factor(x))
    pval <- numeric(nlevels(x)-1)

  res <- new.env()
  N <- ifelse(is.data.frame(x), ncol(x), 1)
  for(i in 1:N){
    res.cox <- coxph(survobj ~ x[,i])
    eval(parse( text = paste("cox.",names(x)[i], "<-res.cox", sep = "")))
    val <- summary(res.cox)$conf.int[,-2]

    if(is.vector(val) == 1){
      val <- t(as.matrix(val))
      rownames(val) <- substring(names(res.cox$coefficients), first = 8)
      val <- cbind(res.cox$n, res.cox$nevent, val, summary(res.cox)$ coefficients[,5])
    }
    else {
      rownames(val) <- substring(names(res.cox$coefficients), first = 8)
      val <- cbind(rep(res.cox$n, times = nrow(summary(res.cox)$ coefficients)), rep(res.cox$nevent, times = nrow(summary(res.cox)$ coefficients)), val, summary(res.cox)$ coefficients[,5])
    }

    eval(parse( text = paste("res$",names(x)[i], "<-val", sep = "")))
    pval[i] <- summary(res.cox)$loglik[3]
  }

  res <- as.list(res)
  res2 <- NULL

  for(i in 1:N){
    res2 <- rbind(res2, res[[i]])
  }
  #rownames(res2) <- colnames(x)
  colnames(res2) <- c("N.obs", "N.event", "HR", "lower CI", "upper CI", "p-value")
  invisible(res2)
}

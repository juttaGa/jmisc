#' sumNum
#'
#' Summarise all numerical variables in a \code{data.frame}.
#'
#' @param data a data frame or matrix.
#' @param grp possible factor to group by, defaults to NULL. Maximum are two groups.
#' @param rd round results to \code{rd} digits, default is 2.
#' @return prints a summary table of number of observations (N), number of missing values (miss), minimum, maximum, median,
#' mean and standard deviation; possibly separated by grp.
#' @author Jutta Gamper
#' @export
#' @examples
#'
#' sumNum(iris)
#' sumNum(iris, 5)
#' sumNum(iris, 5, 4)

sumNum <- function(data, grp=NULL, rd=2){

  if(is.matrix(data)) data <- as.data.frame(data)
  num <- data %>% select_if(is.numeric)

  if(length(grp)>2) stop("Maximum number of factors to group by is 2!")

  if(is.null(grp)){
    sum <- num %>% summarise_each(funs(N=length(which(!is.na(.))), miss=length(which(is.na(.))), Minimum=min(., na.rm=T), Q1=quantile(., na.rm=F)[2],
                                       Median=median(.,na.rm=T), Q3=quantile(., na.rm=F)[4], Maximum=max(.,na.rm=T), Mean=mean(.,na.rm=T), Sd=sd(.,na.rm=T)))
    sum <- sum %>% gather(key, value) %>% separate(key, into = c("variable", "what"), sep = "\\_") %>% spread(what, value)
    sum <- sum %>% dplyr::select(variable, N, miss, Minimum, Maximum, Median, Mean, Sd)
  }
  else if(length(grp)==1){
    num <- num %>% mutate(group=data[,grp])
    sum <- num %>% group_by(group) %>%
      summarise_each(funs(N=length(which(!is.na(.))), miss=length(which(is.na(.))), Minimum=min(., na.rm=T), Q1=quantile(., na.rm=F)[2],
                          Median=median(.,na.rm=T), Q3=quantile(., na.rm=F)[4], Maximum=max(.,na.rm=T), Mean=mean(.,na.rm=T), Sd=sd(.,na.rm=T)))
    sum <- sum %>% gather(key, value, -group) %>% separate(key, into = c("variable", "what"), sep = "\\_") %>% spread(what, value)
    sum <- sum %>% dplyr::select(variable, group, N, miss, Minimum, Maximum, Median, Mean, Sd)
  }
  else{
    num <- num %>% mutate(group1=data[,grp[1]], group2=data[,grp[2]])
    sum <- num %>% group_by(group1, group2) %>%
      summarise_each(funs(Minimum=min(., na.rm=T), Q1=quantile(., na.rm=F)[2], Median=median(.,na.rm=T), Q3=quantile(., na.rm=F)[4],
                          Maximum=max(.,na.rm=T), Mean=mean(.,na.rm=T), Sd=sd(.,na.rm=T)))
    sum <- sum %>% gather(key, value, -group1, -group2) %>% separate(key, into = c("variable", "what"), sep = "\\_") %>% spread(what, value)
    sum <- sum %>% dplyr::select(variable, group1, group2, Minimum, Q1, Median, Q3, Maximum, Mean, Sd)
  }
  sum %>% mutate_if(is.numeric, funs(round(.,rd)))
}


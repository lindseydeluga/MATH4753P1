#' myci
#'
#' A function that will create a 95 percent confidence interval for mu from a single sample x
#'
#' @param x sample with a given distribution
#' @param n sample size
#' @param alpha significance level, probability of rejecting the null hypothesis when it is true.
#' @param df degrees of freedom, df = n-1
#'
#' @return
#' @export
#'
#' @examples set.seed(23);x = rnorm(30,mean=10,sd=12)
#' myci(x,alpha=.05, df=29, n=30)
#'
#'
myci = function(x,n,alpha,df){
  t=qt(1-alpha/2,df)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(n)
  ci[2]=mean(x)+t*sd(x)/sqrt(n)
  ci
}

#' myncurve
#'
#'A function that produces  a curve with the area shaded between the curve and the x axis from - infinity to x=a and will calculte the area P(X<=a)
#'
#' @param mu  population mean
#' @param sigma population variance
#' @param a upper limit of the curve
#'
#' @return
#' @export
#'
#' @examples myncurve(mu=10,sigma=5,a=6)
myncurve = function(mu,sigma,a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim=c(mu-3*sigma,mu+3*sigma))

  xcurve=seq(to = a,length=1000)

  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  polygon(c(a,xcurve),c(0,ycurve),col="Purple")

  prob= pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  list(prob)
}

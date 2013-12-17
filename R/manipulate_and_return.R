#' @title Manipulate and return a plot parameter
#' @param plot.fun A function returning a plot (or an expression creating a plot)
#' @param ... additional arguments to \code{\link{slider}}
#' @seealso \code{\link{manipulate}}
#' @export
#' @examples
#' \donttest{
#' fun=function(param){
#'  xx <- seq(-pi, pi, pi/20)
#'  qplot(xx,sin(param*xx))
#' }
#' param=slider.(fun,-1.5,1.5)
#' }
slider.=function(plot.fun,...){
  param.res=NULL
  manipulate({print(plot.fun(param));
              param.res<<-param},
              param = slider(...))
  repeat if(readline("Done ? (y/n): ")=="y") break
  param.res
}
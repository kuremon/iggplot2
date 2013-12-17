\name{slider.}
\alias{slider.}
\title{Manipulate and return a plot parameter}
\usage{
  slider.(plot.fun, ...)
}
\arguments{
  \item{plot.fun}{A function returning a plot (or an
  expression creating a plot)}

  \item{...}{additional arguments to \code{\link{slider}}}
}
\description{
  Manipulate and return a plot parameter
}
\examples{
\donttest{
fun=function(param){
 xx <- seq(-pi, pi, pi/20)
 qplot(xx,sin(param*xx))
}
param=slider.(fun,-1.5,1.5)
}
}
\seealso{
  \code{\link{manipulate}}
}


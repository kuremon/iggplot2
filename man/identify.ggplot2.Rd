\name{identify.ggplot2}
\alias{identify.ggplot2}
\title{Identify points based on a ggplot2 object}
\usage{
  identify.ggplot2(g, ...)
}
\arguments{
  \item{g}{a ggplot2 object}

  \item{...}{additional arguments to
  \code{\link{plot.and.identify}}}
}
\value{
  An index vector of selected points.
}
\description{
  Identify points based on a ggplot2 object
}
\details{
  The identification phase is not done on the ggplot2
  figure but on a basic plot ressembing the original
  ggplot2 object.
}
\examples{
\donttest{
g=ggplot(data=mtcars,aes(x=wt,y=disp))+
 geom_point(aes(color=as.factor(cyl)))
idx=identify.ggplot2(g)
}
}


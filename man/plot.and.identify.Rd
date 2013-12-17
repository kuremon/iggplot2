\name{plot.and.identify}
\alias{plot.and.identify}
\title{Plot points and identify them}
\usage{
  plot.and.identify(x, y, group, ...)
}
\arguments{
  \item{x,}{y coordinates of points in a scatter plot.}

  \item{group}{a grouping factor to determine colors on the
  plot.}

  \item{...}{additional arguments to
  \code{\link{identifyPch}}}
}
\value{
  An index vector of selected points.
}
\description{
  Plot points and identify them
}
\examples{
\donttest{
idx=with(iris,plot.and.identify(Sepal.Length,Sepal.Width,Species))
}
}


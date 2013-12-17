\name{identifyPch}
\alias{identifyPch}
\title{Identify Points in a scatter plots}
\usage{
  identifyPch(x, y = NULL, n = length(x), col = "black",
    pch = 19, alpha, remove.points = FALSE, ...)
}
\arguments{
  \item{x,}{y coordinates of points in a scatter plot.}

  \item{n}{the maximum number of points to be identified.}

  \item{col}{points color}

  \item{pch}{points size}

  \item{alpha}{alpha level of the selected points}

  \item{remove.points}{Should the selected points be
  removed from the plots?}

  \item{...}{additional arguments to
  \code{\link{identify}}}
}
\description{
  Identify Points in a scatter plots
}


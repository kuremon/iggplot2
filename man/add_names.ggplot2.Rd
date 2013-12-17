\name{add_names.ggplot2}
\alias{add_names.ggplot2}
\title{Add names to ggplot2 scatter plot points}
\usage{
  add_names.ggplot2(g, idx, label, ...)
}
\arguments{
  \item{g}{a ggplot2 object}

  \item{idx}{index of the names to add. If \code{idx} is
  not specified, \code{\link{identify.ggplot2}} is called.}

  \item{label}{the name of the variable to be used for the
  names. If \code{label} is not specified, the indexes
  \code{idx} are used instead.}

  \item{...}{additional arguments to
  \code{\link{geom_text}}}
}
\value{
  The ggplot2 object \code{g} with names added.
}
\description{
  Add names to ggplot2 scatter plot points
}
\examples{
\donttest{
g=ggplot(data=mtcars,aes(x=wt,y=disp))+
 geom_point(aes(color=as.factor(cyl)))
add_names.ggplot2(g)
}
}


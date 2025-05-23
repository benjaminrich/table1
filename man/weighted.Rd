% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table1.R
\name{weighted}
\alias{weighted}
\title{A simple class for weighted data.}
\usage{
weighted(x, w)
}
\arguments{
\item{x}{An atomic vector or \code{\link{data.frame}}.}

\item{w}{An numeric vector of weights.}
}
\value{
An object identical to \code{x} but with the additional class
\code{weighted} and a \code{weights} attribute.
}
\description{
A simple class for weighted data.
}
\examples{
x <- c(3.7, 3.3, 3.5, 2.8)
y <- c(1, 2, 1, 2)
w <- c(5, 3, 4, 1)

z <- weighted(x=x, w=w)
weights(z)
weights(z[2:3])  # Weights are preserved

d <- weighted(
  data.frame(
    x=x,
    y=y
  ),
  w
)

weights(d)
weights(d[[1]])
weights(d$x)
weights(subset(d, y==1))
lapply(split(d, d$y), weights)
}

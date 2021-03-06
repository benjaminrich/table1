% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table1.R
\name{render.missing.default}
\alias{render.missing.default}
\title{Render missing values for table output.}
\usage{
render.missing.default(x, ...)
}
\arguments{
\item{x}{A vector.}

\item{...}{Further arguments, passed to \code{\link{stats.apply.rounding}}.}
}
\value{
A \code{character} vector. Each element is to be displayed in a
separate cell in the table. The \code{\link{names}} of the vector are the
labels to use in the table. Empty strings are allowed and
result in empty table cells.
}
\description{
Called from \code{\link{table1}} by default to render missing (i.e.
\code{NA}) values for displaying in the table.
}
\examples{
y <- factor(sample(0:1, 99, replace=TRUE), labels=c("Female", "Male"))
y[1:10] <- NA
render.missing.default(y)
}
\keyword{utilities}

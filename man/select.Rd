% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/select-methods.R
\name{selectIf}
\alias{selectIf}
\alias{select}
\alias{selectIf,DataFrame,function-method}
\title{Select multiple columns}
\usage{
selectIf(object, predicate, ...)

\S4method{selectIf}{DataFrame,`function`}(object, predicate)
}
\arguments{
\item{object}{Object.}

\item{predicate}{\code{function}.
A predicate function to be applied to the columns.
For example, \code{is.atomic()}.}

\item{...}{Additional arguments.}
}
\value{
Modified object.
}
\description{
Select multiple columns
}
\note{
Updated 2019-08-26.
}
\examples{
data(iris, package = "datasets")

## DataFrame ====
x <- as(iris, "DataFrame")
selectIf(x, predicate = is.factor)
}
\seealso{
These functions are inspired by dplyr. However, they are designed to only
work on \code{DataFrame} class, and use base R code internally.\if{html}{\out{<div class="r">}}\preformatted{`help(topic = "select_all", package = "dplyr")`
}\if{html}{\out{</div>}}
}

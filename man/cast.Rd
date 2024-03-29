% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/cast-methods.R
\name{cast}
\alias{cast}
\alias{cast,DFrame-method}
\title{Cast data from long to wide format}
\usage{
cast(object, ...)

\S4method{cast}{DFrame}(object, colnames = "colname", values = "value")
}
\arguments{
\item{object}{Object.}

\item{colnames}{\code{character(1)}.
Name of the column that contains the column name values in long format.
Must define an unordered \code{factor} column.}

\item{values}{\code{character(1)}.
Name of the column that contains the values in long format.
Must define an \code{atomic} column that is not \code{factor}.}

\item{...}{Additional arguments.}
}
\value{
\code{DataFrame}.
}
\description{
Pivot column data from long format to wide format.
}
\note{
Updated 2023-08-23.
}
\examples{
data(matrix, package = "AcidTest")

## DFrame ====
long <- melt(matrix)
print(long)
wide <- cast(long)
print(wide)
}
\seealso{
\itemize{
\item https://seananderson.ca/2013/10/19/reshape/
\item \code{reshape2::dcast} (deprecated).
\item \code{tidyr::pivot_wider}.
\item Python \code{pandas.pivot_table}.
\item Python \code{pandas.wide_to_long}.
\item Python \code{pandas.DataFrame.pivot}.
}
}

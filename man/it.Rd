% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sugar.R
\name{it}
\alias{it}
\title{Syntactic Sugar Input Trafo Construction}
\usage{
it(.key, ...)
}
\arguments{
\item{.key}{(\code{character(1)})\cr
Key passed to the respective \link[mlr3misc:Dictionary]{dictionary} to retrieve
the object.}

\item{...}{(named \code{list()})\cr
Named arguments passed to the constructor, to be set as parameters in the
\link[paradox:ParamSet]{paradox::ParamSet}, or to be set as public field. See
\code{\link[mlr3misc:dictionary_sugar_get]{mlr3misc::dictionary_sugar_get()}} for more details.}
}
\value{
\link{InputTrafo}
}
\description{
This function complements \link{mlr_input_trafos} with functions in the spirit
of \code{mlr_sugar} from \CRANpkg{mlr3}.
}
\examples{
it("unitcube")
}

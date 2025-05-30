% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlr_input_trafos.R
\docType{data}
\name{mlr_input_trafos}
\alias{mlr_input_trafos}
\title{Dictionary of Input Transformations}
\format{
\link[R6:R6Class]{R6::R6Class} object inheriting from \link[mlr3misc:Dictionary]{mlr3misc::Dictionary}.
}
\description{
A simple \link[mlr3misc:Dictionary]{mlr3misc::Dictionary} storing objects of class \link{InputTrafo}.
Each input transformation has an associated help page, see \code{mlr_input_trafos[id]}.

For a more convenient way to retrieve and construct an input trafo, see \code{\link[=it]{it()}}.
}
\section{Methods}{

See \link[mlr3misc:Dictionary]{mlr3misc::Dictionary}.
}

\examples{
library(data.table)
as.data.table(mlr_input_trafos)
it("unitcube")
}
\seealso{
Sugar function: \code{\link[=it]{it()}}

Other Dictionary: 
\code{\link{mlr_acqfunctions}},
\code{\link{mlr_loop_functions}},
\code{\link{mlr_output_trafos}},
\code{\link{mlr_result_assigners}}

Other Input Transformation: 
\code{\link{InputTrafo}},
\code{\link{InputTrafoUnitcube}}
}
\concept{Dictionary}
\concept{Input Transformation}
\keyword{datasets}

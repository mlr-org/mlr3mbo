% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{redis_available}
\alias{redis_available}
\title{Check if Redis Server is Available}
\usage{
redis_available()
}
\value{
(\code{logical(1)})
}
\description{
Attempts to establish a connection to a Redis server using the \CRANpkg{redux} package
and sends a \code{PING} command. Returns \code{TRUE} if the server is available and
responds appropriately, \code{FALSE} otherwise.
}
\examples{
if (redis_available()) {
  # Proceed with code that requires Redis
  message("Redis server is available.")
} else {
  message("Redis server is not available.")
}
}

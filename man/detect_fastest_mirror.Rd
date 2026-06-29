\name{detect_fastest_mirror}
\alias{detect_fastest_mirror}
\title{Detect the Fastest CRAN Mirror}
\description{
Probes all CRAN mirrors using a two-step strategy: concurrent HEAD requests
to all mirrors, followed by real download verification on the top 10
candidates. Results are cached for 24 hours.
}
\usage{
detect_fastest_mirror(refresh_mirrors = FALSE)
}
\arguments{
\item{refresh_mirrors}{If \code{TRUE}, try to refresh the CRAN mirror list
from CRAN before probing. By default, the local CRAN mirror list bundled with R
is used.}
}
\value{
A character string with the URL of the fastest CRAN mirror.
}
\examples{
\dontrun{
mirror <- detect_fastest_mirror()
}
}
\keyword{utilities}

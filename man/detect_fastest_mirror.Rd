\name{detect_fastest_mirror}
\alias{detect_fastest_mirror}
\title{Detect the Fastest CRAN Mirror}
\description{
Probes all CRAN mirrors using a two-step strategy: concurrent HEAD requests
to all mirrors, followed by real download verification on the top 10
candidates. Results are cached for 24 hours.
}
\usage{
detect_fastest_mirror()
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

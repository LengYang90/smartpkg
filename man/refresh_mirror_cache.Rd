\name{refresh_mirror_cache}
\alias{refresh_mirror_cache}
\title{Refresh the Mirror Cache}
\description{
Clears the cached fastest mirror selection, forcing a fresh probe of all
CRAN mirrors on the next call to \code{smart_install()} or
\code{detect_fastest_mirror()}.
}
\usage{
refresh_mirror_cache()
}
\value{
Invisibly returns \code{NULL}. Called for its side effect of clearing
the cache file under \code{tools::R_user_dir("smartpkg", "cache")}.
}
\examples{
\dontrun{
refresh_mirror_cache()
}
}
\keyword{utilities}

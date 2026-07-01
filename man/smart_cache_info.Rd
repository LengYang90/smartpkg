\name{smart_cache_info}
\alias{smart_cache_info}
\title{Show Smartpkg Mirror Cache Information}
\description{
Returns the current smartpkg mirror cache status, including the cache file
location, selected CRAN and Bioconductor mirrors, cache age, and validity.
}
\usage{
smart_cache_info()
}
\value{
A one-row \code{data.frame} with columns including \code{exists},
\code{valid}, \code{path}, \code{mirror_url}, \code{bioc_mirror_url},
\code{bioc_version}, \code{timestamp}, \code{age_seconds},
\code{all_mirrors_tested}, and \code{candidate_count}.
}
\examples{
smart_cache_info()
}
\keyword{utilities}

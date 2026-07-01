\name{smart_install}
\alias{smart_install}
\title{Smart R Package Installer}
\description{
Unified replacement for \code{install.packages()}, \code{BiocManager::install()}
and \code{remotes::install_github()}. Automatically detects the package source
(CRAN, Bioconductor, GitHub, or local) and selects responsive CRAN and
Bioconductor mirrors.
}
\usage{
smart_install(pkg, ..., dry_run = FALSE, backend = c("base", "pak"))
}
\arguments{
  \item{pkg}{Package name or identifier. Supported formats:
    \describe{
      \item{\code{"dplyr"}}{CRAN package}
      \item{\code{"Bioc::limma"}}{Bioconductor package}
      \item{\code{"tidyverse/dplyr"}}{GitHub package}
      \item{\code{"./pkg_1.0.tar.gz"}}{Local package}
    }
  }
  \item{...}{Additional arguments passed to the installation backend
    (e.g. \code{quiet = TRUE}, \code{dependencies = TRUE}).}
  \item{dry_run}{Logical. If \code{TRUE}, returns the installation plan
    as a list without actually installing. Default: \code{FALSE}.}
  \item{backend}{Installation backend. Use \code{"base"} for smartpkg's
    default dispatch through \code{install.packages()},
    \code{BiocManager::install()}, and \code{remotes::install_github()}.
    Use \code{"pak"} to install through \code{pak::pkg_install()} with
    smartpkg-selected mirrors.}
}
\value{
When \code{dry_run = FALSE}: the result of the installation backend.
When \code{dry_run = TRUE}: a list with components \code{source},
\code{pkg}, \code{backend}, and \code{args}.
For multiple packages with the default backend, a \code{data.frame}
summarizes success and failure for each package.
}
\examples{
\dontrun{
smart_install("dplyr")
smart_install("Bioc::limma")
smart_install("tidyverse/dplyr")
smart_install(c("dplyr", "limma"), backend = "pak")
}
}
\keyword{utilities}

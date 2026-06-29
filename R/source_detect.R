#' Detect the source type of an R package
#'
#' Automatically identifies whether a package comes from CRAN, Bioconductor,
#' GitHub, a local path, or an unknown source.
#'
#' @param pkg Package name string
#' @return list containing source, pkg, and source-specific fields
#' @export
detect_pkg_source <- function(pkg) {
  if (is.null(pkg) || is.na(pkg) || nchar(trimws(pkg)) == 0) {
    stop("Package name is empty")
  }

  pkg <- trimws(pkg)

  # Check explicit namespace prefixes.
  if (grepl("::", pkg, fixed = TRUE)) {
    parts <- strsplit(pkg, "::", fixed = TRUE)[[1]]
    namespace <- tolower(trimws(parts[1]))
    pkg_name <- if (length(parts) >= 2) trimws(parts[2]) else ""

    if (namespace == "cran") {
      return(list(source = "cran", pkg = pkg_name))
    } else if (namespace == "bioc") {
      return(list(source = "bioc", pkg = pkg_name))
    } else if (namespace == "github") {
      return(parse_github(pkg_name))
    } else {
      return(list(source = "unknown", pkg = pkg))
    }
  }

  # Check local paths, either .tar.gz archives or existing files/directories.
  if (grepl("\\.tar\\.gz$", pkg) || file.exists(pkg)) {
    return(list(source = "local", pkg = pkg))
  }

  # Check GitHub shorthand format (username/repo).
  if (grepl("^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+$", pkg)) {
    return(parse_github(pkg))
  }

  # Treat path-like strings that do not exist as local paths.
  if (grepl("/", pkg, fixed = TRUE)) {
    # This may be a local package path that has not been created yet.
    return(list(source = "local", pkg = pkg))
  }

  # Plain package names default to CRAN.
  list(source = "cran", pkg = pkg)
}

#' Parse a GitHub package identifier
#' @param pkg username/repo format
#' @return list
parse_github <- function(pkg) {
  parts <- strsplit(pkg, "/", fixed = TRUE)[[1]]
  list(
    source = "github",
    pkg = pkg,
    username = parts[1],
    repo = paste(parts[-1], collapse = "/")
  )
}

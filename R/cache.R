#' Cache file path
cache_path <- function() {
  dir <- Sys.getenv("SMARTPKG_CACHE_DIR", unset = tools::R_user_dir("smartpkg", "cache"))
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(dir, "mirror_cache.rds")
}

#' Cache time-to-live in seconds
CACHE_TTL <- 86400  # 24 hours

#' Write mirror cache data
#' @param mirror_data list containing mirror_url, timestamp, all_mirrors_tested, candidate_count
write_cache <- function(mirror_data) {
  saveRDS(mirror_data, file = cache_path())
}

#' Read mirror cache data
#' @return list, or NULL when no cache exists
read_cache <- function() {
  path <- cache_path()
  if (file.exists(path)) {
    readRDS(path)
  } else {
    NULL
  }
}

#' Check whether the cache is still valid
#' @return logical
is_cache_valid <- function() {
  cached <- read_cache()
  if (is.null(cached)) return(FALSE)
  if (is.null(cached$timestamp)) return(FALSE)
  elapsed <- difftime(Sys.time(), cached$timestamp, units = "secs")
  as.numeric(elapsed) < CACHE_TTL
}

#' Show mirror cache information
#'
#' Returns the current smartpkg mirror cache status, including cache location,
#' selected CRAN and Bioconductor mirrors, cache age, and validity.
#'
#' @return A one-row data.frame with cache status and mirror metadata.
#' @export
smart_cache_info <- function() {
  path <- cache_path()
  cached <- read_cache()

  if (is.null(cached)) {
    return(as_smartpkg_cache_info(data.frame(
      exists = FALSE,
      valid = FALSE,
      path = path,
      mirror_url = NA_character_,
      bioc_mirror_url = NA_character_,
      bioc_version = NA_character_,
      timestamp = as.POSIXct(NA),
      age_seconds = NA_real_,
      all_mirrors_tested = NA_integer_,
      candidate_count = NA_integer_,
      stringsAsFactors = FALSE
    )))
  }

  age_seconds <- if (is.null(cached$timestamp)) {
    NA_real_
  } else {
    as.numeric(difftime(Sys.time(), cached$timestamp, units = "secs"))
  }

  as_smartpkg_cache_info(data.frame(
    exists = TRUE,
    valid = is_cache_valid(),
    path = path,
    mirror_url = cached$mirror_url %||% NA_character_,
    bioc_mirror_url = cached$bioc_mirror_url %||% NA_character_,
    bioc_version = cached$bioc_version %||% NA_character_,
    timestamp = cached$timestamp %||% as.POSIXct(NA),
    age_seconds = age_seconds,
    all_mirrors_tested = cached$all_mirrors_tested %||% NA_integer_,
    candidate_count = cached$candidate_count %||% NA_integer_,
    stringsAsFactors = FALSE
  ))
}

as_smartpkg_cache_info <- function(x) {
  class(x) <- c("smartpkg_cache_info", class(x))
  x
}

#' @export
print.smartpkg_cache_info <- function(x, ...) {
  status <- if (isTRUE(x$exists)) {
    if (isTRUE(x$valid)) "valid" else "expired"
  } else {
    "not found"
  }

  cat("smartpkg mirror cache\n")
  cat("  Status: ", status, "\n", sep = "")
  cat("  CRAN mirror: ", format_cache_value(x$mirror_url), "\n", sep = "")
  cat("  Bioc mirror: ", format_cache_value(x$bioc_mirror_url), "\n", sep = "")
  cat("  Bioc version: ", format_cache_value(x$bioc_version), "\n", sep = "")
  cat("  Timestamp: ", format_cache_value(x$timestamp), "\n", sep = "")
  cat("  Age: ", format_cache_age(x$age_seconds), "\n", sep = "")
  cat("  Mirrors tested: ", format_cache_value(x$all_mirrors_tested), "\n", sep = "")
  cat("  Verified candidates: ", format_cache_value(x$candidate_count), "\n", sep = "")
  cat("  Cache path: ", format_cache_value(x$path), "\n", sep = "")
  invisible(x)
}

format_cache_value <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return("<none>")
  }
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d %H:%M:%S"))
  }
  as.character(x)
}

format_cache_age <- function(seconds) {
  if (length(seconds) == 0 || is.na(seconds)) {
    return("<unknown>")
  }
  if (seconds < 60) {
    return(sprintf("%.0f seconds", seconds))
  }
  if (seconds < 3600) {
    return(sprintf("%.1f minutes", seconds / 60))
  }
  sprintf("%.1f hours", seconds / 3600)
}

#' Manually refresh the cache by deleting the cache file
#' @export
refresh_mirror_cache <- function() {
  path <- cache_path()
  if (file.exists(path)) {
    file.remove(path)
    smartpkg_message("Mirror cache cleared.")
  } else {
    smartpkg_message("No mirror cache found.")
  }
}

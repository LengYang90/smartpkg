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

#' Manually refresh the cache by deleting the cache file
#' @export
refresh_mirror_cache <- function() {
  path <- cache_path()
  if (file.exists(path)) {
    file.remove(path)
    message("Mirror cache cleared.")
  } else {
    message("No mirror cache found.")
  }
}

#' Get the official CRAN mirror list
#' @param refresh If TRUE, try to fetch the latest mirror list from CRAN first.
#' @return data.frame containing URL, Country, and related columns
get_mirror_list <- function(refresh = FALSE) {
  if (!isTRUE(refresh)) {
    return(utils::getCRANmirrors(all = TRUE, local.only = TRUE))
  }

  tryCatch(
    utils::getCRANmirrors(all = TRUE, local.only = FALSE),
    error = function(e) {
      smartpkg_warning("Unable to fetch remote CRAN mirror list; using local copy. ",
                       conditionMessage(e), call. = FALSE)
      utils::getCRANmirrors(all = TRUE, local.only = TRUE)
    }
  )
}

# ── Known Bioconductor Mirrors ─────────────────────────────────────────────

known_bioc_mirrors <- c(
  "https://bioconductor.org",                             # Main (US)
  "https://mirrors.ustc.edu.cn/bioc/",                     # USTC (China)
  "https://mirrors.tuna.tsinghua.edu.cn/bioconductor/",    # Tsinghua (China)
  "https://bioconductor.statistik.tu-dortmund.de/",        # Dortmund (Germany)
  "https://bioconductor.mirrors.ustc.edu.cn/"              # USTC alt (China)
)

official_bioc_mirror <- "https://bioconductor.org"

#' Get the Bioconductor mirror list
#' @return character vector of URLs
get_bioc_mirror_list <- function() {
  known_bioc_mirrors
}

#' Get the current Bioconductor version
#' @return character scalar
get_current_bioc_version <- function() {
  if (requireNamespace("BiocManager", quietly = TRUE)) {
    as.character(BiocManager::version())
  } else {
    "3.19"  # Fallback to a common version when BiocManager is unavailable.
  }
}

# ── Generic Probing Engine ─────────────────────────────────────────────────

#' Probe the response time of a single mirror using a HEAD request
#' @param url Mirror URL
#' @param timeout Timeout in seconds
#' @return Response time in seconds, or Inf on failure
probe_mirror_response_time <- function(url, timeout = 3) {
  # Do not append a trailing slash to file URLs such as PACKAGES.gz.
  if (!grepl("\\.[A-Za-z0-9]+$", url)) {
    url <- gsub("/?$", "/", url)
  }
  start <- Sys.time()

  if (requireNamespace("curl", quietly = TRUE)) {
    tryCatch({
      handle <- curl::new_handle(timeout = timeout * 1000)
      curl::handle_setopt(handle, customrequest = "HEAD", nobody = TRUE)
      con <- curl::curl(url, handle = handle, open = "r")
      on.exit(try(close(con), silent = TRUE))
      readLines(con, n = 1)
      as.numeric(difftime(Sys.time(), start, units = "secs"))
    }, error = function(e) Inf)
  } else {
    tryCatch({
      tf <- tempfile()
      on.exit(unlink(tf))
      utils::download.file(
        url = paste0(url, "index.html"), destfile = tf,
        method = "auto", quiet = TRUE, timeout = timeout
      )
      as.numeric(difftime(Sys.time(), start, units = "secs"))
    }, error = function(e) Inf)
  }
}

#' Probe multiple mirrors concurrently with curl::multi_run() HEAD requests
#' Requests are issued together, so total time is bounded by the slowest request.
#' @param mirrors Mirror URL vector
#' @param timeout Timeout in seconds
#' @return data.frame with response times
probe_mirrors_curl_multi <- function(mirrors, timeout = 3) {
  if (length(mirrors) == 0) {
    return(data.frame(URL = character(0), response_time = numeric(0),
                      stringsAsFactors = FALSE))
  }

  pool <- curl::new_pool(total_con = 100, host_con = 6)
  results <- new.env(hash = FALSE, parent = emptyenv())

  for (i in seq_along(mirrors)) {
    url <- mirrors[i]
    # Do not append a trailing slash to file URLs.
    if (!grepl("\\.[A-Za-z0-9]+$", url)) {
      url <- gsub("/?$", "/", url)
    }
    h <- curl::new_handle()
    curl::handle_setopt(h,
      customrequest = "HEAD", nobody = TRUE,
      timeout_ms = timeout * 1000, connecttimeout_ms = timeout * 1000
    )

    local({
      idx <- i
      curl::curl_fetch_multi(url, handle = h, pool = pool,
        done = function(resp) {
          if (resp$status_code == 200) {
            assign(as.character(idx), as.numeric(resp$times[["total"]]),
                   envir = results)
          } else {
            assign(as.character(idx), Inf, envir = results)
          }
        },
        fail = function(msg) {
          assign(as.character(idx), Inf, envir = results)
        }
      )
    })
  }

  curl::multi_run(pool = pool)

  response_times <- vapply(seq_along(mirrors), function(i) {
    val <- results[[as.character(i)]]
    if (is.null(val)) Inf else val
  }, numeric(1))

  data.frame(URL = mirrors, response_time = response_times,
             stringsAsFactors = FALSE)
}

#' Probe response times for multiple mirrors concurrently
#' Uses curl::multi_run() for true concurrency when curl is available.
#' @param mirrors Mirror URL vector
#' @param timeout Timeout in seconds
#' @return data.frame with response times
probe_mirrors_concurrent <- function(mirrors, timeout = 3) {
  if (requireNamespace("curl", quietly = TRUE)) {
    probe_mirrors_curl_multi(mirrors, timeout)
  } else {
    # Fallback to slower sequential probing.
    times <- vapply(mirrors, probe_mirror_response_time,
                    numeric(1), timeout = timeout, USE.NAMES = FALSE)
    data.frame(URL = mirrors, response_time = times, stringsAsFactors = FALSE)
  }
}

#' Verify candidate mirrors by concurrently downloading index.html
#' @param candidates data.frame with a URL column
#' @param timeout Timeout in seconds
#' @return data.frame with an added download_time column
verify_candidates_concurrent <- function(candidates, timeout = 5) {
  if (nrow(candidates) == 0) return(candidates)

  if (requireNamespace("curl", quietly = TRUE)) {
    pool <- curl::new_pool(total_con = 10, host_con = 6)
    results <- new.env(hash = FALSE, parent = emptyenv())

    for (i in seq_len(nrow(candidates))) {
      url <- paste0(gsub("/?$", "/", candidates$URL[i]), "index.html")
      h <- curl::new_handle()
      curl::handle_setopt(h,
        timeout_ms = timeout * 1000, connecttimeout_ms = timeout * 1000
      )

      local({
        idx <- i
        start <- Sys.time()
        curl::curl_fetch_multi(url, handle = h, pool = pool,
          done = function(resp) {
            elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
            assign(as.character(idx), elapsed, envir = results)
          },
          fail = function(msg) {
            assign(as.character(idx), Inf, envir = results)
          }
        )
      })
    }

    curl::multi_run(pool = pool)

    candidates$download_time <- vapply(seq_len(nrow(candidates)), function(i) {
      val <- results[[as.character(i)]]
      if (is.null(val)) Inf else val
    }, numeric(1))
  } else {
    candidates$download_time <- vapply(seq_len(nrow(candidates)), function(i) {
      url <- paste0(gsub("/?$", "/", candidates$URL[i]), "index.html")
      start <- Sys.time()
      tryCatch({
        tf <- tempfile()
        on.exit(unlink(tf))
        utils::download.file(url = url, destfile = tf, quiet = TRUE, timeout = timeout)
        as.numeric(difftime(Sys.time(), start, units = "secs"))
      }, error = function(e) Inf)
    }, numeric(1))
  }

  candidates
}

# ── CRAN Mirror Selection ──────────────────────────────────────────────────

#' Select the fastest mirror from the mirror list
#' @param mirrors data.frame containing a URL column
#' @param top_n Number of fastest mirrors to verify
#' @return Fastest mirror URL
get_fastest_mirror <- function(mirrors, top_n = 10) {
  urls <- mirrors$URL

  probe_results <- probe_mirrors_concurrent(urls)
  probe_results <- probe_results[is.finite(probe_results$response_time), ]
  probe_results <- probe_results[order(probe_results$response_time), ]

  if (nrow(probe_results) == 0) {
    return("https://cloud.r-project.org")
  }

  candidates <- head(probe_results, min(top_n, nrow(probe_results)))

  if (nrow(candidates) > 1) {
    candidates <- verify_candidates_concurrent(candidates, timeout = 5)
    candidates <- candidates[is.finite(candidates$download_time), ]
    if (nrow(candidates) == 0) {
      return("https://cloud.r-project.org")
    }
    candidates <- candidates[order(candidates$download_time), ]
  }

  candidates$URL[1]
}

#' Probe and return the fastest CRAN mirror URL
#'
#' Uses a two-step probing strategy:
#' 1. Send concurrent HEAD requests to all CRAN mirrors and keep the top 10.
#' 2. Verify the top candidates with real download timing and select the fastest.
#'
#' Results are cached for 24 hours.
#'
#' @param refresh_mirrors If TRUE, try to refresh the CRAN mirror list before
#'   probing. By default, the local CRAN mirror list bundled with R is used.
#' @return Fastest mirror URL string
#' @export
detect_fastest_mirror <- function(refresh_mirrors = FALSE) {
  if (is_cache_valid()) {
    cached <- read_cache()
    if (!is.null(cached$mirror_url)) {
      smartpkg_message("Using cached mirror: ", cached$mirror_url)
      return(cached$mirror_url)
    }
  }

  smartpkg_message("Probing CRAN mirrors to find the fastest one...")
  mirrors <- get_mirror_list(refresh = refresh_mirrors)
  smartpkg_message("Found ", nrow(mirrors), " CRAN mirrors")
  fastest <- get_fastest_mirror(mirrors, top_n = 10)
  smartpkg_message("Fastest mirror selected: ", fastest)

  # Update cache while preserving any existing Bioconductor mirror URL.
  cached <- read_cache()
  write_cache(list(
    mirror_url = fastest,
    bioc_mirror_url = cached$bioc_mirror_url %||% NULL,
    timestamp = Sys.time(),
    all_mirrors_tested = nrow(mirrors),
    candidate_count = 10
  ))

  fastest
}

# ── Bioconductor Mirror Selection ─────────────────────────────────────────

#' Probe and return the fastest Bioconductor mirror URL
#'
#' Sends concurrent HEAD requests to all known Bioconductor mirrors and selects
#' the fastest compatible mirror. The probe uses a version-specific path
#' (packages/{version}/bioc/) rather than the mirror root, so mirrors that do not
#' support the current Bioconductor version return 404 and are excluded.
#' Results are cached together with the CRAN mirror for 24 hours.
#'
#' @return Fastest Bioconductor mirror URL string without a trailing slash
#' @export
detect_fastest_bioc_mirror <- function() {
  bioc_version <- get_current_bioc_version()

  if (is_cache_valid()) {
    cached <- read_cache()
    if (!is.null(cached$bioc_mirror_url) &&
        identical(cached$bioc_version, bioc_version)) {
      smartpkg_message("Using cached Bioc mirror: ", cached$bioc_mirror_url)
      return(cached$bioc_mirror_url)
    }
  }

  smartpkg_message("Probing Bioconductor mirrors to find the fastest one...")
  mirrors <- unique(c(get_bioc_mirror_list(), official_bioc_mirror))
  smartpkg_message("Found ", length(mirrors), " Bioc mirrors")

  # Build version-specific probe URLs:
  # {mirror}/packages/{version}/bioc/src/contrib/PACKAGES.gz.
  # Directory paths are not enough because they may exist without PACKAGES files.
  probe_urls <- file.path(gsub("/$", "", mirrors),
                          "packages", bioc_version, "bioc",
                          "src", "contrib", "PACKAGES.gz")

  # Single-step probing is enough because the Bioconductor mirror list is small.
  probe_results <- if (length(probe_urls) > 0) {
    probe_mirrors_concurrent(probe_urls)
  } else {
    data.frame(URL = character(0), response_time = numeric(0),
               stringsAsFactors = FALSE)
  }

  probe_results <- probe_results[is.finite(probe_results$response_time), ]
  probe_results <- probe_results[order(probe_results$response_time), ]

  # Count mirrors skipped because they do not support the current Bioc version.
  all_count <- length(mirrors)
  compatible_count <- nrow(probe_results)
  skipped <- all_count - compatible_count

  if (compatible_count == 0) {
    stop("No Bioconductor mirror supports Bioc version ", bioc_version, ". ",
         "Your Bioconductor release may be too old. Please upgrade:\n",
         "  BiocManager::install(version = \"latest\")")
  }

  # Restore the mirror root URL from the version-specific PACKAGES.gz URL.
  fastest <- sub("/packages/[^/]+/bioc/src/contrib/PACKAGES\\.gz$", "",
                 probe_results$URL[1])

  # Warn when some mirrors were skipped due to version incompatibility.
  if (skipped > 0) {
    smartpkg_warning(skipped, " of ", all_count,
                     " Bioconductor mirrors do not support ",
                     "Bioc version ", bioc_version, ". ",
                     "Selected: ", fastest, ". ",
                     "A faster mirror may be available after upgrading:\n",
                     "  BiocManager::install(version = \"latest\")")
  }

  smartpkg_message("Fastest Bioc mirror selected: ", fastest)

  # Update cache while preserving any existing CRAN mirror URL.
  cached <- read_cache()
  write_cache(list(
    mirror_url = cached$mirror_url %||% NULL,
    bioc_mirror_url = fastest,
    bioc_version = bioc_version,
    timestamp = Sys.time(),
    all_mirrors_tested = cached$all_mirrors_tested %||% 0,
    candidate_count = cached$candidate_count %||% 0
  ))

  fastest
}

# ── Compatibility Helpers ─────────────────────────────────────────────────

`%||%` <- function(x, y) if (is.null(x)) y else x

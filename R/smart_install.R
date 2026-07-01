#' Smart R package installation
#'
#' Provides a unified alternative to install.packages(), BiocManager::install(),
#' and remotes::install_github(). Automatically detects package sources (CRAN,
#' Bioconductor, GitHub, or local paths) and selects responsive mirrors without
#' requiring manual configuration.
#'
#' For plain package names without an explicit namespace, the detection flow is:
#' 1. Try CRAN first.
#' 2. If the package is not found on CRAN, query Bioconductor.
#' 3. Install from Bioconductor when available.
#' 4. Error if neither source provides the package.
#'
#' @param pkg Package name. Supported formats include:
#'   - `"dplyr"` for CRAN packages, with automatic fallback to Bioconductor
#'   - `"Bioc::limma"` for Bioconductor packages
#'   - `"tidyverse/dplyr"` for GitHub packages
#'   - `"./pkg_1.0.tar.gz"` for local packages
#' @param ... Extra arguments passed to the installation backend, such as
#'   `quiet = TRUE` or `dependencies = TRUE`
#' @param dry_run If TRUE, return the installation plan without installing.
#' @return For a single package, the backend result or a plan list for dry_run.
#'   For multiple packages, a data.frame summarizing success and failure.
#' @export
smart_install <- function(pkg, ..., dry_run = FALSE) {
  if (!is.null(pkg) && length(pkg) > 1) {
    return(smart_install_many(pkg, list(...), dry_run = dry_run))
  }

  smart_install_one(pkg, list(...), dry_run = dry_run)
}

smart_install_one <- function(pkg, args, dry_run = FALSE, metadata_env = NULL) {
  info <- detect_pkg_source(pkg)

  # For plain package names during real installs, preload CRAN and Bioc package
  # indexes into R's internal cache, then classify the package source quickly.
  if (info$source == "cran" && !dry_run) {
    mirror <- detect_fastest_mirror()

    # Preload package indexes once per session.
    warm_package_cache(mirror)

    # Check CRAN from the session cache.
    on_cran <- info$pkg %in% .smartpkg_cache$cran_pkgs

    if (on_cran) {
      record_install_metadata(metadata_env, "cran")
      result <- install_cran(info$pkg, args, dry_run = FALSE)
      return(invisible(result))
    }

    # If missing from CRAN, check Bioconductor from the session cache.
    if (length(.smartpkg_cache$bioc_pkgs) > 0) {
      if (info$pkg %in% .smartpkg_cache$bioc_pkgs) {
        smartpkg_message("Not found on CRAN, installing from Bioconductor instead...")
        record_install_metadata(metadata_env, "bioc")
        return(invisible(install_bioc(
          info$pkg,
          args,
          dry_run = FALSE,
          cran_mirror = mirror,
          bioc_mirror = .smartpkg_cache$bioc_mirror_url
        )))
      }
    }

    # Neither source provides the package.
    stop("Package '", info$pkg, "' is not available on CRAN or Bioconductor.")
  }

  # dry_run or explicit namespaces can be routed directly.
  record_install_metadata(metadata_env, info$source)
  switch(info$source,
    cran = install_cran(info$pkg, args, dry_run),
    bioc = install_bioc(info$pkg, args, dry_run),
    github = install_github(info$pkg, info$username, info$repo, args, dry_run),
    local = install_local(info$pkg, args, dry_run),
    stop("Unknown package source: ", info$source)
  )
}

smart_install_many <- function(pkgs, args, dry_run = FALSE) {
  rows <- lapply(pkgs, function(pkg) {
    tryCatch({
      metadata <- new.env(parent = emptyenv())
      result <- smart_install_one(pkg, args, dry_run = dry_run, metadata_env = metadata)
      source <- metadata$source %||%
        (if (is.list(result)) result$source %||% NA_character_ else NA_character_)
      backend <- metadata$backend %||%
        (if (is.list(result)) result$backend %||% NA_character_ else NA_character_)
      smartpkg_message("\u2705 ", pkg)
      data.frame(
        status = "\u2705",
        pkg = pkg,
        success = TRUE,
        source = source,
        backend = backend,
        error = NA_character_,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      smartpkg_message("\u274c ", pkg, ": ", conditionMessage(e))
      data.frame(
        status = "\u274c",
        pkg = pkg,
        success = FALSE,
        source = NA_character_,
        backend = NA_character_,
        error = conditionMessage(e),
        stringsAsFactors = FALSE
      )
    })
  })

  summary <- do.call(rbind, rows)
  rownames(summary) <- NULL
  summary
}

record_install_metadata <- function(metadata_env, source) {
  if (is.null(metadata_env)) {
    return(invisible(NULL))
  }

  metadata_env$source <- source
  metadata_env$backend <- backend_for_source(source)
  invisible(NULL)
}

backend_for_source <- function(source) {
  switch(source,
    cran = "install.packages",
    bioc = "BiocManager::install",
    github = "remotes::install_github",
    local = "install.packages",
    NA_character_
  )
}

# ── Installation Backends ─────────────────────────────────────────────────

#' Install a CRAN package
install_cran <- function(pkg, args, dry_run) {
  mirror <- detect_fastest_mirror()

  if (dry_run) {
    return(list(
      source = "cran",
      pkg = pkg,
      mirror = mirror,
      backend = "install.packages",
      args = args
    ))
  }

  do.call(utils::install.packages, c(
    list(pkgs = pkg, repos = mirror),
    args
  ))
}

#' Install a Bioconductor package
#' Uses the selected CRAN mirror and Bioconductor mirror automatically.
install_bioc <- function(pkg, args, dry_run, cran_mirror = NULL, bioc_mirror = NULL) {
  cran_mirror <- cran_mirror %||% detect_fastest_mirror()
  bioc_mirror <- bioc_mirror %||% detect_fastest_bioc_mirror()
  args <- add_bioc_install_defaults(args)

  if (dry_run) {
    return(list(
      source = "bioc",
      pkg = pkg,
      mirror = cran_mirror,
      bioc_mirror = bioc_mirror,
      backend = "BiocManager::install",
      args = args
    ))
  }

  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    stop("BiocManager is required for Bioconductor packages. ",
         "Install it with: install.packages('BiocManager')")
  }

  old_repos <- getOption("repos")
  old_bioc_mirror <- getOption("BioC_mirror")
  on.exit({
    options(repos = old_repos)
    options(BioC_mirror = old_bioc_mirror)
  })

  # Set the Bioc mirror first, then let BiocManager build the full repository
  # set so Bioc repositories are not overwritten by options(repos = c(CRAN=...)).
  options(BioC_mirror = bioc_mirror)
  repos <- suppress_biocmanager_repository_messages(BiocManager::repositories())
  repos["CRAN"] <- cran_mirror     # Replace only CRAN; keep Bioc subrepos.
  options(repos = repos)

  suppress_biocmanager_repository_messages(
    do.call(BiocManager::install, c(list(pkg), args))
  )
}

add_bioc_install_defaults <- function(args) {
  if (is.null(args$ask)) {
    args$ask <- FALSE
  }
  if (is.null(args$update)) {
    args$update <- FALSE
  }
  args
}

#' Install a GitHub package
install_github <- function(pkg, username, repo, args, dry_run) {
  if (dry_run) {
    return(list(
      source = "github",
      pkg = pkg,
      backend = "remotes::install_github",
      args = args
    ))
  }

  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("remotes is required for GitHub packages. ",
         "Install it with: install.packages('remotes')")
  }

  do.call(remotes::install_github, c(list(pkg), args))
}

#' Install a local package
install_local <- function(pkg, args, dry_run) {
  if (dry_run) {
    return(list(
      source = "local",
      pkg = pkg,
      backend = "install.packages",
      args = args
    ))
  }

  do.call(utils::install.packages, c(
    list(pkgs = pkg, repos = NULL, type = "source"),
    args
  ))
}

# ── Package Index Preloading ──────────────────────────────────────────────

#' Session-level cache markers
.smartpkg_cache <- new.env(parent = emptyenv())

#' Preload CRAN and Bioconductor package indexes
#'
#' Downloads PACKAGES.gz files for CRAN and Bioconductor on first use. R caches
#' these results internally, so repeated lookups in the same session are fast.
#' The cache is not shared across R sessions.
#'
#' @param mirror CRAN mirror URL
warm_package_cache <- function(mirror) {
  bioc_available <- requireNamespace("BiocManager", quietly = TRUE)
  if (isTRUE(.smartpkg_cache$cran_warmed) &&
      (!bioc_available || isTRUE(.smartpkg_cache$bioc_warmed))) {
    .smartpkg_cache$warmed <- TRUE
    return()
  }

  # Preload the CRAN package index.
  if (!isTRUE(.smartpkg_cache$cran_warmed)) {
    tryCatch({
      contrib <- utils::contrib.url(mirror)
      all <- utils::available.packages(contriburl = contrib)
      .smartpkg_cache$cran_pkgs <- rownames(all)
    }, error = function(e) {
      .smartpkg_cache$cran_pkgs <- character(0)
      .smartpkg_cache$cran_error <- conditionMessage(e)
    })
    .smartpkg_cache$cran_warmed <- TRUE
  }

  # Preload the Bioc package index.
  if (bioc_available && !isTRUE(.smartpkg_cache$bioc_warmed)) {
    tryCatch({
      # Set the fastest Bioc mirror so later install_bioc() calls use it too.
      bioc_mirror <- detect_fastest_bioc_mirror()
      .smartpkg_cache$bioc_mirror_url <- bioc_mirror
      options(BioC_mirror = bioc_mirror)
      # Collect package names from all Bioc subrepositories.
      repos <- suppress_biocmanager_repository_messages(BiocManager::repositories())
      all_bioc <- character(0)
      for (r in repos) {
        contrib <- utils::contrib.url(r)
        found <- tryCatch(
          rownames(utils::available.packages(contriburl = contrib)),
          error = function(e) NULL
        )
        all_bioc <- c(all_bioc, found)
      }
      .smartpkg_cache$bioc_pkgs <- unique(all_bioc)
      .smartpkg_cache$bioc_error <- NULL
      .smartpkg_cache$bioc_warmed <- TRUE
    }, error = function(e) {
      .smartpkg_cache$bioc_pkgs <- character(0)
      .smartpkg_cache$bioc_error <- conditionMessage(e)
      .smartpkg_cache$bioc_warmed <- FALSE
    })
  }

  .smartpkg_cache$warmed <- isTRUE(.smartpkg_cache$cran_warmed) &&
    (!bioc_available || isTRUE(.smartpkg_cache$bioc_warmed))
}

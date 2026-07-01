seed_smart_install_cache <- function() {
  write_cache(list(
    mirror_url = "https://cran.example.com",
    bioc_mirror_url = "https://bioc.example.com",
    bioc_version = get_current_bioc_version(),
    timestamp = Sys.time(),
    all_mirrors_tested = 3,
    candidate_count = 1
  ))
}

test_that("smart_install routes CRAN packages correctly", {
  seed_smart_install_cache()
  result <- smart_install("dplyr", dry_run = TRUE)
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "dplyr")
  expect_true(grepl("^https?://", result$mirror))
  expect_equal(result$backend, "install.packages")
})

test_that("smart_install routes Bioconductor packages correctly", {
  seed_smart_install_cache()
  result <- smart_install("Bioc::limma", dry_run = TRUE)
  expect_equal(result$source, "bioc")
  expect_equal(result$pkg, "limma")
  expect_equal(result$backend, "BiocManager::install")
})

test_that("smart_install routes GitHub packages correctly", {
  result <- smart_install("tidyverse/dplyr", dry_run = TRUE)
  expect_equal(result$source, "github")
  expect_equal(result$pkg, "tidyverse/dplyr")
  expect_equal(result$backend, "remotes::install_github")
})

test_that("smart_install routes local packages correctly", {
  tf <- tempfile(fileext = ".tar.gz")
  file.create(tf)
  on.exit(unlink(tf))
  result <- smart_install(tf, dry_run = TRUE)
  expect_equal(result$source, "local")
  expect_equal(result$pkg, tf)
  expect_equal(result$backend, "install.packages")
})

test_that("smart_install installs package vectors in order and summarizes results", {
  calls <- character(0)
  local_mocked_bindings(
    smart_install_one = function(pkg, args, dry_run, metadata_env = NULL) {
      calls <<- c(calls, pkg)
      if (pkg == "badpkg") stop("boom")
      if (!is.null(metadata_env)) {
        metadata_env$source <- if (grepl("/", pkg)) "github" else "cran"
        metadata_env$backend <- if (grepl("/", pkg)) {
          "remotes::install_github"
        } else {
          "install.packages"
        }
      }
      list(pkg = pkg, source = "cran")
    }
  )

  expect_message(
    result <- smart_install(c("limma", "badpkg", "tidyverse/ggplot2"), dry_run = TRUE),
    "badpkg: boom"
  )

  expect_equal(calls, c("limma", "badpkg", "tidyverse/ggplot2"))
  expect_s3_class(result, "data.frame")
  expect_equal(result$pkg, c("limma", "badpkg", "tidyverse/ggplot2"))
  expect_equal(result$success, c(TRUE, FALSE, TRUE))
  expect_equal(result$status, c("\u2705", "\u274c", "\u2705"))
  expect_equal(result$source, c("cran", NA, "github"))
  expect_equal(result$backend, c("install.packages", NA, "remotes::install_github"))
  expect_equal(result$error[2], "boom")
})

test_that("smart_install treats atomic backend returns as successful in vectors", {
  local_mocked_bindings(
    smart_install_one = function(pkg, args, dry_run, metadata_env = NULL) {
      if (!is.null(metadata_env)) {
        metadata_env$source <- "bioc"
        metadata_env$backend <- "BiocManager::install"
      }
      pkg
    }
  )

  result <- smart_install(c("limma", "dplyr"), dry_run = FALSE)

  expect_equal(result$pkg, c("limma", "dplyr"))
  expect_equal(result$success, c(TRUE, TRUE))
  expect_equal(result$source, c("bioc", "bioc"))
  expect_equal(result$backend, c("BiocManager::install", "BiocManager::install"))
  expect_true(all(is.na(result$error)))
})

test_that("smart_install returns single-package result unchanged", {
  seed_smart_install_cache()
  result <- smart_install("dplyr", dry_run = TRUE)
  expect_true(is.list(result))
  expect_false(is.data.frame(result))
  expect_equal(result$pkg, "dplyr")
})

test_that("smart_install gives error for unknown packages", {
  expect_error(
    smart_install("some::unknowable::thing", dry_run = TRUE),
    "Unknown package source"
  )
})

test_that("smart_install passes extra arguments to backend", {
  seed_smart_install_cache()
  result <- smart_install("dplyr", dry_run = TRUE, quiet = TRUE, dependencies = TRUE)
  expect_equal(result$args$quiet, TRUE)
  expect_equal(result$args$dependencies, TRUE)
})

test_that("warm_package_cache retries Bioc preload after a previous Bioc failure", {
  .smartpkg_cache$warmed <- NULL
  .smartpkg_cache$cran_warmed <- NULL
  .smartpkg_cache$bioc_warmed <- NULL
  .smartpkg_cache$cran_pkgs <- NULL
  .smartpkg_cache$bioc_pkgs <- NULL

  calls <- 0
  local_mocked_bindings(
    detect_fastest_bioc_mirror = function() {
      calls <<- calls + 1
      if (calls == 1) stop("temporary Bioc failure")
      "https://bioc.example.com"
    },
    detect_fastest_mirror = function() "https://cran.example.com"
  )
  local_mocked_bindings(
    available.packages = function(contriburl, ...) {
      matrix(character(0), nrow = 0, dimnames = list(character(0), "Package"))
    },
    .package = "utils"
  )
  local_mocked_bindings(
    repositories = function(...) c(BioCsoft = "https://bioc.example.com/packages/3.23/bioc"),
    .package = "BiocManager"
  )

  warm_package_cache("https://cran.example.com")
  warm_package_cache("https://cran.example.com")

  expect_equal(calls, 2)
  expect_true(isTRUE(.smartpkg_cache$bioc_warmed))
})

test_that("smart_install reuses mirrors when falling back to Bioconductor", {
  .smartpkg_cache$warmed <- NULL
  .smartpkg_cache$cran_warmed <- NULL
  .smartpkg_cache$bioc_warmed <- NULL
  .smartpkg_cache$cran_pkgs <- NULL
  .smartpkg_cache$bioc_pkgs <- NULL

  cran_calls <- 0
  bioc_calls <- 0
  local_mocked_bindings(
    detect_fastest_mirror = function() {
      cran_calls <<- cran_calls + 1
      "https://cran.example.com"
    },
    detect_fastest_bioc_mirror = function() {
      bioc_calls <<- bioc_calls + 1
      "https://bioc.example.com"
    }
  )
  local_mocked_bindings(
    available.packages = function(contriburl, ...) {
      if (grepl("cran", contriburl) || grepl("CRAN", contriburl)) {
        return(matrix(character(0), nrow = 0, dimnames = list(character(0), "Package")))
      }
      matrix("limma", nrow = 1, dimnames = list("limma", "Package"))
    },
    .package = "utils"
  )
  local_mocked_bindings(
    repositories = function(...) c(BioCsoft = "https://bioc.example.com/packages/3.23/bioc"),
    install = function(...) "installed",
    .package = "BiocManager"
  )

  smart_install("limma")

  expect_equal(cran_calls, 1)
  expect_equal(bioc_calls, 1)
})

test_that("install_bioc uses supplied mirrors without re-detecting", {
  local_mocked_bindings(
    detect_fastest_mirror = function() stop("cran mirror should be supplied"),
    detect_fastest_bioc_mirror = function() stop("bioc mirror should be supplied")
  )

  result <- install_bioc(
    "limma",
    list(),
    dry_run = TRUE,
    cran_mirror = "https://cran.example.com",
    bioc_mirror = "https://bioc.example.com"
  )

  expect_equal(result$mirror, "https://cran.example.com")
  expect_equal(result$bioc_mirror, "https://bioc.example.com")
})

test_that("install_bioc disables interactive updates by default", {
  skip_if_not_installed("BiocManager")
  captured <- NULL
  local_mocked_bindings(
    repositories = function(...) c(BioCsoft = "https://bioc.example.com/packages/3.23/bioc"),
    install = function(...) {
      captured <<- list(...)
      "installed"
    },
    .package = "BiocManager"
  )

  install_bioc(
    "limma",
    list(),
    dry_run = FALSE,
    cran_mirror = "https://cran.example.com",
    bioc_mirror = "https://bioc.example.com"
  )

  expect_false(captured$ask)
  expect_false(captured$update)
})

test_that("install_bioc keeps explicit ask and update arguments", {
  skip_if_not_installed("BiocManager")
  captured <- NULL
  local_mocked_bindings(
    repositories = function(...) c(BioCsoft = "https://bioc.example.com/packages/3.23/bioc"),
    install = function(...) {
      captured <<- list(...)
      "installed"
    },
    .package = "BiocManager"
  )

  install_bioc(
    "limma",
    list(ask = TRUE, update = TRUE),
    dry_run = FALSE,
    cran_mirror = "https://cran.example.com",
    bioc_mirror = "https://bioc.example.com"
  )

  expect_true(captured$ask)
  expect_true(captured$update)
})

test_that("install_bioc suppresses BiocManager repository replacement messages", {
  skip_if_not_installed("BiocManager")
  seed_smart_install_cache()

  local_mocked_bindings(
    repositories = function(...) {
      message("'getOption(\"repos\")' replaces Bioconductor standard repositories")
      message("Replacement repositories:")
      message("    CRAN: https://cloud.r-project.org")
      c(BioCsoft = "https://bioc.example.com/packages/3.23/bioc",
        CRAN = "https://cran.example.com")
    },
    install = function(...) "installed",
    .package = "BiocManager"
  )

  messages <- capture.output(
    result <- install_bioc("limma", list(), dry_run = FALSE)
    ,
    type = "message"
  )

  expect_equal(result, "installed")
  expect_false(any(grepl("getOption\\(\"repos\"\\)|Replacement repositories", messages)))
})

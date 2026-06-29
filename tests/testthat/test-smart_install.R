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

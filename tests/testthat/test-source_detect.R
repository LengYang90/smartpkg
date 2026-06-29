test_that("detect_pkg_source identifies CRAN packages", {
  result <- detect_pkg_source("dplyr")
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "dplyr")
  expect_null(result$repo)

  # CRAN namespace.
  result <- detect_pkg_source("CRAN::dplyr")
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "dplyr")
})

test_that("detect_pkg_source identifies Bioconductor packages", {
  result <- detect_pkg_source("Bioc::limma")
  expect_equal(result$source, "bioc")
  expect_equal(result$pkg, "limma")

  # Namespace matching is case-insensitive.
  result <- detect_pkg_source("bioc::limma")
  expect_equal(result$source, "bioc")
  expect_equal(result$pkg, "limma")
})

test_that("detect_pkg_source identifies GitHub packages", {
  result <- detect_pkg_source("tidyverse/dplyr")
  expect_equal(result$source, "github")
  expect_equal(result$pkg, "tidyverse/dplyr")
  expect_equal(result$username, "tidyverse")
  expect_equal(result$repo, "dplyr")

  # GitHub namespace.
  result <- detect_pkg_source("GitHub::tidyverse/dplyr")
  expect_equal(result$source, "github")
  expect_equal(result$pkg, "tidyverse/dplyr")
})

test_that("detect_pkg_source identifies local packages", {
  result <- detect_pkg_source("/path/to/pkg_1.0.tar.gz")
  expect_equal(result$source, "local")
  expect_equal(result$pkg, "/path/to/pkg_1.0.tar.gz")

  result <- detect_pkg_source("./local/pkg_1.0.tar.gz")
  expect_equal(result$source, "local")
})

test_that("detect_pkg_source handles edge cases", {
  # Empty string.
  expect_error(detect_pkg_source(""), "Package name is empty")

  # NA
  expect_error(detect_pkg_source(NA), "Package name is empty")

  # Invalid format.
  result <- detect_pkg_source("some::thing::else")
  expect_equal(result$source, "unknown")
})

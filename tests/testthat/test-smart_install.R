test_that("smart_install routes CRAN packages correctly", {
  result <- smart_install("dplyr", dry_run = TRUE)
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "dplyr")
  expect_true(grepl("^https?://", result$mirror))
  expect_equal(result$backend, "install.packages")
})

test_that("smart_install routes Bioconductor packages correctly", {
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
  result <- smart_install("dplyr", dry_run = TRUE, quiet = TRUE, dependencies = TRUE)
  expect_equal(result$args$quiet, TRUE)
  expect_equal(result$args$dependencies, TRUE)
})

test_that("write_cache and read_cache roundtrip correctly", {
  # Remove any existing cache.
  path <- cache_path()
  if (file.exists(path)) file.remove(path)

  # Write cache data.
  result <- list(
    mirror_url = "https://cran.example.com",
    timestamp = Sys.time(),
    all_mirrors_tested = 120,
    candidate_count = 10
  )
  write_cache(result)

  # Read cache data.
  cached <- read_cache()
  expect_equal(cached$mirror_url, "https://cran.example.com")
  expect_true("timestamp" %in% names(cached))
  expect_true(file.exists(path))
})

test_that("cache_path uses the R user cache directory", {
  old <- Sys.getenv("SMARTPKG_CACHE_DIR", unset = NA)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("SMARTPKG_CACHE_DIR")
    } else {
      Sys.setenv(SMARTPKG_CACHE_DIR = old)
    }
  }, add = TRUE)
  Sys.unsetenv("SMARTPKG_CACHE_DIR")

  expect_equal(
    cache_path(),
    file.path(tools::R_user_dir("smartpkg", "cache"), "mirror_cache.rds")
  )
})

test_that("cache_path can be isolated with SMARTPKG_CACHE_DIR", {
  old <- Sys.getenv("SMARTPKG_CACHE_DIR", unset = NA)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("SMARTPKG_CACHE_DIR")
    } else {
      Sys.setenv(SMARTPKG_CACHE_DIR = old)
    }
  }, add = TRUE)

  test_dir <- tempfile("smartpkg-cache-")
  Sys.setenv(SMARTPKG_CACHE_DIR = test_dir)

  expect_equal(cache_path(), file.path(test_dir, "mirror_cache.rds"))
})

test_that("is_cache_valid returns TRUE for fresh cache", {
  write_cache(list(
    mirror_url = "https://cran.example.com",
    timestamp = Sys.time(),
    all_mirrors_tested = 10,
    candidate_count = 3
  ))
  expect_true(is_cache_valid())
})

test_that("is_cache_valid returns FALSE for expired cache", {
  write_cache(list(
    mirror_url = "https://cran.example.com",
    timestamp = Sys.time() - 86401,  # 24 hours plus 1 second ago
    all_mirrors_tested = 10,
    candidate_count = 3
  ))
  expect_false(is_cache_valid())
})

test_that("is_cache_valid returns FALSE when no cache exists", {
  path <- cache_path()
  if (file.exists(path)) file.remove(path)
  expect_false(is_cache_valid())
})

test_that("refresh_mirror_cache clears cache and returns FALSE for valid check", {
  # Write a cache entry first.
  write_cache(list(
    mirror_url = "https://cran.example.com",
    timestamp = Sys.time(),
    all_mirrors_tested = 10,
    candidate_count = 3
  ))
  # Clear it.
  refresh_mirror_cache()
  # The cache file should be removed.
  expect_false(file.exists(cache_path()))
  expect_false(is_cache_valid())
})

test_that("smart_cache_info reports an empty cache", {
  path <- cache_path()
  if (file.exists(path)) file.remove(path)

  info <- smart_cache_info()

  expect_s3_class(info, "data.frame")
  expect_false(info$exists)
  expect_equal(info$path, path)
  expect_false(info$valid)
  expect_true(is.na(info$mirror_url))
  expect_true(is.na(info$bioc_mirror_url))
})

test_that("smart_cache_info reports cached mirror details", {
  timestamp <- Sys.time() - 3600
  write_cache(list(
    mirror_url = "https://cran.example.com",
    bioc_mirror_url = "https://bioc.example.com",
    bioc_version = "3.23",
    timestamp = timestamp,
    all_mirrors_tested = 103,
    candidate_count = 10
  ))

  info <- smart_cache_info()

  expect_true(info$exists)
  expect_true(info$valid)
  expect_equal(info$mirror_url, "https://cran.example.com")
  expect_equal(info$bioc_mirror_url, "https://bioc.example.com")
  expect_equal(info$bioc_version, "3.23")
  expect_equal(info$all_mirrors_tested, 103)
  expect_equal(info$candidate_count, 10)
  expect_equal(info$path, cache_path())
  expect_true(info$age_seconds >= 0)
})

test_that("smart_cache_info prints a readable vertical summary", {
  write_cache(list(
    mirror_url = "https://cran.example.com",
    bioc_mirror_url = "https://bioc.example.com",
    bioc_version = "3.23",
    timestamp = as.POSIXct("2026-06-30 16:10:29", tz = "UTC"),
    all_mirrors_tested = 94,
    candidate_count = 10
  ))

  info <- smart_cache_info()
  output <- capture.output(print(info))

  expect_s3_class(info, "data.frame")
  expect_s3_class(info, "smartpkg_cache_info")
  expect_true(any(grepl("^smartpkg mirror cache$", output)))
  expect_true(any(grepl("^  Status:", output)))
  expect_true(any(grepl("^  CRAN mirror:", output)))
  expect_true(any(grepl("^  Bioc mirror:", output)))
  expect_true(any(grepl("^  Cache path:", output)))
  expect_false(any(grepl("^  exists\\s+valid", output)))
})

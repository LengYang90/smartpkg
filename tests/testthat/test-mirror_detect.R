test_that("get_mirror_list returns a data frame with URL and Country", {
  skip_on_cran()
  mirrors <- get_mirror_list()
  expect_true(is.data.frame(mirrors))
  expect_true("URL" %in% names(mirrors))
  expect_true("Country" %in% names(mirrors))
  expect_true(nrow(mirrors) > 10)
})

test_that("get_mirror_list uses local CRAN mirrors by default", {
  local_only_values <- logical(0)
  local_mocked_bindings(
    getCRANmirrors = function(all = TRUE, local.only = FALSE) {
      local_only_values <<- c(local_only_values, local.only)
      data.frame(URL = "https://local.example.com", Country = "Local")
    },
    .package = "utils"
  )

  mirrors <- get_mirror_list()

  expect_equal(mirrors$URL, "https://local.example.com")
  expect_identical(local_only_values, TRUE)
})

test_that("get_mirror_list refresh falls back to local CRAN mirrors", {
  local_only_values <- logical(0)
  local_mocked_bindings(
    getCRANmirrors = function(all = TRUE, local.only = FALSE) {
      local_only_values <<- c(local_only_values, local.only)
      if (!local.only) stop("remote mirror list unavailable")
      data.frame(URL = "https://local.example.com", Country = "Local")
    },
    .package = "utils"
  )

  expect_warning(
    mirrors <- get_mirror_list(refresh = TRUE),
    "remote CRAN mirror list"
  )

  expect_equal(mirrors$URL, "https://local.example.com")
  expect_identical(local_only_values, c(FALSE, TRUE))
})

test_that("probe_mirror_response_time returns a positive number", {
  skip_on_cran()
  # Use cloud.r-project.org as a globally available CDN endpoint.
  time <- probe_mirror_response_time("https://cloud.r-project.org")
  expect_true(is.numeric(time))
  expect_true(time > 0)
})

test_that("probe_mirror_response_time returns Inf for unreachable mirror", {
  skip_on_cran()
  time <- probe_mirror_response_time("https://this-is-not-a-real-mirror.example.com")
  expect_equal(time, Inf)
})

test_that("get_fastest_mirror returns a URL string", {
  # Use a small known mirror list for testing.
  test_mirrors <- data.frame(
    URL = c("https://cloud.r-project.org"),
    Country = c("Global"),
    stringsAsFactors = FALSE
  )
  result <- get_fastest_mirror(test_mirrors, top_n = 1)
  expect_type(result, "character")
  expect_true(nchar(result) > 10)
  expect_true(grepl("^https?://", result))
})

test_that("detect_fastest_mirror returns cached result when cache is valid", {
  # Seed a valid cache entry.
  write_cache(list(
    mirror_url = "https://cached.example.com",
    timestamp = Sys.time(),
    all_mirrors_tested = 10,
    candidate_count = 3
  ))
  result <- detect_fastest_mirror()
  expect_equal(result, "https://cached.example.com")
})

test_that("detect_fastest_mirror messages include a second-level timestamp", {
  write_cache(list(
    mirror_url = "https://cached.example.com",
    timestamp = Sys.time(),
    all_mirrors_tested = 10,
    candidate_count = 3
  ))

  expect_message(
    detect_fastest_mirror(),
    "^\\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\\] Using cached mirror"
  )
})

test_that("get_mirror_list refresh warning includes a second-level timestamp", {
  local_mocked_bindings(
    getCRANmirrors = function(all = TRUE, local.only = FALSE) {
      if (!local.only) stop("remote mirror list unavailable")
      data.frame(URL = "https://local.example.com", Country = "Local")
    },
    .package = "utils"
  )

  expect_warning(
    get_mirror_list(refresh = TRUE),
    "^\\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\\] Unable to fetch remote CRAN mirror list"
  )
})

test_that("detect_fastest_mirror refreshes and caches when cache is expired", {
  skip_on_cran()
  # Clear the cache.
  refresh_mirror_cache()
  result <- detect_fastest_mirror()
  expect_type(result, "character")
  expect_true(grepl("^https?://", result))
  # Verify that the cache was written.
  cached <- read_cache()
  expect_equal(cached$mirror_url, result)
})

test_that("detect_fastest_mirror can request a refreshed mirror list", {
  refresh_values <- logical(0)
  local_mocked_bindings(
    get_mirror_list = function(refresh = FALSE) {
      refresh_values <<- c(refresh_values, refresh)
      data.frame(URL = "https://cran.example.com", Country = "Test")
    },
    get_fastest_mirror = function(mirrors, top_n = 10) mirrors$URL[1]
  )

  refresh_mirror_cache()
  detect_fastest_mirror(refresh_mirrors = TRUE)

  expect_identical(refresh_values, TRUE)
})

test_that("detect_fastest_bioc_mirror ignores cached Bioc mirror for another version", {
  write_cache(list(
    mirror_url = "https://cran.example.com",
    bioc_mirror_url = "https://old-bioc.example.com",
    bioc_version = "0.0",
    timestamp = Sys.time()
  ))

  local_mocked_bindings(
    get_bioc_mirror_list = function() "https://new-bioc.example.com",
    probe_mirrors_concurrent = function(mirrors, timeout = 3) {
      data.frame(URL = mirrors, response_time = 0.1, stringsAsFactors = FALSE)
    }
  )

  expect_equal(detect_fastest_bioc_mirror(), "https://new-bioc.example.com")
})

test_that("detect_fastest_bioc_mirror falls back to official source when mirrors omit old version", {
  refresh_mirror_cache()

  local_mocked_bindings(
    get_bioc_mirror_list = function() "https://third-party.example.com",
    probe_mirrors_concurrent = function(mirrors, timeout = 3) {
      data.frame(
        URL = mirrors,
        response_time = ifelse(grepl("^https://bioconductor\\.org/", mirrors), 0.2, Inf),
        stringsAsFactors = FALSE
      )
    }
  )

  expect_warning(
    expect_equal(detect_fastest_bioc_mirror(), "https://bioconductor.org"),
    "do not support Bioc version"
  )
})

test_that("detect_fastest_bioc_mirror stores Bioc version with cached mirror", {
  refresh_mirror_cache()

  local_mocked_bindings(
    get_bioc_mirror_list = function() "https://versioned-bioc.example.com",
    probe_mirrors_concurrent = function(mirrors, timeout = 3) {
      data.frame(URL = mirrors, response_time = 0.1, stringsAsFactors = FALSE)
    }
  )

  detect_fastest_bioc_mirror()
  cached <- read_cache()

  expect_false(is.null(cached$bioc_version))
})

# ============================================================================
# Full smartpkg integration tests
# Cover all functions, branches, and edge cases.
# ============================================================================

# ── Helpers ────────────────────────────────────────────────────────────────

#' Create a temporary CRAN mirror data frame without using the network
make_test_mirrors <- function() {
  data.frame(
    URL = c(
      "https://fast-mirror.example.com",
      "https://slow-mirror.example.com",
      "https://unreachable-mirror.example.com"
    ),
    Country = c("China", "USA", "Nowhere"),
    stringsAsFactors = FALSE
  )
}

seed_test_mirror_cache <- function() {
  write_cache(list(
    mirror_url = "https://cran.example.com",
    bioc_mirror_url = "https://bioc.example.com",
    bioc_version = get_current_bioc_version(),
    timestamp = Sys.time(),
    all_mirrors_tested = 3,
    candidate_count = 1
  ))
}

expected_cache_dir <- function() {
  Sys.getenv("SMARTPKG_CACHE_DIR", unset = tools::R_user_dir("smartpkg", "cache"))
}

# ── 1. Cache Behavior Tests ────────────────────────────────────────────────

test_that("cache_path returns a valid file path", {
  path <- cache_path()
  expect_type(path, "character")
  expect_true(nchar(path) > 0)
  expect_true(startsWith(path, expected_cache_dir()))
  expect_true(grepl("mirror_cache\\.rds$", path))
})

test_that("write_cache and read_cache roundtrip correctly", {
  # Remove any existing cache.
  if (file.exists(cache_path())) file.remove(cache_path())

  data <- list(
    mirror_url = "https://cran.example.com",
    timestamp = Sys.time(),
    all_mirrors_tested = 120,
    candidate_count = 10
  )
  write_cache(data)
  expect_true(file.exists(cache_path()))

  cached <- read_cache()
  expect_equal(cached$mirror_url, "https://cran.example.com")
  expect_equal(cached$all_mirrors_tested, 120)
  expect_true("timestamp" %in% names(cached))
})

test_that("read_cache returns NULL when no cache exists", {
  if (file.exists(cache_path())) file.remove(cache_path())
  expect_null(read_cache())
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
    timestamp = Sys.time() - 86401,  # 24h + 1s
    all_mirrors_tested = 10,
    candidate_count = 3
  ))
  expect_false(is_cache_valid())
})

test_that("is_cache_valid returns FALSE when cache is missing", {
  if (file.exists(cache_path())) file.remove(cache_path())
  expect_false(is_cache_valid())
})

test_that("is_cache_valid returns FALSE when cache has no timestamp", {
  write_cache(list(mirror_url = "https://cran.example.com"))
  expect_false(is_cache_valid())
})

test_that("refresh_mirror_cache removes the cache file", {
  write_cache(list(mirror_url = "https://cran.example.com", timestamp = Sys.time(),
                   all_mirrors_tested = 10, candidate_count = 3))
  expect_true(file.exists(cache_path()))
  expect_message(refresh_mirror_cache(), "Mirror cache cleared")
  expect_false(file.exists(cache_path()))
})

test_that("refresh_mirror_cache handles missing cache gracefully", {
  if (file.exists(cache_path())) file.remove(cache_path())
  expect_message(refresh_mirror_cache(), "No mirror cache found")
})

# ── 2. Source Detection Tests ──────────────────────────────────────────────

test_that("detect_pkg_source: CRAN plain name", {
  result <- detect_pkg_source("dplyr")
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "dplyr")
})

test_that("detect_pkg_source: CRAN with namespace", {
  result <- detect_pkg_source("CRAN::dplyr")
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "dplyr")
})

test_that("detect_pkg_source: CRAN with lowercase namespace", {
  result <- detect_pkg_source("cran::dplyr")
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "dplyr")
})

test_that("detect_pkg_source: Bioconductor with namespace", {
  result <- detect_pkg_source("Bioc::limma")
  expect_equal(result$source, "bioc")
  expect_equal(result$pkg, "limma")
})

test_that("detect_pkg_source: Bioconductor case insensitive", {
  result <- detect_pkg_source("BIOC::limma")
  expect_equal(result$source, "bioc")
  expect_equal(result$pkg, "limma")
})

test_that("detect_pkg_source: GitHub username/repo", {
  result <- detect_pkg_source("tidyverse/dplyr")
  expect_equal(result$source, "github")
  expect_equal(result$pkg, "tidyverse/dplyr")
  expect_equal(result$username, "tidyverse")
  expect_equal(result$repo, "dplyr")
})

test_that("detect_pkg_source: GitHub complex names", {
  result <- detect_pkg_source("my-org/my_pkg.123")
  expect_equal(result$source, "github")
  expect_equal(result$username, "my-org")
  expect_equal(result$repo, "my_pkg.123")
})

test_that("detect_pkg_source: GitHub with namespace", {
  result <- detect_pkg_source("GitHub::tidyverse/dplyr")
  expect_equal(result$source, "github")
  expect_equal(result$pkg, "tidyverse/dplyr")
})

test_that("detect_pkg_source: GitHub with mixed case namespace", {
  result <- detect_pkg_source("GITHUB::user/repo")
  expect_equal(result$source, "github")
})

test_that("detect_pkg_source: local .tar.gz path", {
  result <- detect_pkg_source("/tmp/mypkg_1.0.tar.gz")
  expect_equal(result$source, "local")
  expect_equal(result$pkg, "/tmp/mypkg_1.0.tar.gz")
})

test_that("detect_pkg_source: local relative .tar.gz path", {
  result <- detect_pkg_source("./packages/pkg_1.0.tar.gz")
  expect_equal(result$source, "local")
})

test_that("detect_pkg_source: local existing file", {
  tf <- tempfile()
  file.create(tf)
  on.exit(unlink(tf))
  result <- detect_pkg_source(tf)
  expect_equal(result$source, "local")
  expect_equal(result$pkg, tf)
})

test_that("detect_pkg_source: local existing directory", {
  td <- tempdir()
  result <- detect_pkg_source(td)
  expect_equal(result$source, "local")
})

test_that("detect_pkg_source: local non-existing path with slash", {
  result <- detect_pkg_source("/nonexistent/path")
  expect_equal(result$source, "local")
})

test_that("detect_pkg_source: unknown namespace", {
  result <- detect_pkg_source("some::thing")
  expect_equal(result$source, "unknown")
  expect_equal(result$pkg, "some::thing")
})

test_that("detect_pkg_source: empty string errors", {
  expect_error(detect_pkg_source(""), "Package name is empty")
})

test_that("detect_pkg_source: NA errors", {
  expect_error(detect_pkg_source(NA), "Package name is empty")
})

test_that("detect_pkg_source: NULL errors", {
  expect_error(detect_pkg_source(NULL), "Package name is empty")
})

test_that("detect_pkg_source: whitespace-only errors", {
  expect_error(detect_pkg_source("   "), "Package name is empty")
})

test_that("detect_pkg_source: package with version numbers", {
  result <- detect_pkg_source("Rcpp")
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "Rcpp")
})

# ── 3. Mirror Probing Tests ────────────────────────────────────────────────

test_that("get_mirror_list returns valid data frame", {
  skip_on_cran()
  mirrors <- get_mirror_list()
  expect_true(is.data.frame(mirrors))
  expect_true("URL" %in% names(mirrors))
  expect_true("Country" %in% names(mirrors))
  expect_gt(nrow(mirrors), 10)
})

test_that("probe_mirror_response_time with valid mirror", {
  skip_on_cran()
  time <- probe_mirror_response_time("https://cloud.r-project.org")
  expect_true(is.numeric(time))
  expect_true(time > 0)
  expect_false(is.infinite(time))
})

test_that("probe_mirror_response_time with unreachable mirror", {
  skip_on_cran()
  time <- probe_mirror_response_time("https://this-is-not-a-real-mirror.example.com")
  expect_equal(time, Inf)
})

test_that("probe_mirror_response_time with malformed URL", {
  skip_on_cran()
  time <- probe_mirror_response_time("")
  expect_equal(time, Inf)
})

test_that("probe_mirrors_concurrent returns correct structure", {
  skip_on_cran()
  test_urls <- c("https://cloud.r-project.org")
  results <- probe_mirrors_concurrent(test_urls)
  expect_true(is.data.frame(results))
  expect_true("URL" %in% names(results))
  expect_true("response_time" %in% names(results))
  expect_equal(nrow(results), 1)
})

test_that("get_fastest_mirror selects fastest from multiple mirrors", {
  skip_on_cran()
  # Use a known reachable mirror.
  test_mirrors <- data.frame(
    URL = c("https://cloud.r-project.org"),
    Country = c("Global"),
    stringsAsFactors = FALSE
  )
  result <- get_fastest_mirror(test_mirrors, top_n = 1)
  expect_type(result, "character")
  expect_true(grepl("^https?://", result))
  expect_true(nchar(result) > 10)
})

test_that("get_fastest_mirror falls back to cloud when all fail", {
  skip_on_cran()
  test_mirrors <- data.frame(
    URL = c("https://nonexistent.example.com"),
    Country = c("Nowhere"),
    stringsAsFactors = FALSE
  )
  result <- get_fastest_mirror(test_mirrors, top_n = 1)
  expect_equal(result, "https://cloud.r-project.org")
})

test_that("get_fastest_mirror handles empty mirror list", {
  test_mirrors <- data.frame(URL = character(0), Country = character(0))
  result <- get_fastest_mirror(test_mirrors, top_n = 1)
  expect_equal(result, "https://cloud.r-project.org")
})

test_that("detect_fastest_mirror uses cache when valid", {
  # Seed a valid cache.
  write_cache(list(
    mirror_url = "https://cached-mirror.example.com",
    timestamp = Sys.time(),
    all_mirrors_tested = 10,
    candidate_count = 3
  ))
  expect_message(
    result <- detect_fastest_mirror(),
    "Using cached mirror"
  )
  expect_equal(result, "https://cached-mirror.example.com")
})

test_that("detect_fastest_mirror probes and caches when cache absent", {
  skip_on_cran()
  refresh_mirror_cache()
  expect_false(is_cache_valid())
  result <- detect_fastest_mirror()
  expect_type(result, "character")
  expect_true(grepl("^https?://", result))
  # Verify that the cache was written.
  cached <- read_cache()
  expect_equal(cached$mirror_url, result)
  expect_true("timestamp" %in% names(cached))
  expect_true(cached$all_mirrors_tested > 10)
})

# ── 4. smart_install Routing Tests ────────────────────────────────────────

test_that("smart_install: CRAN dry_run returns correct structure", {
  seed_test_mirror_cache()
  result <- smart_install("dplyr", dry_run = TRUE)
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "dplyr")
  expect_true(grepl("^https?://", result$mirror))
  expect_equal(result$backend, "install.packages")
})

test_that("smart_install: CRAN with namespace dry_run", {
  seed_test_mirror_cache()
  result <- smart_install("CRAN::dplyr", dry_run = TRUE)
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "dplyr")
})

test_that("smart_install: Bioconductor dry_run", {
  seed_test_mirror_cache()
  result <- smart_install("Bioc::limma", dry_run = TRUE)
  expect_equal(result$source, "bioc")
  expect_equal(result$pkg, "limma")
  expect_equal(result$backend, "BiocManager::install")
})

test_that("smart_install: Bioconductor lowercase dry_run", {
  seed_test_mirror_cache()
  result <- smart_install("bioc::limma", dry_run = TRUE)
  expect_equal(result$source, "bioc")
  expect_equal(result$pkg, "limma")
})

test_that("smart_install: GitHub dry_run", {
  result <- smart_install("tidyverse/dplyr", dry_run = TRUE)
  expect_equal(result$source, "github")
  expect_equal(result$pkg, "tidyverse/dplyr")
  expect_equal(result$backend, "remotes::install_github")
})

test_that("smart_install: GitHub with namespace dry_run", {
  result <- smart_install("GitHub::tidyverse/dplyr", dry_run = TRUE)
  expect_equal(result$source, "github")
  expect_equal(result$pkg, "tidyverse/dplyr")
})

test_that("smart_install: local .tar.gz dry_run", {
  tf <- tempfile(fileext = ".tar.gz")
  file.create(tf)
  on.exit(unlink(tf))
  result <- smart_install(tf, dry_run = TRUE)
  expect_equal(result$source, "local")
  expect_equal(result$pkg, tf)
  expect_equal(result$backend, "install.packages")
})

test_that("smart_install: local existing file dry_run", {
  tf <- tempfile()
  file.create(tf)
  on.exit(unlink(tf))
  result <- smart_install(tf, dry_run = TRUE)
  expect_equal(result$source, "local")
})

test_that("smart_install: unknown source errors", {
  expect_error(
    smart_install("some::unknowable::thing", dry_run = TRUE),
    "Unknown package source"
  )
})

test_that("smart_install: extra arguments passed through", {
  seed_test_mirror_cache()
  result <- smart_install("dplyr", dry_run = TRUE, quiet = TRUE, dependencies = TRUE)
  expect_equal(result$args$quiet, TRUE)
  expect_equal(result$args$dependencies, TRUE)
})

test_that("smart_install: multiple extra arguments", {
  seed_test_mirror_cache()
  result <- smart_install("dplyr", dry_run = TRUE,
                          lib = "/custom/lib", type = "source")
  expect_equal(result$args$lib, "/custom/lib")
  expect_equal(result$args$type, "source")
})

test_that("smart_install: Bioc requires BiocManager", {
  # dry_run does not perform installation checks, so this should not error.
  seed_test_mirror_cache()
  result <- smart_install("Bioc::limma", dry_run = TRUE)
  expect_equal(result$backend, "BiocManager::install")
  # BiocManager is checked only during real installs.
})

test_that("smart_install: GitHub requires remotes", {
  result <- smart_install("user/repo", dry_run = TRUE)
  expect_equal(result$backend, "remotes::install_github")
})

# ── 5. Internal Function Tests ─────────────────────────────────────────────

test_that("parse_github splits correctly", {
  result <- parse_github("user/repo")
  expect_equal(result$source, "github")
  expect_equal(result$username, "user")
  expect_equal(result$repo, "repo")
  expect_equal(result$pkg, "user/repo")
})

test_that("parse_github handles multi-level paths", {
  result <- parse_github("org/team/repo")
  expect_equal(result$username, "org")
  # Everything after the first slash is stored in repo.
  expect_equal(result$repo, "team/repo")
  expect_equal(result$pkg, "org/team/repo")
})

test_that("parse_github handles dots and hyphens", {
  result <- parse_github("my-org/my_pkg.123")
  expect_equal(result$username, "my-org")
  expect_equal(result$repo, "my_pkg.123")
})

test_that("CACHE_TTL is 24 hours in seconds", {
  expect_equal(CACHE_TTL, 86400)
})

test_that("install_cran dry_run returns expected structure", {
  seed_test_mirror_cache()
  result <- install_cran("dplyr", list(), dry_run = TRUE)
  expect_equal(result$source, "cran")
  expect_equal(result$backend, "install.packages")
  expect_true(grepl("^https?://", result$mirror))
})

test_that("install_bioc dry_run returns expected structure", {
  seed_test_mirror_cache()
  result <- install_bioc("limma", list(), dry_run = TRUE)
  expect_equal(result$source, "bioc")
  expect_equal(result$backend, "BiocManager::install")
})

test_that("install_github dry_run returns expected structure", {
  result <- install_github("user/repo", "user", "repo", list(), dry_run = TRUE)
  expect_equal(result$source, "github")
  expect_equal(result$backend, "remotes::install_github")
})

test_that("install_local dry_run returns expected structure", {
  result <- install_local("/tmp/pkg.tar.gz", list(), dry_run = TRUE)
  expect_equal(result$source, "local")
  expect_equal(result$backend, "install.packages")
})

# ── 6. End-to-End Integration Tests ───────────────────────────────────────

test_that("full pipeline: CRAN package with dry_run", {
  result <- smart_install("ggplot2", dry_run = TRUE, dependencies = TRUE)
  # Verify the full path: detect -> mirror -> route.
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "ggplot2")
  expect_true(grepl("^https?://", result$mirror))
  expect_equal(result$backend, "install.packages")
  expect_true(result$args$dependencies)
})

test_that("full pipeline: cache speeds up subsequent calls", {
  # Seed a valid cache first.
  write_cache(list(
    mirror_url = "https://fast-cran.example.com",
    timestamp = Sys.time(),
    all_mirrors_tested = 50,
    candidate_count = 10
  ))
  # The first call should use the cache.
  expect_message(
    detect_fastest_mirror(),
    "Using cached mirror"
  )
  # The second call should also use the cache.
  expect_message(
    detect_fastest_mirror(),
    "Using cached mirror"
  )
})

test_that("full pipeline: invalid cache triggers re-probe", {
  skip_on_cran()
  refresh_mirror_cache()
  expect_false(is_cache_valid())
  # This triggers real probing.
  result <- detect_fastest_mirror()
  expect_true(grepl("^https?://", result))
  # The cache should be valid after probing.
  expect_true(is_cache_valid())
})

test_that("full pipeline: all source types handled", {
  seed_test_mirror_cache()
  sources <- list(
    list(input = "dplyr",           expected = "cran"),
    list(input = "Bioc::limma",     expected = "bioc"),
    list(input = "user/repo",       expected = "github"),
    list(input = "/tmp/p.tar.gz",   expected = "local")
  )
  for (s in sources) {
    result <- smart_install(s$input, dry_run = TRUE)
    expect_equal(result$source, s$expected,
                 label = paste("Source detection failed for:", s$input))
  }
})

# ── 7. Edge Case and Error Tests ──────────────────────────────────────────

test_that("edge: very long package name", {
  long_name <- paste0(rep("x", 100), collapse = "")
  result <- detect_pkg_source(long_name)
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, long_name)
})

test_that("edge: package name with special chars (CRAN)", {
  result <- detect_pkg_source("R.oo")
  expect_equal(result$source, "cran")
})

test_that("edge: namespace with extra whitespace", {
  result <- detect_pkg_source("  CRAN::dplyr  ")
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "dplyr")
})

test_that("edge: multiple colons handled as unknown", {
  result <- detect_pkg_source("a::b::c")
  expect_equal(result$source, "unknown")
})

test_that("edge: namespace without package name", {
  # Empty package name after CRAN::.
  result <- detect_pkg_source("CRAN::")
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "")
})

test_that("edge: cache path uses configured cache directory", {
  path <- cache_path()
  expect_true(startsWith(path, expected_cache_dir()))
})

test_that("edge: concurrent probe with empty list", {
  results <- probe_mirrors_concurrent(character(0))
  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 0)
})

# ── 8. CRAN to Bioc Automatic Fallback Tests ──────────────────────────────

test_that("CRAN→Bioc fallback: known Bioc package not on CRAN", {
  skip_on_cran()
  # clusterProfiler is a Bioc package and should be found by BiocManager.
  skip_if_not_installed("BiocManager")
  avail <- BiocManager::available("clusterProfiler")
  expect_true(length(avail) > 0)
  expect_match(avail, "clusterProfiler")
})

test_that("CRAN→Bioc fallback: BiocManager knows CRAN packages too", {
  # BiocManager also indexes CRAN packages; this is expected.
  skip_if_not_installed("BiocManager")
  avail <- BiocManager::available("dplyr")
  expect_true(length(avail) > 0)
})

test_that("smart_install dry_run still reports CRAN for plain names", {
  # dry_run does not check availability and reports CRAN directly.
  write_cache(list(
    mirror_url = "https://cloud.r-project.org",
    timestamp = Sys.time(),
    all_mirrors_tested = 10, candidate_count = 3
  ))
  result <- smart_install("clusterProfiler", dry_run = TRUE)
  expect_equal(result$source, "cran")
  expect_equal(result$backend, "install.packages")
})

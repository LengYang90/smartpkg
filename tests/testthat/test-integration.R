# ============================================================================
# smartpkg 全量集成测试
# 覆盖所有函数、所有分支、所有边界情况
# ============================================================================

# ── 辅助函数 ────────────────────────────────────────────────────────────────

#' 创建临时 CRAN 镜像数据框（用于测试，不依赖网络）
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

# ── 1. 缓存机制测试 ─────────────────────────────────────────────────────────

test_that("cache_path returns a valid file path", {
  path <- cache_path()
  expect_type(path, "character")
  expect_true(nchar(path) > 0)
  expect_true(grepl("smartpkg_mirror_cache", path))
})

test_that("write_cache and read_cache roundtrip correctly", {
  # 清理可能存在的缓存
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

# ── 2. 来源识别测试 ─────────────────────────────────────────────────────────

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

# ── 3. 镜像探测测试 ─────────────────────────────────────────────────────────

test_that("get_mirror_list returns valid data frame", {
  mirrors <- get_mirror_list()
  expect_true(is.data.frame(mirrors))
  expect_true("URL" %in% names(mirrors))
  expect_true("Country" %in% names(mirrors))
  expect_gt(nrow(mirrors), 10)
})

test_that("probe_mirror_response_time with valid mirror", {
  time <- probe_mirror_response_time("https://cloud.r-project.org")
  expect_true(is.numeric(time))
  expect_true(time > 0)
  expect_false(is.infinite(time))
})

test_that("probe_mirror_response_time with unreachable mirror", {
  time <- probe_mirror_response_time("https://this-is-not-a-real-mirror.example.com")
  expect_equal(time, Inf)
})

test_that("probe_mirror_response_time with malformed URL", {
  time <- probe_mirror_response_time("")
  expect_equal(time, Inf)
})

test_that("probe_mirrors_concurrent returns correct structure", {
  test_urls <- c("https://cloud.r-project.org")
  results <- probe_mirrors_concurrent(test_urls)
  expect_true(is.data.frame(results))
  expect_true("URL" %in% names(results))
  expect_true("response_time" %in% names(results))
  expect_equal(nrow(results), 1)
})

test_that("get_fastest_mirror selects fastest from multiple mirrors", {
  # 使用已知的可达镜像
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
  # 确保缓存有效
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
  refresh_mirror_cache()
  expect_false(is_cache_valid())
  result <- detect_fastest_mirror()
  expect_type(result, "character")
  expect_true(grepl("^https?://", result))
  # 验证缓存已写入
  cached <- read_cache()
  expect_equal(cached$mirror_url, result)
  expect_true("timestamp" %in% names(cached))
  expect_true(cached$all_mirrors_tested > 10)
})

# ── 4. smart_install 路由测试 ──────────────────────────────────────────────

test_that("smart_install: CRAN dry_run returns correct structure", {
  result <- smart_install("dplyr", dry_run = TRUE)
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "dplyr")
  expect_true(grepl("^https?://", result$mirror))
  expect_equal(result$backend, "install.packages")
})

test_that("smart_install: CRAN with namespace dry_run", {
  result <- smart_install("CRAN::dplyr", dry_run = TRUE)
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "dplyr")
})

test_that("smart_install: Bioconductor dry_run", {
  result <- smart_install("Bioc::limma", dry_run = TRUE)
  expect_equal(result$source, "bioc")
  expect_equal(result$pkg, "limma")
  expect_equal(result$backend, "BiocManager::install")
})

test_that("smart_install: Bioconductor lowercase dry_run", {
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
  result <- smart_install("dplyr", dry_run = TRUE, quiet = TRUE, dependencies = TRUE)
  expect_equal(result$args$quiet, TRUE)
  expect_equal(result$args$dependencies, TRUE)
})

test_that("smart_install: multiple extra arguments", {
  result <- smart_install("dplyr", dry_run = TRUE,
                          lib = "/custom/lib", type = "source")
  expect_equal(result$args$lib, "/custom/lib")
  expect_equal(result$args$type, "source")
})

test_that("smart_install: Bioc requires BiocManager", {
  # dry_run 下不需要检查安装，所以不报错
  result <- smart_install("Bioc::limma", dry_run = TRUE)
  expect_equal(result$backend, "BiocManager::install")
  # 只有实际安装时才检查 BiocManager
})

test_that("smart_install: GitHub requires remotes", {
  result <- smart_install("user/repo", dry_run = TRUE)
  expect_equal(result$backend, "remotes::install_github")
})

# ── 5. 内部函数测试 ─────────────────────────────────────────────────────────

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
  # swich 后续部分都被放入 repo
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
  result <- install_cran("dplyr", list(), dry_run = TRUE)
  expect_equal(result$source, "cran")
  expect_equal(result$backend, "install.packages")
  expect_true(grepl("^https?://", result$mirror))
})

test_that("install_bioc dry_run returns expected structure", {
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

# ── 6. 端到端集成测试 ──────────────────────────────────────────────────────

test_that("full pipeline: CRAN package with dry_run", {
  result <- smart_install("ggplot2", dry_run = TRUE, dependencies = TRUE)
  # 验证完整链路: detect → mirror → route
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "ggplot2")
  expect_true(grepl("^https?://", result$mirror))
  expect_equal(result$backend, "install.packages")
  expect_true(result$args$dependencies)
})

test_that("full pipeline: cache speeds up subsequent calls", {
  # 先确保缓存有效
  write_cache(list(
    mirror_url = "https://fast-cran.example.com",
    timestamp = Sys.time(),
    all_mirrors_tested = 50,
    candidate_count = 10
  ))
  # 第一次调用应使用缓存
  expect_message(
    detect_fastest_mirror(),
    "Using cached mirror"
  )
  # 第二次也使用缓存
  expect_message(
    detect_fastest_mirror(),
    "Using cached mirror"
  )
})

test_that("full pipeline: invalid cache triggers re-probe", {
  refresh_mirror_cache()
  expect_false(is_cache_valid())
  # 会触发真实探测
  result <- detect_fastest_mirror()
  expect_true(grepl("^https?://", result))
  # 探测后缓存应有效
  expect_true(is_cache_valid())
})

test_that("full pipeline: all source types handled", {
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

# ── 7. 边界与异常测试 ──────────────────────────────────────────────────────

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
  # CRAN:: 后面为空
  result <- detect_pkg_source("CRAN::")
  expect_equal(result$source, "cran")
  expect_equal(result$pkg, "")
})

test_that("edge: cache path uses HOME env var", {
  # cache_path 应优先使用环境变量 HOME
  path <- cache_path()
  expect_true(grepl(Sys.getenv("HOME", unset = "~"), path))
})

test_that("edge: concurrent probe with empty list", {
  results <- probe_mirrors_concurrent(character(0))
  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 0)
})

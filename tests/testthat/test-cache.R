test_that("write_cache and read_cache roundtrip correctly", {
  # 清理可能存在的缓存
  cache_path <- file.path("~/.R", "smartpkg_mirror_cache")
  if (file.exists(cache_path)) file.remove(cache_path)

  # 写入缓存
  result <- list(
    mirror_url = "https://cran.example.com",
    timestamp = Sys.time(),
    all_mirrors_tested = 120,
    candidate_count = 10
  )
  write_cache(result)

  # 读取缓存
  cached <- read_cache()
  expect_equal(cached$mirror_url, "https://cran.example.com")
  expect_true("timestamp" %in% names(cached))
  expect_true(file.exists(cache_path))
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
    timestamp = Sys.time() - 86401,  # 24小时 + 1秒前
    all_mirrors_tested = 10,
    candidate_count = 3
  ))
  expect_false(is_cache_valid())
})

test_that("is_cache_valid returns FALSE when no cache exists", {
  cache_path <- file.path("~/.R", "smartpkg_mirror_cache")
  if (file.exists(cache_path)) file.remove(cache_path)
  expect_false(is_cache_valid())
})

test_that("refresh_mirror_cache clears cache and returns FALSE for valid check", {
  # 先写入一个缓存
  write_cache(list(
    mirror_url = "https://cran.example.com",
    timestamp = Sys.time(),
    all_mirrors_tested = 10,
    candidate_count = 3
  ))
  # 清除它
  refresh_mirror_cache()
  # 缓存文件应该被删除
  cache_path <- file.path("~/.R", "smartpkg_mirror_cache")
  expect_false(file.exists(cache_path))
  expect_false(is_cache_valid())
})

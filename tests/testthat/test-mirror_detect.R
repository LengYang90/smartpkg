test_that("get_mirror_list returns a data frame with URL and Country", {
  mirrors <- get_mirror_list()
  expect_true(is.data.frame(mirrors))
  expect_true("URL" %in% names(mirrors))
  expect_true("Country" %in% names(mirrors))
  expect_true(nrow(mirrors) > 10)
})

test_that("probe_mirror_response_time returns a positive number", {
  # 用 cloud.r-project.org 测试（CDN，全球通用）
  time <- probe_mirror_response_time("https://cloud.r-project.org")
  expect_true(is.numeric(time))
  expect_true(time > 0)
})

test_that("probe_mirror_response_time returns Inf for unreachable mirror", {
  time <- probe_mirror_response_time("https://this-is-not-a-real-mirror.example.com")
  expect_equal(time, Inf)
})

test_that("get_fastest_mirror returns a URL string", {
  # 使用已知的小镜像列表测试
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
  # 先写入有效缓存
  write_cache(list(
    mirror_url = "https://cached.example.com",
    timestamp = Sys.time(),
    all_mirrors_tested = 10,
    candidate_count = 3
  ))
  result <- detect_fastest_mirror()
  expect_equal(result, "https://cached.example.com")
})

test_that("detect_fastest_mirror refreshes and caches when cache is expired", {
  # 清除缓存
  refresh_mirror_cache()
  result <- detect_fastest_mirror()
  expect_type(result, "character")
  expect_true(grepl("^https?://", result))
  # 验证已写入缓存
  cached <- read_cache()
  expect_equal(cached$mirror_url, result)
})

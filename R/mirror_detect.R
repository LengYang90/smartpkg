#' 获取 CRAN 官方镜像列表
#' @return data.frame，包含 URL 和 Country 等列
get_mirror_list <- function() {
  utils::getCRANmirrors(all = TRUE, local.only = FALSE)
}

#' 探测单个镜像的响应时间（HEAD 请求）
#' @param url 镜像 URL
#' @param timeout 超时秒数
#' @return 响应时间（秒），失败返回 Inf
probe_mirror_response_time <- function(url, timeout = 3) {
  # 确保 URL 以 / 结尾
  url <- gsub("/?$", "/", url)

  start <- Sys.time()

  # 尝试使用 curl 包（并发友好），否则用基础的 download.file
  if (requireNamespace("curl", quietly = TRUE)) {
    tryCatch({
      handle <- curl::new_handle(timeout = timeout * 1000)
      curl::handle_setopt(handle, customrequest = "HEAD", nobody = TRUE)
      # 只做连接测试，不下载内容
      con <- curl::curl(url, handle = handle, open = "r")
      on.exit(try(close(con), silent = TRUE))
      readLines(con, n = 1)
      elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
      elapsed
    }, error = function(e) {
      Inf
    })
  } else {
    # fallback: download.file 方式
    tryCatch({
      tf <- tempfile()
      on.exit(unlink(tf))
      # time = 1 只报告连接耗时
      utils::download.file(
        url = paste0(url, "index.html"),
        destfile = tf,
        method = "auto",
        quiet = TRUE,
        timeout = timeout
      )
      elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
      elapsed
    }, error = function(e) {
      Inf
    })
  }
}

#' 并发探测多个镜像的响应时间
#'
#' 如果 curl 包可用，使用 curl::multi_run() 实现真正并发探测，
#' 96 个镜像同时检测，总耗时 ≈ 3 秒（超时上限），而非逐个探测的 5 分钟。
#'
#' @param mirrors 镜像 URL 向量
#' @param timeout 超时秒数
#' @return 带响应时间的数据框
probe_mirrors_concurrent <- function(mirrors, timeout = 3) {
  if (length(mirrors) == 0) {
    return(data.frame(URL = character(0), response_time = numeric(0),
                      stringsAsFactors = FALSE))
  }

  if (requireNamespace("curl", quietly = TRUE)) {
    probe_mirrors_curl_multi(mirrors, timeout)
  } else {
    # fallback: 顺序探测（慢）
    times <- vapply(mirrors, probe_mirror_response_time,
                    numeric(1), timeout = timeout, USE.NAMES = FALSE)
    data.frame(URL = mirrors, response_time = times, stringsAsFactors = FALSE)
  }
}

#' 使用 curl::multi_run() 并发 HEAD 请求探测所有镜像
#' 所有请求同时发出，总耗时 = 最慢请求耗时（≤ timeout）
#' @param mirrors 镜像 URL 向量
#' @param timeout 超时秒数
#' @return 带响应时间的数据框
probe_mirrors_curl_multi <- function(mirrors, timeout = 3) {
  pool <- curl::new_pool(total_con = 100, host_con = 6)
  results <- new.env(hash = FALSE, parent = emptyenv())

  for (i in seq_along(mirrors)) {
    url <- gsub("/?$", "/", mirrors[i])
    h <- curl::new_handle()
    curl::handle_setopt(h,
      customrequest = "HEAD",
      nobody = TRUE,
      timeout_ms = timeout * 1000,
      connecttimeout_ms = timeout * 1000
    )

    # 用 local() 捕获 i 的当前值，避免 R 闭包陷阱
    local({
      idx <- i
      curl::curl_fetch_multi(url, handle = h, pool = pool,
        done = function(resp) {
          assign(as.character(idx), as.numeric(resp$times[["total"]]),
                 envir = results)
        },
        fail = function(msg) {
          assign(as.character(idx), Inf, envir = results)
        }
      )
    })
  }

  curl::multi_run(pool = pool)

  response_times <- vapply(seq_along(mirrors), function(i) {
    val <- results[[as.character(i)]]
    if (is.null(val)) Inf else val
  }, numeric(1))

  data.frame(URL = mirrors, response_time = response_times,
             stringsAsFactors = FALSE)
}

#' 并发下载候选镜像的 index.html 进行真实测速
#' @param candidates 带 URL 列的 data.frame
#' @param timeout 超时秒数
#' @return 新增 download_time 列的 data.frame
verify_candidates_concurrent <- function(candidates, timeout = 5) {
  if (nrow(candidates) == 0) return(candidates)

  if (requireNamespace("curl", quietly = TRUE)) {
    pool <- curl::new_pool(total_con = 10, host_con = 6)
    results <- new.env(hash = FALSE, parent = emptyenv())

    for (i in seq_len(nrow(candidates))) {
      url <- paste0(gsub("/?$", "/", candidates$URL[i]), "index.html")
      h <- curl::new_handle()
      curl::handle_setopt(h,
        timeout_ms = timeout * 1000,
        connecttimeout_ms = timeout * 1000
      )

      local({
        idx <- i
        start <- Sys.time()
        curl::curl_fetch_multi(url, handle = h, pool = pool,
          done = function(resp) {
            elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
            assign(as.character(idx), elapsed, envir = results)
          },
          fail = function(msg) {
            assign(as.character(idx), Inf, envir = results)
          }
        )
      })
    }

    curl::multi_run(pool = pool)

    candidates$download_time <- vapply(seq_len(nrow(candidates)), function(i) {
      val <- results[[as.character(i)]]
      if (is.null(val)) Inf else val
    }, numeric(1))
  } else {
    # fallback: 顺序下载
    candidates$download_time <- vapply(seq_len(nrow(candidates)), function(i) {
      url <- paste0(gsub("/?$", "/", candidates$URL[i]), "index.html")
      start <- Sys.time()
      tryCatch({
        tf <- tempfile()
        on.exit(unlink(tf))
        utils::download.file(url = url, destfile = tf, quiet = TRUE, timeout = timeout)
        as.numeric(difftime(Sys.time(), start, units = "secs"))
      }, error = function(e) Inf)
    }, numeric(1))
  }

  candidates
}

#' 从镜像列表中获取最快的 N 个镜像
#' @param mirrors data.frame，需含 URL 列
#' @param top_n 返回前 N 个
#' @return 最快的镜像 URL
get_fastest_mirror <- function(mirrors, top_n = 10) {
  urls <- mirrors$URL

  # 第一步：并发 HEAD 探测，筛选 top_n
  probe_results <- probe_mirrors_concurrent(urls)
  probe_results <- probe_results[is.finite(probe_results$response_time), ]
  probe_results <- probe_results[order(probe_results$response_time), ]

  if (nrow(probe_results) == 0) {
    # 全部失败，回退到 CDN
    return("https://cloud.r-project.org")
  }

  candidates <- head(probe_results, min(top_n, nrow(probe_results)))

  # 第二步：对候选镜像做真实下载验证（并发）
  if (nrow(candidates) > 1) {
    candidates <- verify_candidates_concurrent(candidates, timeout = 5)
    candidates <- candidates[is.finite(candidates$download_time), ]
    if (nrow(candidates) == 0) {
      return("https://cloud.r-project.org")
    }
    candidates <- candidates[order(candidates$download_time), ]
  }

  candidates$URL[1]
}

#' 探测并返回最快的 CRAN 镜像 URL
#'
#' 使用两步探测法：
#' 1. 对所有 CRAN 镜像并发 HEAD 请求，取响应最快的 top 10
#' 2. 对 top 10 做真实下载测速，选最快的
#'
#' 结果缓存 24 小时。
#'
#' @return 最快镜像的 URL 字符串
#' @export
detect_fastest_mirror <- function() {
  # 1. 检查缓存
  if (is_cache_valid()) {
    cached <- read_cache()
    message("Using cached mirror: ", cached$mirror_url)
    return(cached$mirror_url)
  }

  message("Probing CRAN mirrors to find the fastest one...")

  # 2. 获取镜像列表
  mirrors <- get_mirror_list()
  message("Found ", nrow(mirrors), " CRAN mirrors")

  # 3. 两步探测
  fastest <- get_fastest_mirror(mirrors, top_n = 10)
  message("Fastest mirror selected: ", fastest)

  # 4. 写入缓存
  write_cache(list(
    mirror_url = fastest,
    timestamp = Sys.time(),
    all_mirrors_tested = nrow(mirrors),
    candidate_count = 10
  ))

  fastest
}

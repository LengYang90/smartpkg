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
#' @param mirrors 镜像 URL 向量
#' @param timeout 超时秒数
#' @return 带响应时间的数据框
probe_mirrors_concurrent <- function(mirrors, timeout = 3) {
  times <- vapply(mirrors, probe_mirror_response_time,
                  numeric(1), timeout = timeout, USE.NAMES = FALSE)
  data.frame(
    URL = mirrors,
    response_time = times,
    stringsAsFactors = FALSE
  )
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

  # 第二步：对候选镜像做真实下载验证
  if (nrow(candidates) > 1) {
    # 下载一个极小文件（CRAN 根目录的 index.html）
    candidates$download_time <- vapply(
      candidates$URL,
      function(url) {
        url <- gsub("/?$", "/", url)
        start <- Sys.time()
        tryCatch({
          tf <- tempfile()
          on.exit(unlink(tf))
          utils::download.file(
            url = paste0(url, "index.html"),
            destfile = tf,
            quiet = TRUE,
            timeout = 5
          )
          as.numeric(difftime(Sys.time(), start, units = "secs"))
        }, error = function(e) Inf)
      },
      numeric(1)
    )
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

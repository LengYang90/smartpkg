#' 获取 CRAN 官方镜像列表
#' @return data.frame，包含 URL 和 Country 等列
get_mirror_list <- function() {
  utils::getCRANmirrors(all = TRUE, local.only = FALSE)
}

# ── 已知的 Bioconductor 镜像列表 ──────────────────────────────────────────

known_bioc_mirrors <- c(
  "https://bioconductor.org",                             # Main (US)
  "https://mirrors.ustc.edu.cn/bioc/",                     # USTC (China)
  "https://mirrors.tuna.tsinghua.edu.cn/bioconductor/",    # Tsinghua (China)
  "https://bioconductor.statistik.tu-dortmund.de/",        # Dortmund (Germany)
  "https://bioconductor.mirrors.ustc.edu.cn/"              # USTC alt (China)
)

#' 获取 Bioconductor 镜像列表
#' 优先从 BiocManager 获取官方列表，否则使用已知列表
#' @return character vector of URLs
get_bioc_mirror_list <- function() {
  if (requireNamespace("BiocManager", quietly = TRUE)) {
    mirror_table <- tryCatch(
      BiocManager:::.getMirrorTable(),
      error = function(e) NULL
    )
    if (is.data.frame(mirror_table) && "URL" %in% names(mirror_table)) {
      return(mirror_table$URL)
    }
  }
  known_bioc_mirrors
}

# ── 探测引擎（通用） ──────────────────────────────────────────────────────

#' 探测单个镜像的响应时间（HEAD 请求）
#' @param url 镜像 URL
#' @param timeout 超时秒数
#' @return 响应时间（秒），失败返回 Inf
probe_mirror_response_time <- function(url, timeout = 3) {
  # 对文件 URL（带扩展名）不加尾部斜杠，避免 PACKAGES.gz 变成 PACKAGES.gz/
  if (!grepl("\\.[A-Za-z0-9]+$", url)) {
    url <- gsub("/?$", "/", url)
  }
  start <- Sys.time()

  if (requireNamespace("curl", quietly = TRUE)) {
    tryCatch({
      handle <- curl::new_handle(timeout = timeout * 1000)
      curl::handle_setopt(handle, customrequest = "HEAD", nobody = TRUE)
      con <- curl::curl(url, handle = handle, open = "r")
      on.exit(try(close(con), silent = TRUE))
      readLines(con, n = 1)
      as.numeric(difftime(Sys.time(), start, units = "secs"))
    }, error = function(e) Inf)
  } else {
    tryCatch({
      tf <- tempfile()
      on.exit(unlink(tf))
      utils::download.file(
        url = paste0(url, "index.html"), destfile = tf,
        method = "auto", quiet = TRUE, timeout = timeout
      )
      as.numeric(difftime(Sys.time(), start, units = "secs"))
    }, error = function(e) Inf)
  }
}

#' 使用 curl::multi_run() 并发 HEAD 请求探测多个镜像
#' 所有请求同时发出，总耗时 = 最慢请求耗时（≤ timeout）
#' @param mirrors 镜像 URL 向量
#' @param timeout 超时秒数
#' @return 带响应时间的数据框
probe_mirrors_curl_multi <- function(mirrors, timeout = 3) {
  if (length(mirrors) == 0) {
    return(data.frame(URL = character(0), response_time = numeric(0),
                      stringsAsFactors = FALSE))
  }

  pool <- curl::new_pool(total_con = 100, host_con = 6)
  results <- new.env(hash = FALSE, parent = emptyenv())

  for (i in seq_along(mirrors)) {
    url <- mirrors[i]
    # 对文件 URL（带扩展名）不加尾部斜杠
    if (!grepl("\\.[A-Za-z0-9]+$", url)) {
      url <- gsub("/?$", "/", url)
    }
    h <- curl::new_handle()
    curl::handle_setopt(h,
      customrequest = "HEAD", nobody = TRUE,
      timeout_ms = timeout * 1000, connecttimeout_ms = timeout * 1000
    )

    local({
      idx <- i
      curl::curl_fetch_multi(url, handle = h, pool = pool,
        done = function(resp) {
          if (resp$status_code == 200) {
            assign(as.character(idx), as.numeric(resp$times[["total"]]),
                   envir = results)
          } else {
            assign(as.character(idx), Inf, envir = results)
          }
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

#' 并发探测多个镜像的响应时间
#' 如果 curl 包可用，使用 curl::multi_run() 实现真正并发探测
#' @param mirrors 镜像 URL 向量
#' @param timeout 超时秒数
#' @return 带响应时间的数据框
probe_mirrors_concurrent <- function(mirrors, timeout = 3) {
  if (requireNamespace("curl", quietly = TRUE)) {
    probe_mirrors_curl_multi(mirrors, timeout)
  } else {
    # fallback: 顺序探测（慢）
    times <- vapply(mirrors, probe_mirror_response_time,
                    numeric(1), timeout = timeout, USE.NAMES = FALSE)
    data.frame(URL = mirrors, response_time = times, stringsAsFactors = FALSE)
  }
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
        timeout_ms = timeout * 1000, connecttimeout_ms = timeout * 1000
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

# ── CRAN 镜像选择 ─────────────────────────────────────────────────────────

#' 从镜像列表中获取最快的 N 个镜像
#' @param mirrors data.frame，需含 URL 列
#' @param top_n 返回前 N 个
#' @return 最快的镜像 URL
get_fastest_mirror <- function(mirrors, top_n = 10) {
  urls <- mirrors$URL

  probe_results <- probe_mirrors_concurrent(urls)
  probe_results <- probe_results[is.finite(probe_results$response_time), ]
  probe_results <- probe_results[order(probe_results$response_time), ]

  if (nrow(probe_results) == 0) {
    return("https://cloud.r-project.org")
  }

  candidates <- head(probe_results, min(top_n, nrow(probe_results)))

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
  if (is_cache_valid()) {
    cached <- read_cache()
    if (!is.null(cached$mirror_url)) {
      message("Using cached mirror: ", cached$mirror_url)
      return(cached$mirror_url)
    }
  }

  message("Probing CRAN mirrors to find the fastest one...")
  mirrors <- get_mirror_list()
  message("Found ", nrow(mirrors), " CRAN mirrors")
  fastest <- get_fastest_mirror(mirrors, top_n = 10)
  message("Fastest mirror selected: ", fastest)

  # 更新缓存，保留已有的 bioc_mirror_url
  cached <- read_cache()
  write_cache(list(
    mirror_url = fastest,
    bioc_mirror_url = cached$bioc_mirror_url %||% NULL,
    timestamp = Sys.time(),
    all_mirrors_tested = nrow(mirrors),
    candidate_count = 10
  ))

  fastest
}

# ── Bioconductor 镜像选择 ────────────────────────────────────────────────

#' 探测并返回最快的 Bioconductor 镜像 URL
#'
#' 对所有已知 Bioc 镜像做并发 HEAD 请求，选响应最快的。
#' 注意：探测的是版本特定路径（packages/{version}/bioc/），
#' 而非镜像根路径。这样不兼容当前 Bioc 版本的镜像会返回 404 被自动排除。
#' 结果与 CRAN 镜像一起缓存 24 小时。
#'
#' @return 最快 Bioc 镜像的 URL 字符串（末尾无斜线）
#' @export
detect_fastest_bioc_mirror <- function() {
  if (is_cache_valid()) {
    cached <- read_cache()
    if (!is.null(cached$bioc_mirror_url)) {
      message("Using cached Bioc mirror: ", cached$bioc_mirror_url)
      return(cached$bioc_mirror_url)
    }
  }

  message("Probing Bioconductor mirrors to find the fastest one...")
  mirrors <- get_bioc_mirror_list()
  message("Found ", length(mirrors), " Bioc mirrors")

  # 获取当前 Bioc 版本，构造版本特定探测 URL
  # 这样不支持当前 Bioc 版本的镜像会返回 404，不会被选上
  bioc_version <- if (requireNamespace("BiocManager", quietly = TRUE)) {
    as.character(BiocManager::version())
  } else {
    "3.19"  # fallback：如果没装 BiocManager，用常见版本
  }

  # 构造版本特定的探测 URL：{mirror}/packages/{version}/bioc/src/contrib/PACKAGES.gz
  # 注意：不能用目录路径（如 .../bioc/），因为目录存在不代表里面的 PACKAGES 文件存在
  probe_urls <- file.path(gsub("/$", "", mirrors),
                          "packages", bioc_version, "bioc",
                          "src", "contrib", "PACKAGES.gz")

  # 单步探测（Bioc 镜像少，无需下载验证）
  probe_results <- if (length(probe_urls) > 0) {
    probe_mirrors_concurrent(probe_urls)
  } else {
    data.frame(URL = character(0), response_time = numeric(0),
               stringsAsFactors = FALSE)
  }

  probe_results <- probe_results[is.finite(probe_results$response_time), ]
  probe_results <- probe_results[order(probe_results$response_time), ]

  # 统计被跳过的镜像（版本不兼容）
  all_count <- length(mirrors)
  compatible_count <- nrow(probe_results)
  skipped <- all_count - compatible_count

  if (compatible_count == 0) {
    stop("No Bioconductor mirror supports Bioc version ", bioc_version, ". ",
         "Your BiocManager may be too old. Please upgrade:\n",
         "  install.packages(\"BiocManager\")")
  }

  # 从版本特定 PACKAGES.gz URL 还原为镜像根 URL
  fastest <- sub("/packages/[^/]+/bioc/src/contrib/PACKAGES\\.gz$", "",
                 probe_results$URL[1])

  # 如果部分镜像因版本不兼容被跳过，提示用户
  if (skipped > 0) {
    warning(skipped, " of ", all_count, " Bioconductor mirrors do not support ",
            "Bioc version ", bioc_version, ". ",
	    "Selected: ", fastest, ". ",
            "A faster mirror may be available after upgrading:\n",
            "  install.packages(\"BiocManager\")")
  }

  message("Fastest Bioc mirror selected: ", fastest)

  # 更新缓存，保留已有的 mirror_url
  cached <- read_cache()
  write_cache(list(
    mirror_url = cached$mirror_url %||% NULL,
    bioc_mirror_url = fastest,
    timestamp = Sys.time(),
    all_mirrors_tested = cached$all_mirrors_tested %||% 0,
    candidate_count = cached$candidate_count %||% 0
  ))

  fastest
}

# ── 兼容函数 ──────────────────────────────────────────────────────────────

`%||%` <- function(x, y) if (is.null(x)) y else x

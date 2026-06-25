#' 智能安装 R 包
#'
#' 统一替代 install.packages()、BiocManager::install() 和 remotes::install_github()。
#' 自动识别包来源（CRAN / Bioconductor / GitHub / 本地），
#' 自动选择最快的 CRAN 镜像，全程无需手动干预。
#'
#' 对于纯包名（无命名空间），自动检测流程：
#' 1. 先尝试 CRAN 安装
#' 2. 如果 CRAN 上找不到，自动查询 Bioconductor
#' 3. 如果 Bioconductor 上有，从 Bioc 安装
#' 4. 如果两个源都没有，报错
#'
#' @param pkg 包名。支持以下格式：
#'   - `"dplyr"` — CRAN 包（自动 fallback 到 Bioconductor）
#'   - `"Bioc::limma"` — Bioconductor 包
#'   - `"tidyverse/dplyr"` — GitHub 包
#'   - `"./pkg_1.0.tar.gz"` — 本地包
#' @param ... 传递给安装后端的额外参数（如 `quiet = TRUE`, `dependencies = TRUE`）
#' @param dry_run 如果为 TRUE，只返回安装计划而不实际安装（用于测试）
#' @return 实际安装时：安装结果（取决于后端）。dry_run 时：list 包含安装计划。
#' @export
smart_install <- function(pkg, ..., dry_run = FALSE) {
  info <- detect_pkg_source(pkg)
  args <- list(...)

  # 纯包名 + 实际安装：CRAN → Bioc 自动 fallback
  # 注意：install.packages() 对不存在的包只发 warning 不抛 error。
  # 使用 HEAD 请求快速检查（约 0.1s），而非 available.packages()（下载 PACKAGES.gz，3-5s）。
  if (info$source == "cran" && !dry_run) {
    mirror <- detect_fastest_mirror()

    # 快速检查包是否在 CRAN 上存在
    on_cran <- pkg_exists_on_cran(info$pkg)

    if (!on_cran) {
      # CRAN 上不存在 → 查询 Bioconductor
      if (requireNamespace("BiocManager", quietly = TRUE)) {
        bioc_avail <- BiocManager::available(info$pkg)
        if (length(bioc_avail) > 0) {
          message("Not found on CRAN, installing from Bioconductor instead...")
          return(invisible(install_bioc(info$pkg, args, dry_run = FALSE)))
        }
      }
      # 两个源都没有 → 让 install.packages 给出标准错误提示
    }

    result <- install_cran(info$pkg, args, dry_run = FALSE)
    return(invisible(result))
  }

  # dry_run 或显式命名空间：直接路由
  switch(info$source,
    cran = install_cran(info$pkg, args, dry_run),
    bioc = install_bioc(info$pkg, args, dry_run),
    github = install_github(info$pkg, info$username, info$repo, args, dry_run),
    local = install_local(info$pkg, args, dry_run),
    stop("Unknown package source: ", info$source)
  )
}

# ── 安装后端 ──────────────────────────────────────────────────────────────

#' 安装 CRAN 包
install_cran <- function(pkg, args, dry_run) {
  mirror <- detect_fastest_mirror()

  if (dry_run) {
    return(list(
      source = "cran",
      pkg = pkg,
      mirror = mirror,
      backend = "install.packages",
      args = args
    ))
  }

  do.call(utils::install.packages, c(
    list(pkgs = pkg, repos = mirror),
    args
  ))
}

#' 安装 Bioconductor 包
#' 自动使用最快 CRAN 镜像 + 最快 Bioconductor 镜像
install_bioc <- function(pkg, args, dry_run) {
  cran_mirror <- detect_fastest_mirror()
  bioc_mirror <- detect_fastest_bioc_mirror()

  if (dry_run) {
    return(list(
      source = "bioc",
      pkg = pkg,
      mirror = cran_mirror,
      bioc_mirror = bioc_mirror,
      backend = "BiocManager::install",
      args = args
    ))
  }

  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    stop("BiocManager is required for Bioconductor packages. ",
         "Install it with: install.packages('BiocManager')")
  }

  old_repos <- getOption("repos")
  old_bioc_mirror <- getOption("BioC_mirror")
  on.exit({
    options(repos = old_repos)
    options(BioC_mirror = old_bioc_mirror)
  })

  # 先设 Bioc 镜像，再通过 BiocManager::repositories() 构建完整仓库列表
  # 这样 Bioc 仓库走最快镜像，而不是被 options(repos = c(CRAN=...)) 覆盖
  options(BioC_mirror = bioc_mirror)
  repos <- BiocManager::repositories()
  repos["CRAN"] <- cran_mirror     # 只替换 CRAN 条目，保留 Bioc 各子仓库
  options(repos = repos)

  do.call(BiocManager::install, c(list(pkg), args))
}

#' 安装 GitHub 包
install_github <- function(pkg, username, repo, args, dry_run) {
  if (dry_run) {
    return(list(
      source = "github",
      pkg = pkg,
      backend = "remotes::install_github",
      args = args
    ))
  }

  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("remotes is required for GitHub packages. ",
         "Install it with: install.packages('remotes')")
  }

  do.call(remotes::install_github, c(list(pkg), args))
}

#' 安装本地包
install_local <- function(pkg, args, dry_run) {
  if (dry_run) {
    return(list(
      source = "local",
      pkg = pkg,
      backend = "install.packages",
      args = args
    ))
  }

  do.call(utils::install.packages, c(
    list(pkgs = pkg, repos = NULL, type = "source"),
    args
  ))
}

# ── CRAN 包存在性快速检查 ────────────────────────────────────────────────

#' 快速检查包是否在 CRAN 上存在且当前可安装
#'
#' 两步检测法（全部用 HEAD/短读取，避免下载 PACKAGES.gz）:
#' 1. HEAD `web/packages/<pkg>/` → 404 = 从未上过 CRAN → FALSE
#' 2. 读取页面前 15 行 → 含 "removed from the CRAN" → 已归档 → FALSE
#' 3. 否则 → 活跃 CRAN 包 → TRUE
#'
#' @param pkg 包名
#' @return TRUE 表示包在 CRAN 上当前可用，FALSE 表示不在
pkg_exists_on_cran <- function(pkg) {
  if (!requireNamespace("curl", quietly = TRUE)) return(TRUE)

  base_url <- "https://cloud.r-project.org/web/packages"
  url <- paste0(base_url, "/", pkg, "/")

  # Step 1: HEAD 检查 → 404 = 从未上过 CRAN
  head_resp <- tryCatch({
    h <- curl::new_handle()
    curl::handle_setopt(h, customrequest = "HEAD", nobody = TRUE,
                        timeout_ms = 5000)
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e) NULL)

  if (is.null(head_resp) || head_resp$status_code == 404) {
    return(FALSE)
  }

  # Step 2: HEAD 200 → 读取前 15 行判断是否已归档（已移除的 CRAN 包）
  page_url <- paste0(url, "index.html")
  con <- NULL
  lines <- NULL
  tryCatch({
    con <- curl::curl(page_url, open = "r")
    lines <- readLines(con, n = 15, warn = FALSE)
  }, error = function(e) {
    lines <<- NULL
  }, finally = {
    if (!is.null(con)) try(close(con), silent = TRUE)
  })

  # 无法读取页面 → 保守返回 TRUE
  if (is.null(lines)) return(TRUE)

  # 已归档的包页面含有 "removed from the CRAN repository"
  if (any(grepl("removed from the CRAN", lines, ignore.case = TRUE))) {
    return(FALSE)
  }

  TRUE
}

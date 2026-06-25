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

  # 纯包名 + 实际安装：自动判断 CRAN 还是 Bioconductor
  # 直接让 install.packages() 跑，它内部调 available.packages() 且结果被 R 缓存。
  # 用 withCallingHandlers 捕获 "not available" 警告来触发 Bioc fallback，
  # 不需要我们事先再调一次 available.packages()。
  if (info$source == "cran" && !dry_run) {
    mirror <- detect_fastest_mirror()

    not_available <- FALSE
    result <- withCallingHandlers(
      install_cran(info$pkg, args, dry_run = FALSE),
      warning = function(w) {
        msg <- conditionMessage(w)
        # install.packages("limma", repos=...) → "package 'limma' is not available"
        if (grepl("not available", msg, fixed = TRUE)) {
          not_available <<- TRUE
          invokeRestart("muffleWarning")
        }
      }
    )

    if (not_available) {
      # CRAN 上不存在 → 查询 Bioconductor
      if (requireNamespace("BiocManager", quietly = TRUE)) {
        if (length(BiocManager::available(info$pkg)) > 0) {
          message("Not found on CRAN, installing from Bioconductor instead...")
          return(invisible(install_bioc(info$pkg, args, dry_run = FALSE)))
        }
      }
      # Bioc 也找不到
      warning("package '", info$pkg, "' is not available for this version of R")
      return(invisible(NULL))
    }

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

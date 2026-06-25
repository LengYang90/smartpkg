#' 智能安装 R 包
#'
#' 统一替代 install.packages()、BiocManager::install() 和 remotes::install_github()。
#' 自动识别包来源（CRAN / Bioconductor / GitHub / 本地），
#' 自动选择最快的 CRAN 镜像，全程无需手动干预。
#'
#' @param pkg 包名。支持以下格式：
#'   - `"dplyr"` — CRAN 包
#'   - `"Bioc::limma"` — Bioconductor 包
#'   - `"tidyverse/dplyr"` — GitHub 包
#'   - `"./pkg_1.0.tar.gz"` — 本地包
#' @param ... 传递给安装后端的额外参数（如 `quiet = TRUE`, `dependencies = TRUE`）
#' @param dry_run 如果为 TRUE，只返回安装计划而不实际安装（用于测试）
#' @return 实际安装时：安装结果（取决于后端）。dry_run 时：list 包含安装计划。
#' @export
smart_install <- function(pkg, ..., dry_run = FALSE) {
  # 1. 识别来源
  info <- detect_pkg_source(pkg)

  # 2. 根据来源路由
  args <- list(...)

  switch(info$source,
    cran = install_cran(info$pkg, args, dry_run),
    bioc = install_bioc(info$pkg, args, dry_run),
    github = install_github(info$pkg, info$username, info$repo, args, dry_run),
    local = install_local(info$pkg, args, dry_run),
    stop("Unknown package source: ", info$source)
  )
}

#' 安装 CRAN 包
install_cran <- function(pkg, args, dry_run) {
  # 获取最快镜像
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

  # 实际安装
  do.call(utils::install.packages, c(
    list(pkgs = pkg, repos = mirror),
    args
  ))
}

#' 安装 Bioconductor 包
#' 自动使用最快 CRAN 镜像安装 CRAN 依赖
install_bioc <- function(pkg, args, dry_run) {
  # Bioconductor 也需要最快 CRAN 镜像来安装 CRAN 依赖
  mirror <- detect_fastest_mirror()

  if (dry_run) {
    return(list(
      source = "bioc",
      pkg = pkg,
      mirror = mirror,
      backend = "BiocManager::install",
      args = args
    ))
  }

  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    stop("BiocManager is required for Bioconductor packages. ",
         "Install it with: install.packages('BiocManager')")
  }

  # 设置最快 CRAN 镜像，让 BiocManager 安装 CRAN 依赖时也用最快的源
  old_repos <- getOption("repos")
  on.exit(options(repos = old_repos))
  options(repos = c(CRAN = mirror))

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

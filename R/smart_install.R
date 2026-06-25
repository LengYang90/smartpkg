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

  # 纯包名 + 实际安装：先预加载 CRAN 和 Bioc 的包索引（R 内部自动缓存），
  # 然后快速判断包属于哪个源。首次慢（下载 PACKAGES.gz），后续秒回。
  if (info$source == "cran" && !dry_run) {
    mirror <- detect_fastest_mirror()

    # 预加载包索引（仅首次会话下载，之后全部走缓存）
    warm_package_cache(mirror)

    # 检查 CRAN（从会话缓存查，≈0.001s）
    on_cran <- info$pkg %in% .smartpkg_cache$cran_pkgs

    if (on_cran) {
      result <- install_cran(info$pkg, args, dry_run = FALSE)
      return(invisible(result))
    }

    # CRAN 上不存在 → 查询 Bioconductor（从会话缓存查，≈0.001s）
    if (length(.smartpkg_cache$bioc_pkgs) > 0) {
      if (info$pkg %in% .smartpkg_cache$bioc_pkgs) {
        message("Not found on CRAN, installing from Bioconductor instead...")
        return(invisible(install_bioc(info$pkg, args, dry_run = FALSE)))
      }
    }

    # 两个源都没有
    stop("Package '", info$pkg, "' is not available on CRAN or Bioconductor.")
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

# ── 包索引预缓存 ──────────────────────────────────────────────────────────

#' 会话级缓存标记
.smartpkg_cache <- new.env(parent = emptyenv())

#' 预加载 CRAN 和 Bioconductor 的包索引
#'
#' 第一次调用时下载 PACKAGES.gz（CRAN + Bioc），
#' R 内部自动缓存结果。后续相同的查询瞬间返回。
#' 不同 R 会话之间不共享此缓存。
#'
#' @param mirror CRAN 镜像 URL
warm_package_cache <- function(mirror) {
  if (isTRUE(.smartpkg_cache$warmed)) return()

  # 预加载 CRAN 包索引
  tryCatch({
    contrib <- utils::contrib.url(mirror)
    all <- utils::available.packages(contriburl = contrib)
    .smartpkg_cache$cran_pkgs <- rownames(all)
  }, error = function(e) .smartpkg_cache$cran_pkgs <- character(0))

  # 预加载 Bioc 包索引
  if (requireNamespace("BiocManager", quietly = TRUE)) {
    tryCatch({
      # 先设最快 Bioc 镜像，这样后续 install_bioc() 也用相同 URL
      bioc_mirror <- detect_fastest_bioc_mirror()
      options(BioC_mirror = bioc_mirror)
      # 遍历所有 Bioc 子仓库，把包名全部拉出来
      repos <- BiocManager::repositories()
      all_bioc <- character(0)
      for (r in repos) {
        contrib <- utils::contrib.url(r)
        found <- tryCatch(
          rownames(utils::available.packages(contriburl = contrib)),
          error = function(e) NULL
        )
        all_bioc <- c(all_bioc, found)
      }
      .smartpkg_cache$bioc_pkgs <- unique(all_bioc)
    }, error = function(e) .smartpkg_cache$bioc_pkgs <- character(0))
  }

  .smartpkg_cache$warmed <- TRUE
}

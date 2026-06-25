#' 检测 R 包的来源类型
#'
#' 自动识别包的来源：CRAN、Bioconductor、GitHub、本地或未知。
#'
#' @param pkg 包名字符串
#' @return list，包含 source, pkg 及其他来源特定字段
#' @export
detect_pkg_source <- function(pkg) {
  if (is.null(pkg) || is.na(pkg) || nchar(trimws(pkg)) == 0) {
    stop("Package name is empty")
  }

  pkg <- trimws(pkg)

  # 检查显式命名空间
  if (grepl("::", pkg, fixed = TRUE)) {
    parts <- strsplit(pkg, "::", fixed = TRUE)[[1]]
    namespace <- tolower(trimws(parts[1]))
    pkg_name <- trimws(parts[2])

    if (namespace == "cran") {
      return(list(source = "cran", pkg = pkg_name))
    } else if (namespace == "bioc") {
      return(list(source = "bioc", pkg = pkg_name))
    } else if (namespace == "github") {
      return(parse_github(pkg_name))
    } else {
      return(list(source = "unknown", pkg = pkg))
    }
  }

  # 检查本地路径（.tar.gz 结尾或存在目录/文件）
  if (grepl("\\.tar\\.gz$", pkg) || file.exists(pkg)) {
    return(list(source = "local", pkg = pkg))
  }

  # 检查 GitHub 格式 (username/repo)
  if (grepl("^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+$", pkg)) {
    return(parse_github(pkg))
  }

  # 是本地路径但不存在文件，判断为 GitHub（有点模糊，但本地路径存在文件已在上面检查了）
  if (grepl("/", pkg, fixed = TRUE)) {
    # 可能是路径不存在的本地包
    return(list(source = "local", pkg = pkg))
  }

  # 纯包名 → 默认走 CRAN
  list(source = "cran", pkg = pkg)
}

#' 解析 GitHub 包标识
#' @param pkg username/repo 格式
#' @return list
parse_github <- function(pkg) {
  parts <- strsplit(pkg, "/", fixed = TRUE)[[1]]
  list(
    source = "github",
    pkg = pkg,
    username = parts[1],
    repo = parts[2]
  )
}

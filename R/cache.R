#' 缓存文件路径
cache_path <- function() {
  dir <- file.path(Sys.getenv("HOME", unset = "~"), ".R")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(dir, "smartpkg_mirror_cache")
}

#' 缓存有效期（秒）
CACHE_TTL <- 86400  # 24 小时

#' 写入镜像缓存
#' @param mirror_data list，包含 mirror_url, timestamp, all_mirrors_tested, candidate_count
write_cache <- function(mirror_data) {
  saveRDS(mirror_data, file = cache_path())
}

#' 读取镜像缓存
#' @return list 或 NULL（无缓存时）
read_cache <- function() {
  path <- cache_path()
  if (file.exists(path)) {
    readRDS(path)
  } else {
    NULL
  }
}

#' 检查缓存是否有效
#' @return logical
is_cache_valid <- function() {
  cached <- read_cache()
  if (is.null(cached)) return(FALSE)
  if (is.null(cached$timestamp)) return(FALSE)
  elapsed <- difftime(Sys.time(), cached$timestamp, units = "secs")
  as.numeric(elapsed) < CACHE_TTL
}

#' 手动刷新缓存（删除缓存文件）
#' @export
refresh_mirror_cache <- function() {
  path <- cache_path()
  if (file.exists(path)) {
    file.remove(path)
    message("Mirror cache cleared.")
  } else {
    message("No mirror cache found.")
  }
}

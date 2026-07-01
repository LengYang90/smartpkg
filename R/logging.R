#' Create a second-level timestamp for smartpkg output
smartpkg_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

#' Format a smartpkg output line
smartpkg_log_line <- function(...) {
  paste0("[", smartpkg_timestamp(), "] ", paste0(..., collapse = ""))
}

#' Emit a timestamped smartpkg message
smartpkg_message <- function(...) {
  message(smartpkg_log_line(...))
}

#' Emit a timestamped smartpkg warning
smartpkg_warning <- function(..., call. = FALSE) {
  warning(smartpkg_log_line(...), call. = call.)
}

#' Suppress BiocManager repository replacement chatter
suppress_biocmanager_repository_messages <- function(expr) {
  suppressMessages(expr)
}

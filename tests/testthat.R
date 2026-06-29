library(testthat)
library(smartpkg)

Sys.setenv(SMARTPKG_CACHE_DIR = file.path(tempdir(), "smartpkg-test-cache"))

test_check("smartpkg")

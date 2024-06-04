if (require("testthat", quietly = TRUE)) {
  pkg <- "RespChamberProc" # <-- Change to package name!
  #library(pkg, character.only = TRUE)
  #test_package(pkg)
  test_check(pkg)
} else {
  warning("cannot run unit tests -- package testthat is not available")
}
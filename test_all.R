library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(grapherator)
}

test_dir("tests/testthat")

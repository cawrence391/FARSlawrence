library(testthat)
library(FARS)
library(dplyr)
library(magrittr)
library(readr)
library(devtools)

load_all()

test_that("fars data loading is working", {
  b <- as.data.frame(fars_read_years(2013))
  expect_equal(length(b), 2)
})

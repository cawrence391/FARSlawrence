library(testthat)
library(FARS)

test_that("fars data loading is working", {
  b <- as.data.frame(fars_read_years(2013))
  expect_equal(length(b), 2)
})

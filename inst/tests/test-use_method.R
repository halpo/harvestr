library(harvestr)
library(testthat)

test_that("use_method", {
  source(system.file("examples", "use_method.R", package="harvestr"))
  x <- mr$new(x = 2L)
  x$x <- 2L
  name <- "Hello World!"
  x$name <- "Hello World!"

  
  expect_that(use_method("hello"), is_a("function"))
  expect_that(use_method(hello), is_a("function"))
  expect_that(use_method(hello, 1), is_a("function"))
  expect_that(use_method(hello)(x), is_a("character"))
  expect_that(use_method(times, 3)(x), equals(6))
})


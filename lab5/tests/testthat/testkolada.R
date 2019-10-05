context("kolada")


## User input
test_that("Wrong user input is detected from get_id()", {
  Kobj <- kolada$new()
  expect_error(Kobj$get_id("New york"))
  expect_error(Kobj$get_id("London"))
})


test_that("Wrong user input is detected from get_stats()", {
  Kobj <- kolada$new()
  expect_error(Kobj$get_stats("New york",1))
  expect_error(Kobj$get_stats("Ale", 5))
})

## User output
test_that("Wrong user output is detected from get_id()", {
  Kobj <- kolada$new()
  expect_true(is.character(Kobj$get_id("Ale")))
  expect_true(is.character(Kobj$get_id("Linköping")))
})

test_that("Wrong user output is detected from get_skola()", {
  Kobj <- kolada$new()
  expect_true(is.data.frame(Kobj$get_skola("Ale")))
  expect_true(is.data.frame(Kobj$get_skola("Linköping")))
})

test_that("Wrong user output is detected from get_municipality_list()", {
  Kobj <- kolada$new()
  expect_true(is.data.frame(Kobj$get_municipality_list()))
})

test_that("Wrong user output is detected from get_stats()", {
  Kobj <- kolada$new()
  expect_true(is.data.frame(Kobj$get_stats("Ale", 2)))
  expect_true(is.data.frame(Kobj$get_stats("Linköping", 4)))
})

## Testing that the function allways return a given value for a specific set of inputs.
test_that("Wrong user input is detected from get_id()", {
  Kobj <- kolada$new()
  expect_error(Kobj$get_id("Linkoping")=="0580")
  expect_true(Kobj$get_id("Ale")=="1440")
})



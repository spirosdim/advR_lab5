context("kolada")

kolada <- setRefClass("kolada")

Kobj <- kolada$new()



## User input
test_that("Wrong user input is detected from get_id()", {
  expect_error(K$get_id("New york"))
  expect_error(K$get_id("London"))
})


test_that("Wrong user input is detected from get_numb()", {
  expect_error(K$get_numb("New york",1))
  expect_error(K$get_numb("Ale", 5))
})

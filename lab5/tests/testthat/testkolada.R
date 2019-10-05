context("kolada")

kolada <- setRefClass("kolada")

Kobj <- kolada$new()



## User input
test_that("Wrong user input is detected from get_id()", {
  expect_error(Kobj$get_id("New york"))
  expect_error(Kobj$get_id("London"))
})


test_that("Wrong user input is detected from get_stats()", {
  expect_error(Kobj$get_stats("New york",1))
  expect_error(Kobj$get_stats("Ale", 5))
})

## Testing that the function allways return a given value for a specific set of inputs.
test_that("Wrong user input is detected from get_id()", {
  expect_error(Kobj$get_id("Linkoping")=="0580")
  expect_error(Kobj$get_id("Ale")=="1441")
})

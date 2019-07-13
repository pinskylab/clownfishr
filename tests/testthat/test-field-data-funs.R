context("Load clownfish table from database into R")

test_that("get_fish() pulls clownfish table into R as tibble/data frame"), {

  expect_type(get_fish(), tibble)


}

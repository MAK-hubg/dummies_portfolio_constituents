library(PortfolioConstituents)
context("To test non-exportable calculationDates function. The function is used in exportable functions that create dummies.")


test_that("To test if the function provides both the dates or periods of date included: ",
          {
  expect_equal(length(calculationDates("2019-12-31", "2020-06-30", "daily")), 183)
  expect_equal(length(calculationDates("2019-12-31", "2020-06-30", "weekly")), 27)
  expect_equal(length(calculationDates("2019-12-31", "2020-06-30", "monthly")), 7)
  expect_equal(length(calculationDates("2019-12-31", "2020-06-30", "quarterly")),3)
  expect_equal(length(calculationDates("2019-12-31", "2020-06-30", "annaully")), 2)
  }
)

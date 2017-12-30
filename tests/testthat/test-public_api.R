library(cryptor)
context("test-public_api.R")

# Global objects
get_coins_result <- get_coins()

test_that("get_api_limit works", {
  api_limit_result <- get_api_limit()
  expect_equal(class(api_limit_result), "list")
  expect_equal(length(api_limit_result), 2)
  expect_equal(class(api_limit_result[[1]]), c("tbl_df", "tbl", "data.frame"))
  expect_error(get_api_limit("min"))
})

test_that("get_coins works", {
  expect_equal(class(get_coins_result), c("tbl_df", "tbl", "data.frame"))
  expect_true(all(c("Id",
                    "Url",
                    "ImageUrl",
                    "Name",
                    "Symbol",
                    "CoinName",
                    "FullName",
                    "Algorithm",
                    "ProofType",
                    "FullyPremined",
                    "TotalCoinSupply",
                    "PreMinedValue",
                    "TotalCoinsFreeFloat",
                    "SortOrder",
                    "Sponsored") %in% names(get_coins_result)))
  expect_equal(ncol(get_coins_result), 15)
  expect_true(all(c("BTC", "ETH", "LTC") %in% get_coins_result$Name))
})

test_that("get_price works", {
  sample_coins <- get_coins_result$Symbol[as.numeric(get_coins_result$SortOrder) <= 10]
  sample_references <- c("USD", "EUR")
  get_price_result <- get_price(sample_coins, sample_references)
  expect_equal(dim(get_price_result), c(10, 3))
  expect_true(all(sample_coins %in% get_price_result$coin))
  expect_true(all(sample_references %in% names(get_price_result)))
})

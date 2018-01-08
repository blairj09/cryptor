#' Get number of API calls used and remaining in last hour or last second.
#'
#' \code{get_api_limit} returns a tibble detailing API calls made and API calls
#' remaining against established rate limits.
#'
#' @references \url{https://www.cryptocompare.com/api#requests}
#'
#' @param time character. Either "hour", "minute", or "second". Defaults to "hour".
#'
#' @return A single tibble with two rows - one for calls made and one for calls
#' remaining
#'
#' @examples
#' \dontrun{
#' # Get hour limit
#' get_api_limit()
#'
#' # Get second limit
#' get_api_limit(time = "second")
#' }
#' @export
get_api_limit <- function(time = "hour") {

  # Check parameters
  if (!time %in% c("hour", "minute", "second")) {
    stop("time must be either 'hour', 'minute', or 'second'.",
         call. = FALSE)
  }

  query_url <- glue::glue("https://min-api.cryptocompare.com/stats/rate/{time}/limit")

  query_resp  <- httr::GET(query_url)

  # Check for errors from API call
  api_errs(query_resp)

  # Extract and parse JSON from response
  query_cont <- get_response_content(query_resp)

  # Parse JSON from response into tibbles
  query_tbl <- query_cont[-1] %>%
    response_to_tibble() %>%
    dplyr::mutate(call_metric = c("calls_made", "calls_left")) %>%
    dplyr::select(call_metric, dplyr::everything())

  query_tbl
}

#' List all available coins
#'
#' \code{get_coins} provides general information about all coins available through
#' the API.
#'
#' @references https://www.cryptocompare.com/api#-api-data-coinlist-
#'
#' @return A tibble containing details for each coin available through the API
#'
#' @examples
#' \dontrun{
#' coins <- get_coins()
#' head(coins)
#' }
#' @export
get_coins <- function() {
  query_url <- "https://www.cryptocompare.com/api/data/coinlist"

  query_resp <- httr::GET(query_url)

  # Check for errors
  api_errs(query_resp)

  # Get content of result as text
  query_cont <- get_response_content(query_resp)

  # Parse data into tibble
  coin_tbl <- query_cont$Data %>%
    response_to_tibble()

  coin_tbl
}

#' Get price data for one or more coin(s)
#'
#' \code{get_price} provides price information about selected coins in terms
#' of other coins.
#'
#' @references https://www.cryptocompare.com/api#-api-data-price-
#'
#' @param fsyms character. 3 letter name(s) for coins to retreive price details for.
#' @param tsyms character. 3 letter name(s) for how price should be reported for
#' \code{fsyms}.
#' @param exchange character. Exchange to query. Defaults to 'CCCAGG'.
#' @param sign logical. Should the server sign the response? Defaults to FALSE
#' @param try_conversion logical. Should BTC conversion be used fsym is not trading
#' in tsym on specified exchange? Defaults to TRUE.
#' @param app_name character. Name of app to be passed in API request. Defaults
#' to NULL.
#'
#' @return A tibble containing price details about selected coin(s).
#' @examples
#' \dontrun{
#' # Get BTC price in USD
#' get_price("BTC", "USD")
#'
#' # Get BTC and ETH price in USD and EUR
#' get_price(c("BTC", "ETH"), c("USD", "EUR"))
#' }
#'
#' @export
get_price <- function(fsyms,
                      tsyms,
                      exchange = "CCCAGG",
                      sign = FALSE,
                      try_conversion = TRUE,
                      app_name = NULL) {
  base_url <- "https://min-api.cryptocompare.com/data/pricemulti"

  query_url <- httr::modify_url(base_url,
                                query = list(
                                  fsyms = glue::collapse(fsyms, sep = ","),
                                  tsyms = glue::collapse(tsyms, sep = ","),
                                  e = exchange,
                                  sign = tolower(sign),
                                  tryConversion = try_conversion
                                ))

  # If app_name is provided, append it to the url
  if (!is.null(app_name)) {
    query_url <- httr::modify_url(query_url,
                                  query = list(
                                    extraParams = app_name
                                  ))
  }

  query_resp <- httr::GET(query_url)

  # Check response for errors
  api_errs(query_resp)

  # Get content from request
  query_cont <- get_response_content(query_resp)

  # Parse response into tibble with requested coins as rows and requested prices as columns
  query_cont %>%
    response_to_tibble() %>%
    dplyr::mutate(coin = names(query_cont)) %>%
    dplyr::select(coin,
                  dplyr::everything())
}

#' Get full price details for one or more coin(s)
#'
#' \code{get_price_details} is Like \code{\link{get_price}} but provides a more
#' thorough summary of price data by including details like 24 volume and highs
#' and lows.
#'
#' @references https://www.cryptocompare.com/api#-api-data-price-
#'
#' @inheritParams get_price
#'
#' @return A tibble containing comprehensive price details for \code{fsyms} in
#' terms of \code{tsyms}.
#'
#' @examples
#' \dontrun{
#' # Get BTC price details in USD
#' get_price_details("BTC", "USD")
#'
#' # Get BTC and ETH price details in USD and EUR
#' get_price_details(c("BTC", "ETH"), c("USD", "EUR"))
#'
#' }
#' @export
get_price_details <- function(fsyms,
                              tsyms,
                              exchange = "CCCAGG",
                              sign = FALSE,
                              app_name = NULL,
                              try_conversion = TRUE) {
  base_url <- "https://min-api.cryptocompare.com/data/pricemultifull"

  query_url <- httr::modify_url(base_url,
                                query = list(
                                  fsyms = glue::collapse(fsyms, sep = ","),
                                  tsyms = glue::collapse(tsyms, sep = ","),
                                  e = exchange,
                                  sign = tolower(sign),
                                  tryConversion = try_conversion
                                ))

  # If app_name is provided, append it to the url
  if (!is.null(app_name)) {
    query_url <- httr::modify_url(query_url,
                                  query = list(
                                    extraParams = app_name
                                  ))
  }

  query_resp <- httr::GET(query_url)

  # Check for errors
  api_errs(query_resp)

  query_cont <- get_response_content(query_resp)

  price_tbl <- query_cont %>%
    purrr::pluck("RAW") %>%
    purrr::map(
      purrr::map,
      tibble::as_tibble
    ) %>%
    purrr::flatten() %>%
    Reduce(rbind, .) %>%
    purrr::map_df(readr::parse_guess,
                  na = c("", "NA", "N/A"))

  price_tbl
}

#' Get historical daily price data
#'
#' \code{get_historical_price} provides price information about a single currency
#' at the daily level.
#'
#' @references \url{https://www.cryptocompare.com/api#-api-data-histoday-}
#'
#' @param fsym character. 3 letter name for coin to retreive price history for.
#' @param tsym character. 3 letter name for how price should be reported for
#' \code{fsym}.
#' @param end_time character. Final date to retrieve data for in the format of yyyy-mm-dd
#' with optional hh:mm:ss (used when unit is set to "hour" or "minute"). Defaults to
#' \code{Sys.Date()}.
#' @param unit character. Either "day", "hour", or "minute" indicating the granularity of
#' the returned data. The default is 'day'. \strong{Note:} minute data is only available
#' for the past 7 days.
#' @param exchange character. Exchange to query. Defaults to 'CCCAGG'.
#' @param aggregate numeric. Data is reported every n units. Defaults to 1. Maximum
#' value of 30.
#' @param limit numeric. Number of records to retrive leading up to \code{end_date}.
#' If \code{all_data} is set to true, this is ignored. Defaults to 30 when \code{unit}
#' is "day", 168 when \code{unit} is "hour", and 1440 when \code{unit} is "minute".
#' Minimum of 1, maximum of 2000.
#' @param all_data logical. SHould all historical data for \code{fsym} be retreived?
#' Defaults to FALSE.
#' @param sign logical. Should the server sign the response? Defaults to FALSE
#' @param try_conversion logical. Should BTC conversion be used fsym is not trading
#' in tsym on specified exchange? Defaults to TRUE.
#' @param app_name character. Name of app to be passed in API request. Defaults
#' to NULL.
#'
#' @return A tibble containing daily price details for \code{fsym}
#'
#' @examples
#' \dontrun{
#' get_daily_price("BTC", "USD", "2017-10-31")
#' }
#'
#' @export
get_historical_price <- function(fsym,
                                 tsym,
                                 end_time = Sys.Date(),
                                 unit = "day",
                                 exchange = "CCCAGG",
                                 aggregate = 1,
                                 limit = NULL,
                                 all_data = FALSE,
                                 sign = FALSE,
                                 try_conversion = TRUE,
                                 app_name = NULL) {
  # Check parameters
  if (length(fsym) > 1 | class(fsym) != "character") {
    stop("fsym must be character vector of length 1.",
         call. = FALSE)
  }

  if (length(tsym) > 1 | class(tsym) != "character") {
    stop("tsym must be character vector of length 1.",
         call. = FALSE)
  }

  if (!is.numeric(aggregate) | aggregate < 1 | aggregate > 30) {
    stop("aggregate must be an integer between 1 and 30",
         call. = FALSE)
  }

  # Set limit based on unit
  if (is.null(limit)) {
    limit <- dplyr::case_when(unit == "day" ~ 30,
                              unit == "hour" ~ 168,
                              TRUE ~ 1440)
  } else if (!is.numeric(limit) | limit < 1 | limit > 2000) {
    stop("limit must be an integer between 1 and 2000",
         call. = FALSE)
  }

  if (!unit %in% c("day", "hour", "minute")) {
    stop("unit must be either 'day', 'hour', or 'minute'.",
         call. = FALSE)
  }

  base_url <- glue::glue("https://min-api.cryptocompare.com/data/histo{unit}")

  # Parse end_date into proper numeric format
  p_end_time <- lubridate::as_datetime(end_time) %>%
    as.numeric()

  # Build query
  query_url <- httr::modify_url(base_url,
                                query = list(
                                  fsym = fsym,
                                  tsym = tsym,
                                  e = exchange,
                                  limit = limit,
                                  aggregate = aggregate,
                                  allData = tolower(all_data),
                                  sign = tolower(sign),
                                  toTs = p_end_time
                                ))

  # If app_name is provided, append it to the url
  if (!is.null(app_name)) {
    query_url <- httr::modify_url(query_url,
                                  query = list(
                                    extraParams = app_name
                                  ))
  }

  # Get response
  query_resp <- httr::GET(query_url)

  # Check response for API errors
  api_errs(query_resp)

  # Get contents of response
  query_cont <- get_response_content(query_resp)

  # Return parsed tibble
  history_tbl <- query_cont$Data %>%
    response_to_tibble() %>%
    dplyr::mutate(time = lubridate::as_datetime(time)) %>%
    dplyr::rename(date = time)

  history_tbl
}

#' Get data for a currency pair
#'
#' \code{get_pair_snapshot} provides aggregated and individual data for the
#' provided currency pair in every exchange available.
#'
#' @references \url{https://www.cryptocompare.com/api#-api-data-coinsnapshot-}
#'
#' @param fsym character. 3 letter name for coin to retreive.
#' @param tsym character. 3 letter name for how details should be reported for
#' \code{fsym}.
#'
#' @return A list of 3 containing the following:
#' \describe{
#'   \item{coin_data}{A tibble containing general coin data}
#'   \item{aggregated_data}{A tibble containing aggregate data about the pair}
#'   \item{exchange_data}{A tibble containing exchange data for each available
#'        exchange and the provided pair}
#' }
#'
#' @examples
#' \dontrun{
#' # Get data on BTC and USD
#' get_pair_snapshot("BTC", "USD")
#' }
#'
#' @export
get_pair_snapshot <- function(fsym, tsym) {
  base_url <- "https://www.cryptocompare.com/api/data/coinsnapshot"

  query_url <- httr::modify_url(base_url,
                                query = list(
                                  fsym = fsym,
                                  tsym = tsym
                                ))

  query_resp <- httr::GET(query_url)

  api_errs(query_resp)

  query_cont <- get_response_content(query_resp)

  coin_data <- query_cont$Data[!purrr::map_lgl(query_cont$Data, purrr::is_list) &
                                 !purrr::map_lgl(query_cont$Data, purrr::is_null)] %>%
    tibble::as_tibble()

  aggregated_data <- query_cont$Data$AggregatedData %>%
    tibble::as_tibble() %>%
    purrr::map_df(readr::parse_guess,
                  na = c("", "NA", "N/A"))

  exchange_data <- query_cont$Data$Exchanges %>%
    response_to_tibble()

  list(
    coin_data = coin_data,
    aggregated_data = aggregated_data,
    exchange_data = exchange_data
  )
}

#' Get specific coin details by coin id.
#'
#' \code{get_coin_snapshot} returns a tibble of several coin details about the
#' provided id. These details include specifics about the coin, such as the algorithm
#' used, the url for the coin's website, the coin's twitter handle, coin start date,
#' and coin proof type.
#'
#' @references \url{https://www.cryptocompare.com/api#-api-data-coinsnapshotfullbyid-}
#'
#' @param id integer. Coin id. Can be retreived using \code{\link{get_coins}}.
#'
#' @return A tibble containing details about the specified coin.
#'
#' @examples
#' \dontrun{
#' # Get snapshot for bitcoin
#' get_coin_snapshot(1182)
#' }
#'
#' @export
get_coin_snapshot <- function(id) {
  base_url <- "https://www.cryptocompare.com/api/data/coinsnapshotfullbyid"

  query_url <- httr::modify_url(base_url,
                                query = list(
                                  id = id
                                ))

  query_resp <- httr::GET(query_url)

  api_errs(query_resp)

  query_cont <- get_response_content(query_resp)

  flat_cont <- query_cont$Data[1:3] %>%
    purrr::flatten()

  snapshot_tbl <- flat_cont[!purrr::map_lgl(flat_cont, purrr::is_list) &
              !purrr::map_lgl(flat_cont, purrr::is_null)] %>%
    tibble::as_tibble() %>%
    purrr::map_df(readr::parse_guess,
                  na = c("", "NA", "N/A"))

  snapshot_tbl
}

#' Gets social data for a given coin id.
#'
#' \code{get_social} returns a list containing several tibbles that provide
#' social details for the specified coin.
#'
#' @references \url{https://www.cryptocompare.com/api#-api-data-socialstats-}
#'
#' @inheritParams get_coin_snapshot
#'
#' @examples
#' \dontrun{
#' # Get bitcoin social data
#' get_social(1182)
#' }
#'
#' @export
get_social <- function(id) {
  base_url <- "https://www.cryptocompare.com/api/data/socialstats"

  query_url <- httr::modify_url(base_url,
                                query = list(
                                  id = id
                                ))

  query_resp <- httr::GET(query_url)

  api_errs(query_resp)

  query_cont <- get_response_content(query_resp)

  # CryptoCompare data
  crypto_compare <- query_cont$Data$CryptoCompare
  similar_items <- crypto_compare$SimilarItems %>%
    response_to_tibble()

  cryptopian_followers <- crypto_compare$CryptopianFollowers %>%
    response_to_tibble()

  page_views <- crypto_compare$PageViewsSplit %>%
    tibble::as_tibble() %>%
    tidyr::gather(key = "page",
                  value = "views")

  crypto_compare_summary <- crypto_compare[!purrr::map_lgl(crypto_compare, purrr::is_list)] %>%
    tibble::as_tibble() %>%
    purrr::map_df(readr::parse_guess,
                  na = c("", "NA", "N/A"))

  # Social media
  social_names <- c("Twitter", "Reddit", "Facebook")

  social_media <- query_cont$Data[social_names] %>%
    purrr::map(
      purrr::map_df,
      readr::parse_guess,
      na = c("", "NA", "N/A")
    )

  # Repo data
  query_cont$Data$CodeRepository$List %>%
    purrr::map_df(possibly_read)

}

#' Get top currencies by volume for a given currency.
#'
#' \code{get_top_pairs} returns a tibble containing the top currency pairs by
#' volume.
#'
#' @references \url{https://www.cryptocompare.com/api#-api-data-toppairs-}
#'
#' @param fsym character. Three letter symbol for currency of interest.
#' @param limit integer. Number of top pairs to return. Min of 1, max of 2000.
#' Defaults to 5.
#' @param sign logical. Should the server sign the response? Defaults to FALSE.
#'
#' @return A tibble containing the top currencies by volume for the specified
#' currency
#'
#' @examples
#' \dontrun{
#' # Get top 10 currencies for bitcoin
#' get_top_pairs("BTC", limit = 10)
#'
#' # Get top 20 currencies for ethereum
#' get_top_pairs("ETH", limit = 20)
#' }
#'
#' @export
get_top_pairs <- function(fsym, limit = 5, sign = FALSE) {
  base_url <- "https://min-api.cryptocompare.com/data/top/pairs"

  query_url <- httr::modify_url(base_url,
                                query = list(
                                  fsym = fsym,
                                  limit = limit,
                                  sign = tolower(sign)
                                ))

  query_resp <- httr::GET(query_url)

  api_errs(query_resp)

  query_cont <- get_response_content(query_resp)

  pairs_tbl <- query_cont$Data %>%
    response_to_tibble()

  pairs_tbl
}

#' Total API calls used and remaining.
#'
#' \code{get_api_limit} returns a tibble detailing API calls made and API calls
#' remaining against established rate limits.
#'
#' @references \url{https://www.cryptocompare.com/api#requests}
#'
#' @param time character. Either "hour", "minute", or "second". Defaults to "hour".
#'
#' @return A single tibble with two rows - one for calls made and one for calls
#'   remaining.
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
  check_params(time = time)

  query_url <- httr::modify_url(MIN_API_URL,
                                path = glue::glue("stats/rate/{time}/limit"))

  query_resp  <- httr::GET(query_url)

  # Check for errors from API call
  api_errs(query_resp)

  # Extract and parse JSON from response
  query_cont <- get_response_content(query_resp)

  # Parse JSON from response into tibble
  limit_tbl <- query_cont[-1] %>%
    response_to_tbl() %>%
    dplyr::mutate(call_metric = c("calls_made", "calls_left")) %>%
    dplyr::select(call_metric, dplyr::everything())

  limit_tbl
}

#' List all available exchanges.
#'
#' \code{get_exchanges} provides details on each exchange available through the
#' API
#'
#' @references \url{https://min-api.cryptocompare.com}
#'
#' @return A tibble containing details for each exchange available through the API.
#'
#' @examples
#' \dontrun{
#' exchanges <- get_exchanges()
#' head(exchanges)
#' }
#'
#' @export
get_exchanges <- function() {
  query_url <- httr::modify_url(MIN_API_URL,
                                path = "data/all/exchanges")

  query_resp <- httr::GET(query_url)

  # Check for errors
  api_errs(query_resp)

  # Get content
  query_cont <- get_response_content(query_resp)

  # Parse content into tibble
  exchange_tbl <- tibble(
    exchange = names(query_cont),
    coin = purrr::map(
      query_cont,
      names
    )
  ) %>%
    tidyr::unnest() %>%
    dplyr::mutate(market = purrr::flatten(query_cont)) %>%
    tidyr::unnest()

  exchange_tbl
}

#' List all available coins.
#'
#' \code{get_coins} provides general information about all coins available through
#' the CryptoCompare API.
#'
#' @references \url{https://www.cryptocompare.com/api#-api-data-coinlist-}
#'
#' @return A tibble containing details for each coin available through the API.
#'
#' @examples
#' \dontrun{
#' coins <- get_coins()
#' head(coins)
#' }
#'
#' @export
get_coins <- function() {
  query_url <- httr::modify_url(MIN_API_URL,
                                path = "data/all/coinlist")

  query_resp <- httr::GET(query_url)

  # Check for errors
  api_errs(query_resp)

  # Get content of result as text
  query_cont <- get_response_content(query_resp)

  # Parse data into tibble
  coin_tbl <- query_cont$Data %>%
    response_to_tbl()

  coin_tbl
}

#' Get price data for one or more coin(s).
#'
#' \code{get_price} provides price information about selected coins in terms
#' of other coins.
#'
#' @references \url{https://www.cryptocompare.com/api#-api-data-price-}
#'
#' @param fsyms character. 3 letter name(s) for coins to retreive price details for.
#' @param tsyms character. 3 letter name(s) for how price should be reported for
#'   \code{fsyms}.
#' @param exchange character. Exchange to query. Defaults to 'CCCAGG'.
#' @param sign logical. Should the server sign the response? Defaults to FALSE.
#' @param try_conversion logical. Should BTC conversion be used if \code{fsym}
#'   is not trading in \code{tsym} on specified exchange? Defaults to TRUE.
#' @param app_name character. Name of app to be passed in API request. Defaults
#'   to NULL.
#'
#' @return A tibble containing price details about selected coin(s).
#'
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
  check_params(fsyms = fsyms,
               tsyms = tsyms,
               exchange = exchange,
               sign = sign,
               try_conversion = try_conversion,
               app_name = app_name)

  # Ensure fsyms and tsyms are upper case
  fsyms <- toupper(fsyms)
  tsyms <- toupper(tsyms)
  query_url <- httr::modify_url(MIN_API_URL,
                                path = "data/pricemulti",
                                query = list(
                                  fsyms = glue::collapse(fsyms, sep = ","),
                                  tsyms = glue::collapse(tsyms, sep = ","),
                                  e = exchange,
                                  sign = tolower(sign),
                                  tryConversion = tolower(try_conversion),
                                  extraParams = app_name
                                ))

  query_resp <- httr::GET(query_url)

  # Check response for errors
  api_errs(query_resp)

  # Get content from request
  query_cont <- get_response_content(query_resp)

  # Parse response into tibble with requested coins as rows and requested prices as columns
  price_tbl <- query_cont %>%
    tibble::as_tibble() %>%
    tidyr::unnest() %>%
    dplyr::mutate(tosymbol = names(query_cont[[1]])) %>%
    tidyr::gather(
      key = fromsymbol,
      value = price,
      -tosymbol
    ) %>%
    dplyr::select(
      fromsymbol,
      dplyr::everything()
    )

  price_tbl
}

#' Get full price details for one or more coin(s).
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
#' }
#'
#' @export
get_price_details <- function(fsyms,
                              tsyms,
                              exchange = "CCCAGG",
                              sign = FALSE,
                              try_conversion = TRUE,
                              app_name = NULL) {
  check_params(fsyms = fsyms,
               tsyms = tsyms,
               exchange = exchange,
               sign = sign,
               try_conversion = try_conversion,
               app_name = app_name)
  query_url <- httr::modify_url(MIN_API_URL,
                                path = "data/pricemultifull",
                                query = list(
                                  fsyms = glue::collapse(fsyms, sep = ","),
                                  tsyms = glue::collapse(tsyms, sep = ","),
                                  e = exchange,
                                  sign = tolower(sign),
                                  tryConversion = tolower(try_conversion),
                                  extraParams = app_name
                                ))

  query_resp <- httr::GET(query_url)

  # Check for errors
  api_errs(query_resp)

  query_cont <- get_response_content(query_resp)

  price_tbl <- query_cont$RAW %>%
    purrr::flatten() %>%
    response_to_tbl()

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
#' \code{Sys.time()}.
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
                                 end_time = Sys.time(),
                                 unit = "day",
                                 exchange = "CCCAGG",
                                 aggregate = 1,
                                 limit = NULL,
                                 all_data = FALSE,
                                 sign = FALSE,
                                 try_conversion = TRUE,
                                 app_name = NULL) {
  check_params(fsym = fsym,
               tsym = tsym,
               end_time = end_time,
               unit = unit,
               exchange = exchange,
               aggregate = aggregate,
               limit = limit,
               all_data = all_data,
               sign = sign,
               try_conversion = try_conversion,
               app_name = app_name)

  # Set limit based on unit
  if (is.null(limit)) {
    limit <- dplyr::case_when(unit == "day" ~ 30,
                              unit == "hour" ~ 168,
                              TRUE ~ 1440)
  }

  # Parse end_date into proper numeric format
  p_end_time <- lubridate::as_datetime(end_time) %>%
    as.numeric() %>%
    floor()

  # Build query
  query_url <- httr::modify_url(MIN_API_URL,
                                path = glue::glue("data/histo{unit}"),
                                query = list(
                                  fsym = fsym,
                                  tsym = tsym,
                                  e = exchange,
                                  limit = limit,
                                  aggregate = aggregate,
                                  allData = tolower(all_data),
                                  sign = tolower(sign),
                                  tryConversion = tolower(try_conversion),
                                  toTs = p_end_time,
                                  extraParams = app_name
                                ))

  # Get response
  query_resp <- httr::GET(query_url)

  # Check response for API errors
  api_errs(query_resp)

  # Get contents of response
  query_cont <- get_response_content(query_resp)

  # Return parsed tibble
  history_tbl <- query_cont$Data %>%
    response_to_tbl()

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
  check_params(fsym = fsym,
               tsym = tsym)

  query_url <- httr::modify_url(API_URL,
                                path = "api/data/coinsnapshot",
                                query = list(
                                  fsym = fsym,
                                  tsym = tsym
                                ))

  query_resp <- httr::GET(query_url)

  api_errs(query_resp)

  query_cont <- get_response_content(query_resp)

  coin_data <- query_cont$Data[!purrr::map_lgl(query_cont$Data, purrr::is_list) &
                                 !purrr::map_lgl(query_cont$Data, purrr::is_null)] %>%
    as_clean_tbl()

  aggregated_data <- query_cont$Data$AggregatedData %>%
    as_clean_tbl()

  exchange_data <- query_cont$Data$Exchanges %>%
    response_to_tbl()

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
  check_params(id = id)
  query_url <- httr::modify_url(API_URL,
                                path = "api/data/coinsnapshotfullbyid",
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
    as_clean_tbl() %>%
    dplyr::mutate(start_date = lubridate::mdy(start_date))

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
#' @return A list with the following structure:
#' \itemize{
#'   \item \strong{similar_items} A tibble outlining coins similar to the coin provided.
#'   \item \strong{cryptopian_followers} A tibble containing details about followers of
#'     the provided coin on cryptocompare.
#'   \item \strong{page_views} A tibble outlining number of page views for the coin on
#'     different pages.
#'   \item \strong{crypto_compare_summary} Aggergated cryptocompare data for the given coin.
#'   \item \strong{social_media} A list containing the following three elements:
#'   \itemize{
#'     \item \strong{Twitter} A tibble containing Twitter data.
#'     \item \strong{Reddit} A tibble containing Reddit data.
#'     \item \strong{Facebook} A tibble containing Facebook data.
#'   }
#'   \item \strong{repo_summary} A tibble with details about various code repositories
#'     associated with the given coin.
#' }
#'
#' @examples
#' \dontrun{
#' # Get bitcoin social data
#' get_social(1182)
#' }
#'
#' @export
get_social <- function(id) {
  # TODO: The API doesn't return an error when an invalid ID is passed in
  check_params(id = id)
  query_url <- httr::modify_url(API_URL,
                                path = "api/data/socialstats",
                                query = list(
                                  id = id
                                ))

  query_resp <- httr::GET(query_url)

  api_errs(query_resp)

  query_cont <- get_response_content(query_resp)

  # CryptoCompare data
  crypto_compare <- query_cont$Data$CryptoCompare
  similar_items <- crypto_compare$SimilarItems %>%
    response_to_tbl()

  cryptopian_followers <- crypto_compare$CryptopianFollowers %>%
    response_to_tbl()

  page_views <- crypto_compare$PageViewsSplit %>%
    as_clean_tbl() %>%
    tidyr::gather(key = "page",
                  value = "views")

  crypto_compare_summary <- crypto_compare[!purrr::map_lgl(crypto_compare, purrr::is_list)] %>%
    as_clean_tbl()

  # Social media
  social_names <- c("Twitter", "Reddit", "Facebook")

  social_media <- query_cont$Data[social_names] %>%
    purrr::map(as_clean_tbl)

  # Repo data
  repo_summary <- query_cont$Data$CodeRepository$List %>%
    # purrr::map(purrr::flatten) %>%
    purrr::map_df(as_clean_tbl)

  list(
    similar_items = similar_items,
    cryptopian_followers = cryptopian_followers,
    page_views = page_views,
    crypto_compare_summary = crypto_compare_summary,
    social_media = social_media,
    repo_summary = repo_summary
  )
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
  check_params(fsym = fsym,
               limit = limit,
               sign = sign)

  query_url <- httr::modify_url(MIN_API_URL,
                                path = "data/top/pairs",
                                query = list(
                                  fsym = fsym,
                                  limit = limit,
                                  sign = tolower(sign)
                                ))

  query_resp <- httr::GET(query_url)

  api_errs(query_resp)

  query_cont <- get_response_content(query_resp)

  pairs_tbl <- query_cont$Data %>%
    response_to_tbl()

  pairs_tbl
}

# api_endpoints <- httr::GET("https://min-api.cryptocompare.com")
# api_endpoints <- httr::content(api_endpoints)
#
# coinlist <- httr::GET("https://min-api.cryptocompare.com/data/all/coinlist")
# coinlist <- httr::content(coinlist)
# coinlist$Data %>%
#   response_to_tbl()

#' Get weighted average price of coin pair based on any number of exchanges.
#'
#' \code{get_average_price} returns the current trading details
#' (price, vol, open, high, low, etc) of a given coin pair as a volume weighted
#' average based on provided exchanges.
#'
#' @references \url{https://min-api.cryptocompare.com}
#'
#' @param fsym character. 3 letter name for coin to retreive price history for.
#' @param tsym character. 3 letter name for how price should be reported for
#' \code{fsym}.
#' @param exchange character. Exchange to query. Defaults to 'CCCAGG'.
#' @param sign logical. Should the server sign the response? Defaults to FALSE
#' @param try_conversion logical. Should BTC conversion be used fsym is not trading
#' in tsym on specified exchange? Defaults to TRUE.
#' @param app_name character. Name of app to be passed in API request. Defaults
#' to NULL.
#'
#' @return
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
get_average_price <- function(fsym,
                              tsym,
                              exchange,
                              sign = FALSE,
                              try_conversion = TRUE,
                              app_name = NULL) {
  check_params(fsym = fsym,
               tsym = tsym,
               exchange = exchange,
               sign = sign,
               try_conversion = try_conversion,
               app_name = app_name)

  query_url <- httr::modify_url(MIN_API_URL,
                                path = "data/generateAvg",
                                query = list(
                                  fsym = fsym,
                                  tsym = tsym,
                                  e = glue::collapse(exchange, sep = ","),
                                  sign = tolower(sign),
                                  tryConversion = tolower(try_conversion),
                                  extraParams = app_name
                                ))
  "https://min-api.cryptocompare.com/data/generateAvg?fsym=BTC&tsym=USD&e=Coinbase,Kraken,Bitstamp,Bitfinex"
}

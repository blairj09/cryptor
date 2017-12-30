#' Get number of API calls used in last hour or last second.
#'
#' \code{get_api_limit} returns a list of two tibbles detailing API calls made
#' and API calls remaining against second and hour rate limits.
#'
#' @references https://www.cryptocompare.com/api#requests
#'
#' @param time character. Either "hour" or "second". Defaults to "hour".
#'
#' @return A list of two tibbles detailing calls made and calls remaining against
#' the API limit
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
  if (!time %in% c("hour", "second")) {
    stop("time must be either 'hour' or 'second'.")
  }

  query_url <- glue::glue("https://min-api.cryptocompare.com/stats/rate/{time}/limit")

  query_resp  <- httr::GET(query_url)

  # Check for errors from API call
  api_errs(query_resp)

  query_cont <- httr::content(query_resp)

  query_tbls <- query_cont[-1] %>%
    purrr::map(tibble::as_tibble)

  names(query_tbls) <- glue::glue("{time}_{names(query_tbls)}")

  query_tbls
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
  query_url <- "https://www.cryptocompare.com/api/data/coinlist/"

  query_resp <- httr::GET(query_url)

  # Check for errors
  api_errs(query_resp)

  # Parse data into tibble
  coin_tbl <- httr::content(query_resp) %>%
    purrr::pluck("Data") %>%
    purrr::map_df(tibble::as_tibble)

  # Convert to data.frame to replace N/A with NA
  coin_df <- as.data.frame(coin_tbl)
  coin_df[coin_df == "N/A"] <- NA
  coin_tbl <- as_tibble(coin_df)

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
#' @param try_conversion logical. Should conversion be used if exact comparison
#' isn't available? Defaults to TRUE.
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
#' get_price(c("BTC", "ETH"), c("USD", "EUR))
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

  # Build url based on function parameters
  query_url <- glue::glue("{base_url}?fsyms={glue::collapse(fsyms, sep = ',')}&tsyms={glue::collapse(tsyms, sep = ',')}&e={exchange}&sign={tolower(sign)}&tryConversion={tolower(try_conversion)}")

  # If app_name is provided, append it to the url
  if (!is.null(app_name)) {
    query_url <- glue::glue("{query_url}&extraParams={app_name}")
  }

  query_resp <- httr::GET(query_url)

  # Check response for errors
  api_errs(query_resp)

  # Get content from request
  query_cont <- httr::content(query_resp)

  # Parse response into tibble with requested coins as rows and requested prices as columns
  query_cont %>%
    purrr::map_df(as_tibble) %>%
    dplyr::mutate(coin = names(price_content)) %>%
    dplyr::select(coin,
                  dplyr::everything())
}

#' Get full price details for one or more coin(s)
#'
#' \code{get_price_details} is Like code{\link{get_price}} but provides a more
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
#' get_price_details(c("BTC", "ETH"), c("USD", "EUR))
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

  # Build url based on function parameters
  query_url <- glue::glue("{base_url}?fsyms={glue::collapse(fsyms, sep = ',')}&tsyms={glue::collapse(tsyms, sep = ',')}&e={exchange}&sign={tolower(sign)}")

  # If app_name is provided, append it to the url
  if (!is.null(app_name)) {
    query_url <- glue::glue("{query_url}&extraParams={app_name}")
  }

  query_resp <- httr::GET(query_url)

  # Check for errors
  api_errs(query_resp)

  query_cont <- httr::content(query_resp)

  price_tbl <- query_cont %>%
    purrr::pluck("RAW") %>%
    purrr::map(
      purrr::map,
      tibble::as_tibble
    ) %>%
    purrr::flatten() %>%
    Reduce(rbind, .)

  price_tbl
}

#' Get historical daily price data
#'
#' \code{get_daily_price} provides price information about a single currency
#' at the daily level.
#'
#' @references https://www.cryptocompare.com/api#-api-data-histoday-
#'
#' @param fsym character. 3 letter name for coin to retreive price history for.
#' @param tsyms character. 3 letter name for how price should be reported for
#' \code{fsym}.
#' @param end_time character. Final date to retrieve data for in the format of yyyy-mm-dd
#' with optional hh:mm (used when unit is set to hour).
#' @param unit character. Either 'day' or 'hour' indicating the granularity of
#' the returned data. The default is 'day'.
#' @param exchange character. Exchange to query. Defaults to 'CCCAGG'.
#' @param aggregate numeric. Data is reported every n days. Defaults to 1. Maximum
#' value of 30.
#' @param limit numeric. Number of records to retrive leading up to \code{end_date}.
#' If \code{all_data} is set to true, this is ignored. Defaults to 30 when \code{unit}
#' is 'day' and 168 when \code{unit} is 'hour'. Minimum of 1, maximum of 2000.
#' @param all_data logical. SHould all historical data for \code{fsym} be retreived?
#' Defaults to FALSE.
#' @param sign logical. Should the server sign the response? Defaults to FALS
#' @param try_conversion logical. Should conversion be used if exact comparison
#' isn't available? Defaults to TRUE.
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
                                 end_time,
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
    stop("fsym must be character vector of length 1.")
  }

  if (length(tsym) > 1 | class(tsym) != "character") {
    stop("tsym must be character vector of length 1.")
  }

  if (!is.numeric(aggregate) | aggregate < 1 | aggregate > 30) {
    stop("aggregate must be an integer between 1 and 30")
  }

  # Set limit based on unit
  if (is.null(limit)) {
    limit <- ifelse(unit == "day", 30, 168)
  } else if (!is.numeric(limit) | limit < 1 | limit > 2000) {
    stop("limit must be an integer between 1 and 2000")
  }

  if (!unit %in% c("day", "hour")) {
    stop("unit must be either 'day' or 'hour'." )
  }

  # https://min-api.cryptocompare.com/data/histoday?fsym=ETH&tsym=BTC&limit=60&aggregate=1&toTs=1452680400&extraParams=your_app_name
  base_url <- glue::glue("https://min-api.cryptocompare.com/data/histo{unit}")

  # Parse end_date into proper numeric format
  p_end_time <- as.POSIXct(end_time) %>%
    as.numeric()

  # Build query
  query_url <- glue::glue("{base_url}?fsym={fsym}&tsym={tsym}&e={exchange}&limit={limit}&aggregate={aggregate}&allData={tolower(all_data)}&sign={tolower(sign)}&toTs={p_end_time}")

  # Add app_name if it is included
  if (!is.null(app_name)) {
    query_url <- glue::glue("{query_url}&extraParams={app_name}")
  }

  # Get response
  query_resp <- httr::GET(query_url)

  # Check response for API errors
  api_errs(query_resp)

  # Get contents of response
  query_cont <- httr::content(query_resp)

  # Return parsed tibble
  query_cont$Data %>%
    purrr::map_df(tibble::as_tibble) %>%
    dplyr::mutate(time = as.Date(as.POSIXct(time, origin = "1970-01-01"))) %>%
    dplyr::rename(date = time)
}


get_social <- function(id) {
  # https://www.cryptocompare.com/api/data/socialstats/?id=1182
  base_url <- "https://www.cryptocompare.com/api/data/socialstats/"

  query_url <- glue::glue("{base_url}?id={id}")

  query_resp <- httr::GET(query_url)

  api_errs(query_resp)

  query_cont <- httr::content(query_resp)


}

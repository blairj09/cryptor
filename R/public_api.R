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
#' @param fsyms character vector containing the 3 letter name(s) for coins to
#' retreive price details for.
#' @param tsyms character vector containing the 3 letter name(s) for how price
#' should be reported for \code{fsyms}.
#' @param exchange character vector of length 1 indicating which exchange should
#' be queried for pricing data.
#' @param sign logical. Should the server sign the response? Defaults to FALSE
#' @param app_name character. Name of app to be passed in API request. Defaults
#' to NULL.
#' @param try_conversion logical. Should conversion be used if exact comparison
#' isn't available? Defaults to TRUE.
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
                      app_name = NULL,
                      try_conversion = TRUE) {
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

get_day_price <- function(fsym,
                          tsym,
                          time_from,
                          time_to,
                          exchange = "CCCAGG",
                          aggregate = 1,
                          limit = 100,
                          all_data = FALSE,
                          sign = FALSE,
                          app_name = NULL,
                          try_conversion = TRUE) {
  # Check parameters
  if (length(fsym) > 1 | class(fsym) != "character") {
    stop("fsym must be character vector of length 1.")
  }

  if (length(tsym) > 1 | class(tsym) != "character") {
    stop("tsym must be character vector of length 1.")
  }

  # https://min-api.cryptocompare.com/data/histoday?fsym=ETH&tsym=BTC&limit=60&aggregate=1&toTs=1452680400&extraParams=your_app_name
  base_url <- "https://min-api.cryptocompare.com/data/histoday"

  # Parse dates into proper numeric format
  p_ts <- as.POSIXct(to_ts) %>%
    as.numeric()

  # Build query
  query_url <- glue::glue("{base_url}?fsym={fsym}&tsym={tsym}&e={exchange}&limit={limit}&aggregate={aggregate}&allData={tolower(all_data)}&sign={tolower(sign)}&toTs={p_ts}")

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
}



#' Get price data at a specific timestamp (hour).
get_hour_price <- function(fsym,
                           tsym,
                           to_ts,
                           exchange = "CCCAGG",
                           aggregate = 1,
                           limit = 100,
                           sign = FALSE,
                           app_name = NULL,
                           try_conversion = TRUE) {
  # Check parameters
  if (length(fsym) > 1 | class(fsym) != "character") {
    stop("fsym must be character vector of length 1.")
  }

  if (length(tsym) > 1 | class(tsym) != "character") {
    stop("tsym must be character vector of length 1.")
  }



  # https://min-api.cryptocompare.com/data/histohour?fsym=BTC&tsym=USD&limit=60&aggregate=3&e=CCCAGG
  base_url <- "https://min-api.cryptocompare.com/data/histohour"

  # Build query URL
  query_url <- glue::glue("{base_url}?fsym={fsym}&tsym={tsym}&")
}

get_social <- function(id) {
  # https://www.cryptocompare.com/api/data/socialstats/?id=1182
  base_url <- "https://www.cryptocompare.com/api/data/socialstats/"

  query_url <- glue::glue("{base_url}?id={id}")

  social_list <- httr::GET(query_url)

  api_errs(social_list)

  social_content <- httr::content(social_list)


}


# Social data playground
social_content$Data

# How are repositories selected?
social_content$Data$CodeRepository$List %>%
  map_chr("last_update") %>%
  as.numeric() %>%
  order()


# Internal functions for crypto package ---------------------------------------

#' Parse API response
#'
#' \code{get_response_content} extracts the content from an API response and
#' converts the JSON response into an R list.
#'
#' @param api_response API response returned from a \code{\link[httr]{GET}} request
#'
#' @return A list in R parsed from the JSON of the \code{api_response}.
#'
#' @noRd
get_response_content <- function(api_response) {
  httr::content(api_response,
                type = "text",
                encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyDataFrame = FALSE)
}

#' Check API response for common errors
#'
#' \code{api_errs} is a helper function designed to check API responses for
#' common errors.
#'
#' @inheritParams get_response_content
#'
#' @return Does not return anything - called exclusively for side effects
#'
#' @noRd
api_errs <- function(api_response) {
  if (httr::status_code(api_response) != 200) {
    stop("API returned status code: ",
         httr::status_code(coin_list),
         call. = FALSE)
  }

  if (httr::http_type(api_response) != "application/json") {
    stop("API returned type ",
         httr::http_status(coin_list),
         " instead of the expected JSON",
         call. = FALSE)
  }

  api_content <- get_response_content(api_response)
  if ("Response" %in% names(api_content)) {
    if (api_content$Response == "Error") {
      stop(api_content$Message,
           "; ",
           api_content$ErrorsSummary,
           call. = FALSE)
    }
  }
}

possibly_read <- purrr::possibly(
  ~readr::parse_guess(., na = c("", "NA", "N/A")),
  otherwise = NULL
)

#' Parse API response into a tibble
#'
#' \code{response_to_tibble} takes a nested list returned from an API and turns
#' it into a tibble. It uses \code{\link[readr]{parse_guess}} to parse the list
#' data into appropriate R types.
#'
#' @param parsed_response API response that has been parsed into an R list
#'
#' @return A tibble of nested list.
#' @noRd
response_to_tibble <- function(parsed_response) {
  tbl_response <- parsed_response %>%
    purrr::map_df(tibble::as_tibble) %>%
    purrr::map_df(readr::parse_guess,
                  na = c("", "NA", "N/A"))

  # Parse LASTUPDATE to datetime
  if ("LASTUPDATE" %in% names(tbl_response)) {
    tbl_response %>%
      mutate(LASTUPDATE = lubridate::as_datetime(LASTUPDATE))
  }

  tbl_response
}

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
#' @return Returns TRUE invisibly
#'
#' @noRd
api_errs <- function(api_response) {
  if (httr::status_code(api_response) != 200) {
    stop("API returned status code: ",
         httr::status_code(api_response),
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

  invisible(TRUE)
}

check_params <- function(time,
                         fsym,
                         tsym,
                         fsyms,
                         tsyms,
                         exchange,
                         sign,
                         try_conversion,
                         app_name,
                         end_time,
                         unit,
                         aggregate,
                         limit,
                         all_data,
                         id) {
  if (!missing(time)) {
    if (length(time) > 1) {
      stop("time must be either 'hour', 'minute', or 'second'.",
           call. = FALSE)
    }

    if (!time %in% c("hour", "minute", "second")) {
      stop("time must be either 'hour', 'minute', or 'second'.",
           call. = FALSE)
    }
  }

  if (!missing(fsym)) {
    if (!purrr::is_character(fsym) | length(fsym) > 1) {
      stop("fsym must be a character vector of length 1 (ie 'BTC')",
           call. = FALSE)
    }
  }

  if (!missing(tsym)) {
    if (!purrr::is_character(tsym) | length(tsym) > 1) {
      stop("tsym must be a character vector of length 1 (ie 'USD')",
           call. = FALSE)
    }
  }

  if (!missing(fsyms)) {
    if (!purrr::is_character(fsyms)) {
      stop("fsyms must be a character vector",
           call. = FALSE)
    }
  }

  if (!missing(tsyms)) {
    if (!purrr::is_character(tsyms)) {
      stop("tsyms must be a character vector",
           call. = FALSE)
    }
  }

  if (!missing(exchange)) {
    if (!purrr::is_character(exchange) | length(exchange) > 1) {
      stop("exchange must be a chracter vector of length 1 (ie 'CCCAGG')",
           call. = FALSE)
    }
  }

  if (!missing(sign)) {
    if (!purrr::is_logical(sign) | length(sign) > 1) {
      stop("sign must be a logical vector of length 1 (ie TRUE)",
           call. = FALSE)
    }
  }

  if (!missing(try_conversion)) {
    if (!purrr::is_logical(try_conversion) | length(try_conversion) > 1) {
      stop("try_conversion must be a logical vector of length 1 (ie TRUE)",
           call. = FALSE)
    }
  }

  if (!missing(app_name)) {
    if (!is.null(app_name) & (!purrr::is_character(app_name) | length(app_name) > 1)) {
      stop("app_name must be either NULL or a character vector of length 1")
    }
  }

  if (!missing(end_time)) {
    if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", end_time)) {
      stop("end_time must be a string matching the pattern of 'yyyy-mm-dd hh:mm:ss' where 'hh:mm:ss' is optional",
           call. = FALSE)
    }
  }

  if (!missing(unit)) {
    if (!unit %in% c("day", "hour", "minute")) {
      stop("unit must be one of 'day', 'hour', or 'minute'",
           call. = FALSE)
    }
  }

  if (!missing(aggregate)) {
    if (!purrr::is_bare_numeric(aggregate) | aggregate < 1 | aggregate > 30) {
      stop("aggregate must be a numeric vector of length 1 between 1 and 30",
           call. = FALSE)
    }
  }

  if (!missing(limit)) {
    if (!is.null(limit)) {
      if (!purrr::is_bare_numeric(limit) | limit < 1 | limit > 2000) {
        stop("limit must be a numeric vector of length 1 between 1 and 2000",
             call. = FALSE)
      }
    }
  }

  if (!missing(all_data)) {
    if (!purrr::is_logical(all_data) | length(all_data) > 1) {
      stop("all_data must be a logical vector of length 1 (ie TRUE)",
           call. = FALSE)
    }
  }

  if (!missing(id)) {
    if (!purrr::is_bare_numeric(id) | length(id) > 1) {
      stop("id must be a numeric vector of length 1",
           call. = FALSE)
    }
  }
}

#' Possibly read a set of values
#'
#' \code{possibly_read} uses \code{\link[readr]{parse_guess}} and wraps it in
#' \code{\link[purrr]{possibly}} to create a safe parser for objects.
#'
#' @inheritParams readr::parse_guess
#'
#' @return A parsed version of the input vector.
#'
#' @noRd
possibly_read <- purrr::possibly(
  ~readr::parse_guess(., na = c("", "NA", "N/A")),
  otherwise = NA
)

#' Clean up a tbl parsed from API response
#'
#' \code{parse_date_columns} takes a tbl and converts column names to snake case
#' and then parses the date columns that are integers into POSIXct objects. It also
#' converts columns to the proper type using \code{\link{possibly_read}}.
#'
#' @param tbl Tibble parsed from API response
#'
#' @return The input tbl with specified columns cleaned and updated appropriately.
#'
#' @noRd
as_clean_tbl <- function(tbl) {
  tbl %>%
    purrr::map_df(possibly_read) %>%
    janitor::clean_names(case = "snake") %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches("update|^time")),
      dplyr::funs(lubridate::as_datetime)
  )
}

#' Parse API response into a tibble
#'
#' \code{response_to_tbl} takes a nested list returned from an API and turns
#' it into a tibble. It uses \code{\link[readr]{parse_guess}} to parse the list
#' data into appropriate R types.
#'
#' @param parsed_response API response that has been parsed into an R list
#'
#' @return A tibble of a nested list.
#'
#' @noRd
response_to_tbl <- function(parsed_response) {
  tbl_response <- parsed_response %>%
    purrr::map_df(tibble::as_tibble) %>%
    as_clean_tbl()

  tbl_response
}

MIN_API_URL <- "https://min-api.cryptocompare.com"
API_URL <- "https://www.cryptocompare.com"
PUBLIC_KEY <- "a0f4f688350018ad1b9785991c0bde5f704b005dc79972b114dbed4a615a983710bfc647ebe5a320daa28771dce6a2d104f5efa2e4a85ba3760b76d46f8571ca"

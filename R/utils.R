# Internal functions for crypto package ---------------------------------------

api_errs <- function(api_return) {
  if (httr::status_code(api_return) != 200) {
    stop("API returned status code: ",
         httr::status_code(coin_list))
  }

  if (httr::http_type(api_return) != "application/json") {
    stop("API returned type ",
         httr::http_status(coin_list),
         " instead of the expected JSON")
  }

  api_content <- httr::content(api_return)
  if ("Response" %in% names(api_content)) {
    if (api_content$Response == "Error") {
      stop(api_content$Message)
    }
  }
}

get_response_content <- function(api_response) {
  httr::content(api_response,
                type = "text",
                encoding = "UTF-8") %>%
    jsonlite::fromJSON()
}

response_to_tibble <- function(parsed_response) {
  parsed_response %>%
    purrr::map_df(tibble::as_tibble) %>%
    purrr::map_df(readr::parse_guess,
                  na = c("", "NA", "N/A"))
}

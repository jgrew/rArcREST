#' Get available services from arcigs directory
#'
#' @description
#' \code{get_service_directory} retrieves available feature services from a directory url.
#'
#' @param url Required. The url the services will be retrieved from.
#'
#' @importFrom magrittr "%>%"

get_service_directory <- function(url) {

  if (check_token() == FALSE) {
    get_token()
  }

  token = getOption('rArcREST.token')

  ua <- httr::user_agent("http://github.com/jgrew/rArcREST")

  response <- httr::POST(
    url,
    query = list(
      token = token,
      referer = 'http://github.com/jgrew/rArcREST',
      f = 'json'
    ),
    ua,
    httr::add_headers(referer = 'http://github.com/jgrew/rArcREST')
  )

  if (httr::status_code(response) != 200) {
    stop(
      sprintf(
        'API service directory request failed [%s]',
        httr::status_code(response)
      )
    )
  }

  parsed <- httr::content(response, 'text') %>%
    jsonlite::fromJSON()

  if (any(grepl('error', names(parsed)))) {
    stop(
      sprintf(
        'API request failed [%s]\n%s\n<%s>',
        parsed$error$code,
        parsed$error$message,
        parsed$error$details
      ),
      call. = FALSE
    )
  } else {
    directory <- parsed  %>%
      .$services %>%
      dplyr::tbl_df()
  }

  return(directory)
}

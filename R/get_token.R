#' Get a token from arcgis.com
#'
#' @description
#' \code{get_token} retrieves a token from arcgis.com with the supplied username and password and stores them in \code{options}.
#'
#' @param token_url Optional. The url the token will be retrieved from.
#'
#' @importFrom magrittr "%>%"

get_token <-  function(token_url = 'https://www.arcgis.com/sharing/rest/generateToken') {
  ua <- httr::user_agent("http://github.com/jgrew/rArcREST")

  username <- getPass::getPass('Enter username: ')
  password <- getPass::getPass('Enter password: ')

  response <- httr::POST(
    token_url,
    query = list(
      password = password,
      username = username,
      referer = 'http://github.com/jgrew/rArcREST',
      expiration = 60,
      f = 'json'
    ),
    ua,
    httr::add_headers(referer = 'http://github.com/jgrew/rArcREST')
  )

  if (httr::status_code(response) != 200) {
    stop(
      sprintf(
        'API request failed [%s]',
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
    options(
      rArcREST.token = parsed$token,
      rArcREST.expires = parsed$expires
    )
  }

  return(invisible(TRUE))
}

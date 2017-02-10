
#' @importFrom magrittr "%>%"

get_token <-  function() {
  ua <- httr::user_agent("http://github.com/jgrew/rArcREST")

  token_url <- 'https://www.arcgis.com/sharing/rest/generateToken'

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

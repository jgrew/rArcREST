get_service_data <- function(url) {

  if (check_token() == FALSE) {
    get_token()
  }

  ua <- httr::user_agent("http://github.com/jgrew/rArcREST")

  response <- httr::POST(
    paste0(url, '/layers'),
    query = list(
      token = getOption('rArcREST.token'),
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

  return(response)
}

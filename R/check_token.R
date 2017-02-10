check_token <- function() {

  if (is.null(getOption('rArcREST.token'))) {
    return(FALSE)
  } else if (esri_datetime(getOption('rArcREST.expires')) < Sys.time()) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

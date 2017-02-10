esri_datetime <- function(x) {

  x <- x / 1000
  x <- as.POSIXct(x, origin = '1970-01-01')
  return(x)
}

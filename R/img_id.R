#' Image ID
#'
#' Return unique Twitter ID of image
#'
#' @param x Input path to image
#' @return Character vector of unique IDs
#' @export
img_id <- function(x) UseMethod("img_id")

#' @export
img_id.default <- function(x) {
  stopifnot(is.character(x))
}


#' @export
img_id.list <- function(x) {
  lapply(x, img_id)
}

#' @export
img_id.character <- function(x) {
  if (length(x) == 0) {
    return(NA_character_)
  }
  ifelse(is.na(x), NA_character_, gsub(".*/|\\.[^.]+$|\\?.*", "", x))
}



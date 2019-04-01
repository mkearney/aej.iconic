#' Image tiny
#'
#' Return tiny path to Twitter image
#'
#' @param x Input path or ID for image
#' @return Character vector of paths to tiny image version
#' @export
img_tiny <- function(x) UseMethod("img_tiny")

#' @export
img_tiny.default <- function(x) {
  stopifnot(is.character(x))
}


#' @export
img_tiny.list <- function(x) {
  lapply(x, img_tiny)
}

#' @export
img_tiny.character <- function(x) {
  if (length(x) == 0) {
    return(NA_character_)
  }
  ifelse(is.na(x), NA_character_, img_tiny_(x))
}

img_tiny_ <- function(x) {
  ext <- ifelse(grepl("\\.\\w+$", x), gsub(".*\\.", "", x), "jpg")
  paste0(sub("\\.\\w+$", "", x), "?format=", ext, "&name=tiny")
}

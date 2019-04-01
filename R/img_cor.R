
img_cor <- function(x, y) UseMethod("img_cor")

cor_ <- function(x, y) {
  if (!is.array(x)) {
    return(NA_real_)
  }
  if (length(dim(x)) == 3) {
    x <- img_gray(x)
  }
  cols <- ncol(y)
  rows <- nrow(y)
  x <- img_std_size(x, rows, cols)
  x <- as.vector(x)
  y <- as.vector(y)
  suppressWarnings(cor(x, y, use = "complete.obs"))
}


img_cor.default <- function(x, y) {
  ## prep y (template)
  if (is.list(y) && isTRUE(is.array(y[[1]]))) {
    y <- y[[1]]
  }
  if (length(dim(y)) == 3) {
    y <- img_gray(y)
  }
  future::plan(future::multiprocess)
  r <- future.apply::future_sapply(x, cor_, y = y)
  tibble::tibble(
    image = names(x),
    r = r
  )
}



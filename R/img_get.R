
img_get_ <- function(x) {
  future::plan(future::multiprocess)
  i <- future.apply::future_lapply(x, function(.x) httr::content(httr::GET(.x)))
  names(i) <- x
  i
}

is_even <- function(x) x %% 2 == 0

img_std_size <- function(x, rows = 34, cols = 64) {
  if (!is.matrix(x)) {
    return(list())
  }
  if (length(dim(x)) == 3) {
    x <- img_gray(x)
  }
  if (nrow(x) < rows) {
    zero_rows <- matrix(0, nrow = rows - nrow(x), ncol = ncol(x))
    x <- rbind(zero_rows, x)
  }
  if (ncol(x) < cols) {
    zero_cols <- matrix(0, nrow = nrow(x), ncol = cols - ncol(x))
    x <- cbind(zero_cols, x)
  }
  while (nrow(x) > rows) {
    if (is_even(nrow(x))) {
      x <- x[-1, ]
    } else {
      x <- x[-nrow(x), ]
    }
  }
  while (ncol(x) > cols) {
    if (is_even(ncol(x))) {
      x <- x[, -1]
    } else {
      x <- x[, -ncol(x)]
    }
  }
  x
}

img_gray <- function(x) {
  tryCatch(OpenImageR::rgb_2gray(x), error = function(e) list())
}


img_get <- function(x) UseMethod("img_get")

img_get.default <- function(x) {
  img_get_(x)
}

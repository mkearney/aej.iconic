
download_image <- function(x, fl) {
  if (!file.exists(fl)) {
    tryCatch(download.file(x, fl, quiet = TRUE), error = function(e) NULL)
  }
  invisible(fl)
}

download_images <- function(x) {
  ext <- sub(".*\\.", ".", x)
  id <- img_id(x)
  x <- tiny_img(id)
  fl <- file.path("images", paste0(id, ext))
  for (i in seq_along(x)) {
    download_image(x[i], fl[i])
  }
  invisible(fl)
}

read_image <- function(f, width = 33, height = 64) {
  tryCatch({
    i <- OpenImageR::readImage(f)
    i <- OpenImageR::rgb_2gray(i)
    i <- OpenImageR::resizeImage(i, width, height, method = "bilinear")
    as.vector(i)
  }, error = function(e) list())
}

read_images <- function(x, width = 33, height = 64) {
  future::plan(future::multiprocess)
  i <- future.apply::future_lapply(x, read_image, width = width, height = height)
  names(i) <- x
  i[lengths(i) > 0]
}

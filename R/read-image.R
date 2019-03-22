
img_id <- function(x) gsub(".*/|\\.[^.]+$", "", x)

tiny_img <- function(x) {
  x <- ifelse(grepl("^http", x), img_id(x), x)
  paste0("https://pbs.twimg.com/media/", x, "?format=jpg&name=tiny")
}

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

read_images <- function(x) {
  i <- future.apply::future_lapply(x, read_image)
  names(i) <- x
  i[lengths(i) > 0]
}

correlate <- function(x) {
  r <- future.apply::future_sapply(x, function(.x) cor(tmp, .x, use = "complete.obs"))
  tibble::tibble(
    image = names(x),
    r = r
  )
}



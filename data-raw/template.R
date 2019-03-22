## download to temporary file
download.file(
  "https://pbs.twimg.com/media/DxUAAbOXcAkBa-Z?format=jpg&name=tiny",
  tmp <- tempfile(fileext = ".jpg"))

## read temp file
i <- OpenImageR::readImage(tmp)

## convert to black/white image
i <- OpenImageR::rgb_2gray(i)

## save data
template_id <- "DxUAAbOXcAkBa-Z"
template_full <- "http://pbs.twimg.com/media/DxUAAbOXcAkBa-Z.jpg"
template_tiny <- "https://pbs.twimg.com/media/DxUAAbOXcAkBa-Z?format=jpg&name=tiny"
template <- i

usethis::use_data(
  template_id,
  template_full,
  template_tiny,
  template
)

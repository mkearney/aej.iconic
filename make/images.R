sh <- download_images(m)

#library(future.apply)
future::plan(future::multiprocess)


## template image
tmpf <- download_images("http://pbs.twimg.com/media/DxUAAbOXcAkBa-Z.jpg")
tmp <- read_image(tmpf)

library(dplyr)

f <- list.files("images", full.names = TRUE)
length(f)

i <- read_images(f)
r <- correlate(i)



r %>%
  dplyr::arrange(dplyr::desc(r))



## load tidyverse
library(tidyverse)

## read all data files
d <- fs::dir_ls("data") %>%
  grep("ids", ., invert = TRUE, value = TRUE) %>%
  map(readRDS) %>%
  bind_rows() %>%
  filter(!duplicated(status_id))

## view time seris
rtweet::ts_plot(d)

## vector of stopwords
sw <- unique(c(rtweet::stopwordslangs$word[rtweet::stopwordslangs$p > .999],
  tidytext::stop_words$word[tidytext::stop_words$lexicon == "SMART"]))

## tokenize words
w <- tokenizers::tokenize_tweets(
  d$text[sample(seq_len(nrow(d)), 10000)], strip_url = TRUE, stopwords = sw
)

## unlist and calc frequencies
w <- tbltools::tabsort(word = unlist(w, use.names = FALSE))

## print top words
print(w, n = 50)

recent_images <- function(n = 100) {
  .x <- list.files("images", full.names = TRUE)
  fi <- file.info(.x)
  .x <- row.names(fi)[order(fi$mtime, decreasing = TRUE)]
  head(.x, n)
}

d <- readRDS("data/all_data4.rds")
m1 <- unlist(d$ext_media_url)
#m <- unlist(d$media_url)[unlist(d$media_type) == "photo"]
m2 <- unlist(d$media_url)
m3 <- unlist(d$urls_url)
m <- unique(unlist(m1, m2, m3))
f <- future.apply::future_lapply(m[50000:51000], download_image)
f <- future.apply::future_lapply(m[70000:72000], download_image)

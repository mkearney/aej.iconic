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


sw <- tidytext::stop_words$word[tidytext::stop_words$lexicon == "SMART"]
sw <- dplyr::filter(rtweet::stopwordslangs, lang == "en", p > .999) %>%
  dplyr::pull(word) %>%
  c(sw) %>%
  unique()
w <- tokenizers::tokenize_tweets(
  gsub("https://\\S+", "", tw2$text),
  stopwords = sw)
tbltools::tabsort(word = unlist(w))
rtweet::ts_plot(tw2, "3 mins", trim = 1)

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

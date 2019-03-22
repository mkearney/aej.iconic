## get status IDs
all_ids <- readRDS("data/all_ids.rds")

## track any previously collected tweet IDs
if (file.exists("data/all_data_ids.rds")) {
  ids_collected <- c(readRDS("data/all_data_ids.rds"),
    readRDS("data/all_data_ids2.rds"),
    readRDS("data/all_data_ids3.rds"))
  all_ids <- all_ids[!all_ids %in% ids_collected]
}

## retry function to lookup tweets
lup_tweets <- function(ids) {
  d <- tryCatch(h.rtweet::lookup_all_tweets(ids),
    error = function(e) NULL)
  if (is.null(d)) {
    Sys.sleep(1)
    d <- tryCatch(h.rtweet::lookup_all_tweets(ids),
      error = function(e) NULL)
  }
  d
}

## lookup twitter data
d <- lup_tweets(all_ids)

## save data and IDs
saveRDS(d, "data/all_data4.rds")
saveRDS(d$status_id, "data/all_data_ids4.rds")

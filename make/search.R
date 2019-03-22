## load these packages
#library(tfse)
#library(rtweet)
#library(dplyr)

## read/set starting values
#all_ids <- readRDS("data/all_ids.rds")
mp <- NULL
all_ids <- NULL

## initialize output vector
s <- vector("list", 300)

## make list of search terms/phrases
search_terms <- c(
  ## words
  'covington',
  'magahat',
  'magahats',
  '#maga',
  ## grouped words
  '(elder maga)',
  '(march maga)',
  '(maga school)',
  '(maga veteran)',
  '(maga phillips)',
  '(maga philips)',
  '(maga teens)',
  '(maga boys)',
  '(maga students)',
  '(maga boy)',
  '(maga student)',
  ## phrases
  '"nathan phillips"',
  '"nick sandmann"',
  '"lincoln memorial"',
  '"Indigenous Peoples"',
  '"march for life"',
  '"maga hat"',
  '"maga hats"'
)
## wrap in parentheses just in case
search_terms <- ifelse(grepl("^\\(", search_terms),
  search_terms, paste0("(", search_terms, ")"))

## build query
search_query <- paste(
  paste(search_terms, collapse = " OR "),
  "until:2019-02-12 since:2019-01-19 filter:images"
)

## loop through searches
for (i in seq_along(s)) {
  s[[i]] <- h.rtweet::h.search_tweets(
    search_query, n = 10000, mp = mp, attempts = 3
  )
  tfse::print_complete(i)
  if (length(s[[i]]$status_id) == 0) break
  all_ids <- unique(c(all_ids, s[[i]]$status_id))
  tfse::print_complete("Now up to ", length(all_ids), " status IDs!")
  if (is.null(s[[i]]$mp)) break
  mp <- s[[i]]$mp
}

## save all_ids
all_ids <- unique(c(all_ids, unlist(purrr::map(s, "status_id"))))
alr_ids <- unique(c(readRDS("data/all_data_ids.rds"),
  readRDS("data/all_data_ids2.rds"),
  readRDS("data/all_data_ids3.rds")))
all_ids <- tfse::nin(all_ids, alr_ids)
saveRDS(all_ids, "data/all_ids.rds")

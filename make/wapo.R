"https://twitter.com/drpatfarrell/status/1086774983806853120/photo/1"

## IDs from WaPo story: https://www.washingtonpost.com/technology/2019/01/23/
## how-anonymous-tweets-helped-ignite-national-controversy-over-maga-hat-teens
wapo_sids <- c(
  "1087401351246761984",
  "1087538251005595651",
  "1086637125154557952",
  "1087783681765322756",
  "1087783684005081092",
  "1074818910640373760"
)

## get data on wapo tweets
wapo <- lookup_tweets(wapo_sids)

## hack version of mp
hmp <- h.rtweet::h.search_tweets(
  paste0('"', sub("\\s+\\S+$", "", substr(wapo$text[1], 1, 40)), '"')
)

hmp2 <- h.rtweet::h.search_tweets(
  "nathan OR phillips OR covington OR catholic OR maga OR native OR march",
  mp = hmp$mp,
  n = 100,
  attempts = 2
)

td <- h.rtweet::lookup_all_tweets(hmp2$status_id)
range(td$created_at)
wapo$created_at[1]
hmp

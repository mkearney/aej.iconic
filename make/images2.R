
r <- dplyr::bind_rows(imgs)
r$id <- img_id(r$image)
r <- rbind(readRDS("make/data/cor-ids.rds"), r)
saveRDS(r, "make/data/cor-ids.rds")

r <- readRDS("make/data/cor-ids.rds")

r %>%
  dplyr::arrange(desc(r)) %>%
  dplyr::filter(r >= .4255) %>%
  dplyr::pull(id) -> maga_hat_ids

sum(d$is_img)
maga_hat_ids <- img_id(names(vr))
d$is_img <- purrr::map_lgl(d$media_id, ~ any(.x %in% maga_hat_ids))
d$is_img <- ifelse(is.na(d$is_img),
  purrr::map_lgl(d$ext_media_id, ~ any(.x %in% maga_hat_ids)), d$is_img)

table(d$is_img)


library(dplyr)

get_status_replies <- function(.d) {
  id <- .d$status_id
  sn <- .d$screen_name
  sh <- capture.output(s <- h.rtweet::h.search_tweets(
    glue::glue("to:{sn} since_id:{id} filter:replies until:2019-02-15"),
    attempts = 1, n = 200))
  if (length(s$status_id) == 0) return(tibble::tibble())
  tw <- tryCatch(rtweet::lookup_tweets(s$status_id),
    error = function(e) return(tibble::tibble()))
  if (nrow(tw) == 0) return(tw)
  #tw <- dplyr::filter(tw, reply_to_status_id == id)
  if (nrow(tw) == 0) {
    tw <- tibble::tibble()
  }
  tw
}



d %>%
  dplyr::filter(is_img) -> ttt

o <- vector("list", nrow(ttt))

for (i in seq_along(o)) {
  o[[i]] <- get_status_replies(ttt[i, ])
  tfse::print_complete(i)
}

oo <- dplyr::bind_rows(o)
replies <- tfse::read_RDS("make/data/replies.rds")

replies <- dplyr::bind_rows(d, oo) %>%
  dplyr::filter(!duplicated(status_id)) %>%
  dplyr::filter(!is.na(reply_to_status_id)) %>%
  dplyr::filter(reply_to_status_id %in% d$status_id)

replies <- dplyr::bind_rows(d, oo) %>%
  dplyr::filter(!duplicated(status_id)) %>%
  dplyr::filter(!is.na(reply_to_status_id)) %>%
  dplyr::filter(reply_to_status_id %in% c(replies$status_id, d$status_id))



replies$is_img <- purrr::map_lgl(replies$media_id, ~ any(.x %in% maga_hat_ids))
replies$is_img <- ifelse(is.na(replies$is_img),
  purrr::map_lgl(replies$ext_media_id, ~ any(.x %in% maga_hat_ids)), replies$is_img)

replies <- tfse::read_RDS("make/data/replies.rds")

all_img_ids <- unique(c(d$status_id[d$is_img], replies$status_id[replies$is_img]))

m <- rbind(replies, d) %>%
  dplyr::mutate(keep = is_img | reply_to_status_id %in% all_img_ids,
    reply1 = reply_to_status_id %in% status_id[is_img],
    reply2 = reply_to_status_id %in% reply1,
    reply3 = reply_to_status_id %in% reply2,
    reply4 = reply_to_status_id %in% reply3,
    reply5 = reply_to_status_id %in% reply4) %>%
  dplyr::filter(keep | is_img | reply_to_status_id %in% status_id |
      reply1 | reply2 | reply3 | reply4 | reply5)
m <- rbind(replies, d) %>%
  dplyr::mutate(keep = is_img | reply_to_status_id %in% all_img_ids,
    reply1 = reply_to_status_id %in% status_id[is_img],
    reply2 = reply_to_status_id %in% reply1,
    reply3 = reply_to_status_id %in% reply2,
    reply4 = reply_to_status_id %in% reply3,
    reply5 = reply_to_status_id %in% reply4) %>%
  dplyr::filter(keep | is_img | reply_to_status_id %in% status_id |
      status_id %in% c(m$status_id, all_img_ids) |
      reply_to_status_id %in% m$status_id)
m

m$row_sum <- m %>%
  mutate(replied_to = status_id %in% reply_to_status_id) %>%
  select(replied_to, is_img, reply1:reply5) %>%
  mutate(sum = rowSums(.)) %>%
  pull(sum)

from_to_ <- function(x) {
  ats <- tfse::na_omit(unlist(
    c(x$mentions_user_id, x$reply_to_user_id, x$retweet_user_id, x$quoted_user_id)))
  if (length(ats) == 0) return(tibble::tibble())
  tibble::tibble(from = x$user_id[1], to = ats,
    status_id = x$status_id[1], is_img = x$is_img[1])
}
from_to <- function(x) {
  split(x, x$status_id) %>%
    purrr::map(from_to_) %>%
    dplyr::bind_rows()
}
adj_mat <- function(x) {
  users <- unique(c(x$from, x$to))
  m <- matrix(0L, nrow = length(users), length(users))
  l <- x %>%
    dplyr::mutate(from = as.integer(factor(from, levels = users)),
      to = as.integer(factor(to, levels = users)))
  for (i in seq_along(users)) {
    m[i, l$to[l$from == i]] <- 1L
  }
  colnames(m) <- users
  row.names(m) <- users
  m
}


m %>%
  mutate(with_img = reply_to_user_id %in% user_id[is_img] |
      purrr::map_lgl(mentions_user_id, ~ any(.x %in% user_id[is_img], na.rm = TRUE)) |
      purrr::map_lgl(quoted_user_id, ~ any(.x %in% user_id[is_img], na.rm = TRUE))) %>%
  dplyr::filter(is_img | with_img) %>%
  from_to() %>%
  unique() -> ft

ft <- ft %>%
  mutate(count = purrr::map_int(status_id, ~
      sum(status_id == .x, na.rm = TRUE)))

ft %>% mutate(keep = is_img) %>%
  dplyr::filter(keep | (!is_img & status_id %in% status_id[is_img]))  -> ftf#%>%
#dplyr::filter(!to %in% from[keep], !(duplicated(status_id) & !is_img))

#ftf <- left_join(ftf, select(m, status_id, created_at))
adj_mat(ftf) -> am

nrow(am)
library(dplyr)
n <- network::network(am, directed = TRUE)

library(network)

#all_users <- unique(unlist(c(m$user_id, m$reply_to_user_id, m$mentions_user_id,
#  m$retweet_user_id, m$quoted_user_id)))
#mm <- rtweet::lookup_users(all_users)
#mm$is_img <- mm$user_id %in% m$user_id[m$is_img]

matchr <- function(a, b, v, na = NA) {
  m <- match(a, b)
  ifelse(is.na(m), na, v[m])
}

unlq <- function(...) {
  tfse::na_omit(unlist(c(...)))
}

row_ids <- dapr::lap(seq_len(nrow(m)), ~ {
  tibble(row = .x, from = m$user_id[.x], to = unlq(m$reply_to_user_id[.x],
    m$mentions_user_id[.x], m$retweet_user_id[.x], m$quoted_user_id[.x]))
}) %>%
  dplyr::bind_rows()



ca <- matchr(n %v% "vertex.names", row_ids$from, row_ids$row, NA)
ca <- ifelse(is.na(ca),
  matchr(n %v% "vertex.names", row_ids$to, row_ids$row, NA),
  ca)
ca <- m$created_at[ca]
ca <- rtweet::round_time(ca, "hours")

n %v% "created_at" <- rank(as.numeric(ca))
n %v% "is_img" <- matchr(n %v% "vertex.names", m$user_id, m$is_img, FALSE)
n %v% "verified" <- matchr(n %v% "vertex.names", m$user_id, m$verified, FALSE)
n %v% "followers_count" <- matchr(n %v% "vertex.names", m$user_id, m$followers_count, 0)
library(ggplot2)

p <- ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  ggnetwork::geom_edges(color = "#666666", alpha = .5, curvature = .2, size = .6) +
  ggnetwork::geom_nodes(aes(fill = is_img, size = sqrt(followers_count + 1),
    shape = verified),
    alpha = .8, color = "#333333aa") +
  ggnetwork::theme_blank() +
  scale_fill_manual(values = c("#3366ff66", "#dd0000")) +
  scale_size_continuous(range = c(2.5, 7), guide = FALSE) +
  scale_shape_manual(values = c(21, 22))
#  gganimate::transition_time(created_at) +
#  gganimate::enter_fade() +
#  gganimate::exit_fade()

p

#gganimate::animate(p, nframes = n_uq(ca), fps = 3)



ca <- matchr(n %v% "vertex.names", row_ids$from, row_ids$row, NA)
ca <- ifelse(is.na(ca),
  matchr(n %v% "vertex.names", row_ids$to, row_ids$row, NA),
  ca)
ca <- m$created_at[ca]
ca2 <- rtweet::round_time(ca, "hours")

tibble::tibble(user_id = n %v% "vertex.names",
  created_at = as.integer(factor(n %v% "created_at")),
  is_img = n %v% "is_img",
  verified = n %v% "is_img") -> y
y$created_at <- ca2

y %>%
  mutate(dup = !duplicated(user_id)) %>%
  group_by(created_at, is_img) %>%
  summarise(n = sum(dup)) %>%
  group_by(is_img) %>%
  mutate(n = cumsum(n)) %>%
  ungroup() %>%
  mutate(is_img = factor(is_img, labels = c("  Interacting users      ",
    "  Posting users"))) %>%
  ggplot(aes(x = created_at, y = n, color = is_img)) +
  geom_smooth(method = "loess", se = TRUE, span = .125) +
  #geom_line() +
  #geom_point() +
  dataviz::theme_mwk(base_size = 16) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "Cumulative number of unique users",
    title = "Number of users posting and interacting with MAGA hat photo",
    subtitle = "Cumulative number of unique users posting or interacting (i.e., reply, quote) with posters of photo") +
  ggsave("make/posting-interacting.png", width = 9, height = 6, units = "in")

tibble::tibble(user_id = colnames(as.matrix(n)),
  from = colSums(as.matrix(n)),
  to = rowSums(as.matrix(n))) %>%
  left_join(m) %>%
  group_by(is_img, verified) %>%
  summarise(from = mean(from), to = mean(to), n = n())


#animate(p, renderer = ffmpeg_renderer())

#tfse::save_RDS(replies, "make/data/replies.rds")


d



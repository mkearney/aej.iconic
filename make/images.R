#library(future.apply)
#future::plan(future::multiprocess)

## load tfse
library(tfse)

## read all data files
d <- fs::dir_ls("make/data") %>%
  grep("ids", ., invert = TRUE, value = TRUE) %>%
  purrr::map(readRDS) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(!duplicated(status_id)) %>%
  dplyr::arrange(created_at)# %>%
  #dplyr::select(status_id, created_at, media_url, ext_media_url)

select_and_pull <- function(.data, ...) {
  .data <- as.list(dplyr::select(.data, ...))
  unique(tfse::na_omit(unlist(.data, use.names = FALSE)))
}

d$media_id <- img_id(d$media_url)
d$ext_media_id <- img_id(d$ext_media_url)

select_and_pull(d, media_url, ext_media_url) %>%
  img_tiny() -> images

r <- readRDS("make/data/cor-ids.rds")

images <- r$image[r$image %in% r$image[r$r > .45 & !is.na(r$r)]]


#st <- seq(1, length(images), 2000)
#en <- seq(2000, length(images), 2000)
st <- seq(1, length(images), 20)
en <- seq(20, length(images), 20)

imgs <- vector("list", length(en))
tmp <- img_get(template_tiny)

std_img_size <- function(img, size) {
  std_img_size_ <- function(img, size) {
    if (length(img) < size) {
      if ((size - length(img)) > length(img)) {
        repl <- TRUE
      } else {
        repl <- FALSE
      }
      img <- img[sort(c(seq_along(img),
        sample(length(img), size - length(img), replace = repl)
      ))]
    }
    if (length(img) > size) {
      img <- img[sort(sample(length(img), size))]
    }
    img
  }
  lapply(img, std_img_size_, size)
}

good_item <- function(x) {
  x2 <- x[order(x, decreasing = TRUE)[-c(1:2)]]
  max(x) >= .40 && all(x2 < .35)
}

good_items <- function(x) {
  sum(x >= .40) > 1
}

factor_analyze <- function(x) {
  nms <- names(x)
  x <- matrix(unlist(x), ncol = length(x))
  suppressWarnings(
    suppressMessages(
      r <- cor(x, use = "pairwise.complete.obs")
    )
  )
  colnames(x) <- nms
  nfactors <- sum(eigen(r, only.values = TRUE)[[1]] >= .99)
  f1 <- psych::fa(x, rotate = "varimax", nfactors = nfactors, fm = "pa",
    SMC = FALSE, max.iter = 100)
  inds <- apply(f1$loadings[, ], 2, good_items)
  inds <- names(inds)[inds]
  kpr <- apply(f1$loadings[, inds], 1, good_item)
  kpr <- names(kpr)[kpr]
  nfactors <- length(inds)
  f1 <- suppressWarnings(
    psych::fa(x[, kpr], rotate = "varimax", nfactors = nfactors, fm = "pa",
    SMC = FALSE, max.iter = 100))
  psych::fa.diagram(f1, cut = .45)
  l <- f1$loadings[]
  f <- function(l, i) which(l[, i] > .45)[order(l[l[, i] > .45, i], decreasing = TRUE)]
  o <- vector("list", ncol(l))
  for (i in seq_len(ncol(l))) {
    o[[i]] <- f(l, i)
  }
  l <- l[unlist(o), ]
  rns <- row.names(l)
  l <- as.data.frame(l, row.names = NULL)
  l$item <- rns
  x <- tibble::as_tibble(l[, c(ncol(l), 1:(ncol(l) - 1))])
  dplyr::left_join(x, dplyr::filter(tidyr::gather(x, factor, est, -item),
    est > .45)[, 1:2])
}


tmp <- as.vector(tmp[[1]])
names(tmp) <- "http://pbs.twimg.com/media/DxUAAbOXcAkBa-Z.jpg"

for (i in seq_along(imgs)) {
  imgs[[i]] <- img_get(images[st[i]:en[i]])
  #imgs[[i]] <- img_cor(imgs[[i]], tmp)
  #sh <- gc()
  tfse::print_complete(i, "/", length(imgs))
}
str(imgs, 1)
v <- unlist(imgs, recursive = FALSE)
images_ <- images[lengths(v) > 0]
v <- v[lengths(v) > 0]
vr <- std_img_size(v, length(tmp[[1]]))

x <- factor_analyze(vr)



print(x, n = 100)

lap()

grep("DxUAAbOXcAkBa", x$item)
lapply(x$item[x$factor == "PA10"][1:3], browseURL)
lapply(x$item[!duplicated(x$factor)], browseURL)
library(dplyr)

x %>%
  tidyr::gather(dim, est, -factor, -item) %>%
  select(-dim) %>%
  arrange(factor, desc(est)) %>%
  group_by(factor) %>%
  mutate(p = est / max(est)) %>%
  filter(p > .6) %>%
  top_n(5, est) %>%
  ungroup() %>%
  pull(item) -> training_images

saveRDS(training_images, "make/data/training_images.rds")


training_ids <- img_id(training_images)




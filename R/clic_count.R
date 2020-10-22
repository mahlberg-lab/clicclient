clic_count <- function(corpora, subset = c("all", "shortsus", "longsus", "nonquote", "quote")) {
  ql <- setNames(as.list(corpora), rep("corpora", length(corpora)))
  ss <- setNames(as.list(subset), rep("subset", length(subset)))
  
  qq <- c(ql, ss)
  
  r <- clic_request(endpoint = "count", query = ql)
  
  data.frame(
    book                 = r$data[, 1],
    total_words          = r$data[, 2],
    in_short_suspensions = r$data[, 3],
    in_long_suspensions  = r$data[, 4],
    in_non_quotes        = r$data[, 5],
    in_quotes            = r$data[, 6]
  )
}

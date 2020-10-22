subset_df <- function(x) {
  
  left <- paste(unlist(head(x[[1]], -1)), collapse = "")
  left_words <- unlist(tail(x[[1]], 1))
  
  node <- paste(unlist(head(x[[2]], -1)), collapse = "")
  node_words <- unlist(tail(x[[2]], 1))
  
  right <- paste(unlist(head(x[[3]], -1)), collapse = "")
  right_word <- unlist(tail(x[[3]], 1))
  
  result_metadata <- x[[4]]
  position_metadata <- x[[5]]
  
  book_title <- result_metadata[[1]]
  
  data.frame(
    left             = left,
    node             = node,
    right            = right,
    book             = book_title,
    stringsAsFactors = FALSE
  )
}

clic_subset <- function(corpora, subset, contextsize = 3) {
  
  ql <- setNames(as.list(corpora), rep("corpora", length(corpora)))
  
  ql$subset <- subset
  ql$contextsize = contextsize
  
  r <- clic_request(endpoint = "subset", query = ql)
  
  df <- setDF(rbindlist(lapply(r$data, subset_df)))
  df
}

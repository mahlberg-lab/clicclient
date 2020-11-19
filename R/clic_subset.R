subset_df <- function(x, metadata = FALSE) {
  
  left <- paste(unlist(head(x[[1]], -1)), collapse = "")
  left_words <- unlist(tail(x[[1]], 1))
  
  node <- paste(unlist(head(x[[2]], -1)), collapse = "")
  node_words <- unlist(tail(x[[2]], 1))
  
  right <- paste(unlist(head(x[[3]], -1)), collapse = "")
  right_word <- unlist(tail(x[[3]], 1))
  
  result_metadata <- x[[4]]
  position_metadata <- x[[5]]
  
  book_title <- result_metadata[[1]]
  
  X <- data.frame(
    left             = left,
    node             = node,
    right            = right,
    book             = book_title,
    stringsAsFactors = FALSE
  )
  
  if(metadata) {
    X$chapter <- position_metadata[[1]]
    X$paragraph <- position_metadata[[2]]
    X$sentence <- position_metadata[[3]]
    X$begin <- result_metadata[[2]]
    X$end <- result_metadata[[3]]
  }
  
  return(X)
}

clic_subset <- function(corpora, subset, contextsize = 3, metadata = FALSE, json = FALSE) {
  
  ql <- setNames(as.list(corpora), rep("corpora", length(corpora)))
  
  ql$subset <- subset
  ql$contextsize = contextsize
  
  r <- clic_request(endpoint = "subset", query = ql, json = json)

  if(json) {
    return(r)
  } else {
    df <- setDF(rbindlist(lapply(r$data, subset_df, metadata)))
    return(df)
  }
  
}

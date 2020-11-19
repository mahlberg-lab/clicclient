concordance_df <- function(x, metadata = FALSE) {
  left_context <- x[[1]]
  
  left_context_string <- paste(left_context[1:length(left_context) - 1], collapse = "")
  left_context_words <- unlist(left_context[length(left_context)])
  
  node <- x[[2]]
  
  node_string <- paste(node[1:length(node) - 1], collapse = "")
  node_words <- unlist(node[length(node)])
  
  right_context <- x[[3]]
  
  right_context_string <- paste(right_context[1:length(right_context) - 1], collapse = "")
  right_context_words <- unlist(right_context[length(right_context)])
  
  result_metadata <- unlist(x[[4]])
  position_in_book_metadata <- unlist(x[[5]])
  
  book_title <- result_metadata[[1]]
  
  X <- data.frame(
    left             = left_context_string,
    node             = node_string,
    right            = right_context_string,
    book             = book_title,
    stringsAsFactors = FALSE
  )
  
  if(metadata) {
    X$chapter <- position_in_book_metadata[[1]]
    X$paragraph <- position_in_book_metadata[[2]]
    X$sentence <- position_in_book_metadata[[3]]
    X$begin <- result_metadata[[2]]
    X$end <- result_metadata[[3]]
  }
  
  return(X)
  
}

clic_concordance <- function(corpora, q, subset = "all", contextsize = 3, metadata = FALSE, json = FALSE) {
  
  ql <- setNames(as.list(corpora), rep("corpora", length(corpora)))
  qq <- setNames(as.list(q), rep("q", length(q)))
  
  ql <- c(ql, qq)
  
  ql$contextsize = contextsize
  ql$subset = subset
  
  r <- clic_request(endpoint = "concordance", query = ql, json = json)
  
  if(json) {
    return(r)
  } else {
    df <- setDF(rbindlist(lapply(r$data, concordance_df, metadata)))
    return(df)
  }
}

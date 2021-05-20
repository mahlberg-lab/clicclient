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

subset_query_list <- function(params) {
  ql <- setNames(as.list(params$corpora), rep("corpora", length(params$corpora)))
  ql$subset <- params$subset
  ql$contextsize <- params$contextsize
  return(ql)
}

clic_subset <- function(corpora, subset, contextsize = 3, metadata = FALSE, json = FALSE) {
  
  clic_response <- clic_request(
    endpoint = "subset",
       query = subset_query_list(list(corpora = corpora, subset = subset, contextsize = contextsize)),
        json = json
  )

  if(json) {
    result <- clic_response
  } else {
    result <- setDF(rbindlist(lapply(clic_response$data, subset_df, metadata)))
  }
  
  return(result)
  
}

subset_df <- function(response_data, metadata = FALSE) {
  
  result <- data.frame(
    left             = paste(unlist(head(response_data[[1]], -1)), collapse = ""),
    node             = paste(unlist(head(response_data[[2]], -1)), collapse = ""),
    right            = paste(unlist(head(response_data[[3]], -1)), collapse = ""),
    book             = response_data[[4]][[1]],
    stringsAsFactors = FALSE
  )
  
  if(metadata) {
      result$chapter <- response_data[[5]][[1]]
    result$paragraph <- response_data[[5]][[2]]
     result$sentence <- response_data[[5]][[3]]
        result$begin <- response_data[[4]][[2]]
          result$end <- response_data[[4]][[3]]
  }
  
  return(result)
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

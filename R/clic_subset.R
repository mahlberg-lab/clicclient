#' One row of the subsets data frame.
#'
#' Builds a data frame corresponding to a single
#' response from the subset endpoint.
#' 
#' @param response_data list of subset data from clic subset endpoint
#' @param metadata TRUE/FALSE
#'
#' @return a data frame with character variables left, node, right and book
#' if metadata is TRUE then the output also has
#' numeric variables chapter, paragraph, sentence, begin and end.
#' 
#' @keywords internal
subset_df <- function(response_data, metadata = FALSE) {
  
  result <- data.frame(
    left             = paste(unlist(head(response_data[[1]], -1)), collapse = ""),
    node             = paste(unlist(head(response_data[[2]], -1)), collapse = ""),
    right            = paste(unlist(head(response_data[[3]], -1)), collapse = ""),
    book             = response_data[[4]][[1]],
    stringsAsFactors = FALSE
  )
  
  if(metadata) {
      result$chapter <- as.numeric(response_data[[5]][[1]])
    result$paragraph <- as.numeric(response_data[[5]][[2]])
     result$sentence <- as.numeric(response_data[[5]][[3]])
        result$begin <- as.numeric(response_data[[4]][[2]])
          result$end <- as.numeric(response_data[[4]][[3]])
  }
  
  return(result)
}

#' A query list for subset queries.
#'
#' Builds a query list to pass to clic_request when
#' calling the subset endpoint.
#'
#' @param params list of parameters
#'
#' @return query list for subset endpoint
#' 
#' @keywords internal
subset_query_list <- function(params) {
  ql <- setNames(as.list(params$corpora), rep("corpora", length(params$corpora)))
  ql$subset <- params$subset
  ql$contextsize <- params$contextsize
  return(ql)
}

#' Fetch subsets.
#' 
#' Subsets of given texts, for example quotations.
#'
#' @param corpora 1+ corpus name (e.g. 'dickens') or book name ('AgnesG') to search within
#' @param subset subset to search through, one of shortsus/longsus/nonquote/quote/all. Default 'all' (i.e. all text)
#' @param contextsize size of context window around search results. Default 3.
#' @param metadata  Return metadata. TRUE/FALSE
#' @param json JSON format. TRUE/FALSE
#'
#' @return A \code{data.frame} with one entry per result.
#' @export
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

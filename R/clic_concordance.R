#' Build one row of a concordance data frame
#'
#' Creates a data frame corresponding to a single response from the
#' concordance endpoint.
#'
#' @param x list of concordance data from CLiC concordance endpoint
#' @param metadata Get metadata. TRUE/FALSE
#'
#' @return a data frame
#' 
#' @keywords internal
concordance_df <- function(response_data, metadata = FALSE) {
 
     left_context <- response_data[[1]]
             node <- response_data[[2]]
    right_context <- response_data[[3]]
  result_metadata <- unlist(response_data[[4]])
  
  result <- data.frame(
                left = paste(left_context[1:length(left_context) - 1], collapse = ""),
                node = paste(node[1:length(node) - 1], collapse = ""),
               right = paste(right_context[1:length(right_context) - 1], collapse = ""),
                book = result_metadata[[1]],
    stringsAsFactors = FALSE
  )
  
  if(metadata) {
    position_in_book_metadata <- unlist(response_data[[5]])
               result$chapter <- position_in_book_metadata[[1]]
             result$paragraph <- position_in_book_metadata[[2]]
              result$sentence <- position_in_book_metadata[[3]]
                 result$begin <- result_metadata[[2]]
                   result$end <- result_metadata[[3]]
  }
  
  return(result)
  
}

#' A query list for concordance queries.
#'
#' Builds a query list to pass to clic_request when
#' calling the concordance endpoint.
#'
#' @param params list of parameters
#'
#' @return query list for subset endpoint
#' 
#' @keywords internal
concordance_query_list <- function(params) {
  ql <- setNames(as.list(params$corpora), rep("corpora", length(params$corpora)))
  qq <- setNames(as.list(params$q), rep("q", length(params$q)))
  
  ql <- c(ql, qq)
  
  ql$contextsize = params$contextsize
  ql$subset = params$subset
  
  return(ql)
}

#' Fetch concordance
#'
#' @param corpora 1+ corpus name (e.g. 'dickens') or book name ('AgnesG')
#'   to search within
#' @param q 1+ string to search for. If multiple terms are provided, we will
#'   search for each in turn
#' @param subset  A string containing Any one of \dQuote{shortsus},
#'   \dQuote{longsus}, \dQuote{nonquote} and \dQuote{quote}.
#' @param contextsize Size of context window around search results. Default 3.
#' @param metadata Return metadata. TRUE/FALSE.
#' @param json JSON format. TRUE/FALSE.
#'
#' @return a \code{data.frame}, one entry per result. Each item is an array with the following items:
#'   \itemize{
#'     \item The left context window (if ``contextsize`` > 0, otherwise omitted)
#'     \item The node (i.e. the text searched for)
#'     \item The right context window (if ``contextsize`` > 0, otherwise omitted)
#'     \item Result metadata
#'     \item Position-in-book metadata
#'   }
#' @export
#' @seealso \url{https://github.com/birmingham-ccr/clic/blob/2.1/server/clic/concordance.py}
clic_concordance <- function(corpora, q, subset = "all", contextsize = 3, metadata = FALSE, json = FALSE) {
  
  clic_response <- clic_request(
    endpoint = "concordance",
    query = concordance_query_list(list(corpora = corpora, q = q, subset = subset, contextsize = contextsize)),
    json = json
  )
  
  if(json) {
    result <- clic_response
  } else {
    result <- setDF(rbindlist(lapply(clic_response$data, concordance_df, metadata)))
  }
  
  return(result)
}

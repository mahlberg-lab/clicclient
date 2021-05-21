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

#' Fetch concordance
#'
#' @param corpora 1+ corpus name (e.g. 'dickens') or book name ('AgnesG')
#' to search within
#' @param q 1+ string to search for. If multiple terms are provided, we will
#' search for each in turn
#' @param subset  A string containing Any one of \dQuote{shortsus},
#' \dQuote{longsus}, \dQuote{nonquote} and \dQuote{quote}.
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

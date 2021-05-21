#' Fetch tokens
#' 
#' Fetches tokens using the CLiC API \sQuote{subset} endpoint.
#'
#' @param shortname Can be any value from the \sQuote{corpus} or
#'   \sQuote{shortname} columns returned by \code{\link{clic_lookup}}.
#'   Can be given as a single string, or a list of strings in which case the
#'   results will be concatenated.
#' @param subset A string containing Any one of \dQuote{shortsus},
#'   \dQuote{longsus}, \dQuote{nonquote} and \dQuote{quote}.
#' @param lowercase Boolean indicating if the tokens should be transformed to
#'   lower case.
#' @param punctuation Boolean indicating if punctuation tokens should be
#'   included. Note that the puctuation tokens may include whitespace.
#'
#' @return A vector of tokens
#' @export
#'
#' @seealso \url{https://github.com/birmingham-ccr/clic/server/clic/subset.py}
clic_tokens <- function(
    shortname,
    subset = NULL,
    lowercase = TRUE,
    punctuation = FALSE  # includes whitespace
) {
    query <- paste(sprintf("corpora=%s", shortname), collapse = "&")
    if(! is.null(subset)) {
        subset <- match.arg(subset, c("shortsus", "longsus", "nonquote", "quote"))
        query <- sprintf("%s&subset=%s", query, subset)
    }
    rv <- clic_request(endpoint = "subset", query = query)
    if(punctuation) {
        tokens <- unlist( sapply(rv$data, function(x) {
            head(x[[1]], -1)
        }) )
    } else {
        tokens <- unlist( sapply(rv$data, function(x) {
            head(x[[1]], -1)[as.integer(tail(x[[1]], 1)[[1]])+1]
        }) )
    }
    if(lowercase) {
        tokens <- tolower(tokens)
    }
    return(tokens)
}


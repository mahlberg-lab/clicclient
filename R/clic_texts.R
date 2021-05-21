#' Fetch texts
#' 
#' Fetches texts using the CLiC API.
#'
#' @param shortname Can be any value from the \sQuote{corpus} or
#'   \sQuote{shortname} columns returned by \code{\link{clic_lookup}}.
#'   Can be given as a single string, or a list of strings in which case the
#'   results will be concatenated.
#' @param subset A string containing Any one of \dQuote{shortsus},
#'   \dQuote{longsus}, \dQuote{nonquote} and \dQuote{quote}.
#'
#' @return Returns a single string of text.
#' @export
clic_texts <- function(
    shortname,
    subset = NULL
) {
    query <- paste(sprintf("corpora=%s", shortname), collapse = "&")
    if(! is.null(subset)) {
        subset <- match.arg(subset, c("shortsus", "longsus", "nonquote", "quote"))
        query <- sprintf("%s&subset=%s", query, subset)
    }
    rv <- clic_request(endpoint = "subset", query = query)
    tokens <- unlist( sapply(rv$data, function(x) {
        head(x[[1]], -1)
    }) )
    # attempt to regain some white space integrity
    tokens <- ifelse(tokens %in% c("."),
        paste0(tokens, " "),
        tokens
    )
    return(paste(tokens, collapse = ""))
}


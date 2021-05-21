#' Fetch clusters
#' 
#' Fetches n-grams using the CLiC API \sQuote{cluster} endpoint.
#'
#' @param shortname Can be any value from the \sQuote{corpus} or
#'   \sQuote{shortname} columns returned by \code{\link{clic_lookup}}.
#'   Can be given as a single string, or a list of strings in which case the
#'   results will be concatenated.
#' @param length Cluster length to search for, one of 1/3/4/5
#'   (NB: There is no 2).
#' @param cutoff The cutoff frequency, if a cluster occurs less times than
#'   this then it is not returned.
#' @param subset A string containing Any one of \dQuote{shortsus},
#'   \dQuote{longsus}, \dQuote{nonquote} and \dQuote{quote}.
#' @param json JSON format. TRUE/FALSE.
#'
#' @return A \code{data.frame} of clusters to counts.
#' @export
clic_clusters <- function(
    shortname,
    length,
    cutoff = 5,
    subset = NULL,
    json = FALSE
) {
    if(! length %in% c(1, 3, 4, 5)) {
        stop(paste0("bad length parameter: '", length, "'"))
    }
    query <- paste(sprintf("corpora=%s", shortname), collapse = "&")
    query <- sprintf("%s&clusterlength=%d&cutoff=%s", query, length, cutoff)
    if(! is.null(subset)) {
        subset <- match.arg(subset, c("shortsus", "longsus", "nonquote", "quote"))
        query <- sprintf("%s&subset=%s", query, subset)
    }
    rv <- clic_request(endpoint = "cluster", query = query, json = json)

    if(json) {
        return(rv)
    } else {
        clusters <- data.frame("cluster" = rv$data[ , 1], "count" = as.integer(rv$data[ , 2]), stringsAsFactors = FALSE)
        clusters <- clusters[order(clusters$count, decreasing = TRUE),]
        rownames(clusters) <- NULL
        return(clusters)
    }

}

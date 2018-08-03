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


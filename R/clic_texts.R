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


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

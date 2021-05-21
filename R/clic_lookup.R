#' Fetch listing of CLiC content
#' 
#' Fetches a listing of the texts for each of the available corpora in CLiC.
#'
#' @return Returns a \code{data.frame} listing the texts for each of the available corpora.
#' @export
clic_lookup <- function() {
    rv <- clic_request(endpoint = "corpora")
    # hack to stop R CMD check warnings - ref: data.table
    corpus = NULL
    DT <- rbindlist(
        rv$corpora$children, fill = TRUE,
        idcol = 'corpus'
    )[ , corpus := rv$corpora$id[corpus]]
    setkeyv(DT, cols = c("corpus", "author", "title"))
    return(DT[])
}

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

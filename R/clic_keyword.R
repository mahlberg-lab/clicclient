clic_keyword <- function(corpora, refcorpora, subset = c("all"), refsubset = c("all"), clusterlength = 3, pvalue = .0001) {
  
  ql <- setNames(as.list(corpora), rep("corpora", length(corpora)))
  qr <- setNames(as.list(refcorpora), rep("refcorpora", length(refcorpora)))
  
  ss <- setNames(as.list(subset), rep("subset", length(subset)))
  sr <- setNames(as.list(refsubset), rep("refsubset", length(refsubset)))
  
  qq <- c(ql, qr, ss, sr)
  
  qq$clusterlength <- clusterlength
  qq$pvalue <- pvalue
  
  r <- clic_request(endpoint = "keyword", query = qq)
  
  data.frame(
    ngram               = r$data[, 2],
    target_frequency    = r$data[, 3],
    reference_frequency = r$data[, 5],
    LL                  = r$data[, 9],
    p                   = r$data[, 11]
  )
  
}

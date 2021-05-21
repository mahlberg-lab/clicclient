#' Fetch keywords
#' 
#' Compute keywords (words that are used significantly more frequently
#' in one corpus than they are in a reference corpus). The statistical measure
#' used is log-likelihood as explained by Rayson and Garside:
#'   Rayson, P. and Garside, R. (2000). Comparing corpora using frequency profiling.
#' In proceedings of the workshop on Comparing Corpora, held in conjunction
#' with the 38th annual meeting of the Association for Computational Linguistics
#' (ACL 2000). 1-8 October 2000, Hong Kong, pp. 1 - 6
#' Available at: http://ucrel.lancs.ac.uk/people/paul/publications/rg_acl2000.pdf
#'
#' @param corpora 1+ corpus name (e.g. 'dickens') or book name ('AgnesG') to search within
#' @param refcorpora 1+ corpus name (e.g. 'dickens') or book name ('AgnesG') to search within
#' @param subset subset to search through, one of
#'   shortsus/longsus/nonquote/quote/all. Default 'all' (i.e. all text)
#' @param refsubset subset to search through, one of
#'   shortsus/longsus/nonquote/quote/all. Default 'all' (i.e. all text)
#' @param clusterlength cluster length to search for, 1 or more. Default 1
#' @param pvalue  A p_value of 0.0001 will select keywords that have 0.0001
#'   *or less* as their p_value. It is a cut-off. One can choose one out of four
#'   values: 0.0001, 0.001, 0.01, or 0.05. If any other value is chosen, it is
#'   ignored and no filtering on p_value is done. 
#' @param json JSON format. TRUE/FALSE.
#'
#' @return a \code{data.frame}, one entry per result.
#' @export
#' 
#' @seealso \url{https://github.com/birmingham-ccr/clic/blob/2.1/server/clic/keyword.py}
clic_keyword <- function(corpora, refcorpora, subset = c("all"), refsubset = c("all"), clusterlength = 3, pvalue = .0001, json = FALSE) {
  
  ql <- setNames(as.list(corpora), rep("corpora", length(corpora)))
  qr <- setNames(as.list(refcorpora), rep("refcorpora", length(refcorpora)))
  
  ss <- setNames(as.list(subset), rep("subset", length(subset)))
  sr <- setNames(as.list(refsubset), rep("refsubset", length(refsubset)))
  
  qq <- c(ql, qr, ss, sr)
  
  qq$clusterlength <- clusterlength
  qq$pvalue <- pvalue
  
  r <- clic_request(endpoint = "keyword", query = qq, json = json)
  
  if(json) {
    return(r)
  } else {
    df <- data.frame(
      ngram               = r$data[, 2],
      target_frequency    = r$data[, 3],
      reference_frequency = r$data[, 5],
      LL                  = r$data[, 9],
      p                   = r$data[, 11]
    )
    return(df)
  }
  
}

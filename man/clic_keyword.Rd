\name{clic_keyword}
\alias{clic_keyword}

\title{Fetch keywords}
\description{
  Compute keywords (words that are used significantly more frequently
  in one corpus than they are in a reference corpus).
  The statistical measure used is
  Log Likelihood as explained by Rayson and Garside:
  Rayson, P. and Garside, R. (2000). Comparing corpora using frequency profiling.
  In proceedings of the workshop on Comparing Corpora, held in conjunction with the 38th
  annual meeting of the Association for Computational Linguistics (ACL 2000).
  1-8 October 2000, Hong Kong, pp. 1 - 6
  Available at: http://ucrel.lancs.ac.uk/people/paul/publications/rg_acl2000.pdf
}

\usage{
    clic_keyword(corpora, refcorpora, subset, refsubset, clusterlength, pvalue)
}

\arguments{
    \item{corpora}{
        1+ corpus name (e.g. 'dickens') or book name ('AgnesG') to search within
    }
    \item{refcorpora}{
        1+ corpus name (e.g. 'dickens') or book name ('AgnesG') to search within
    }
    \item{subset}{
        subset to search through, one of shortsus/longsus/nonquote/quote/all. Default 'all' (i.e. all text)
    }
    \item{refsubset}{
        subset to search through, one of shortsus/longsus/nonquote/quote/all. Default 'all' (i.e. all text)
    }
    \item{clusterlength}{
      cluster length to search for, 1 or more. Default 1
    }
    \item{pvalue}{
      A p_value of 0.0001 will select keywords that have 0.0001 *or less*
      as their p_value. It is a cut-off. One can choose one out of four
      values: 0.0001, 0.001, 0.01, or 0.05.
      If any other value is chosen, it is ignored and no filtering on
      p_value is done.
    }
}

\value{
    Returns a \code{data.frame}, one entry per result.
}

\seealso{
    \url{https://github.com/birmingham-ccr/clic/blob/2.1/server/clic/keyword.py}
}
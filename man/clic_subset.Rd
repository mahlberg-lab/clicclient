\name{clic_subset}
\alias{clic_subset}

\title{Fetch subset}
\description{
}

\usage{
    clic_subset(corpora, subset, contextsize)
}

\arguments{
    \item{corpora}{
        1+ corpus name (e.g. 'dickens') or book name ('AgnesG') to search within
    }
    \item{subset}{
        subset to search through, one of shortsus/longsus/nonquote/quote/all. Default 'all' (i.e. all text)
    }
    \item{contextsize}{
        size of context window around search results. Default 0.
    }
}

\value{
    Returns a \code{data.frame}, one entry per result.

\seealso{
    \url{https://github.com/birmingham-ccr/clic/blob/2.1/server/clic/subset.py}
}
\name{clic_concordance}
\alias{clic_concordance}

\title{Fetch concordance}
\description{
    Searches texts for given phrase(s).
}

\usage{
    clic_concordance(corpora, q, contextsize)
}

\arguments{
    \item{corpora}{
        1+ corpus name (e.g. 'dickens') or book name ('AgnesG') to search within
    }
    \item{q}{
        1+ string to search for. If multiple terms are provided, we will search for each in turn
    }
    \item{contextsize}{
        Size of context window around search results. Default 0.
    }
}

\value{
    Returns a \code{data.frame}, one entry per result. Each item is an array with the following items:
    \itemize{
      \item The left context window (if ``contextsize`` > 0, otherwise omitted)
      \item The node (i.e. the text searched for)
      \item The right context window (if ``contextsize`` > 0, otherwise omitted)
      \item Result metadata
      \item Position-in-book metadata
    }
}

\seealso{
    \url{https://github.com/birmingham-ccr/clic/blob/2.1/server/clic/concordance.py}
}
\name{clic_count}
\alias{clic_count}

\title{Count words}
\description{
    Returns counts of words within subsets.
}

\usage{
    clic_count(corpora, subset)
}

\arguments{
    \item{corpora}{
        1+ corpus name (e.g. 'dickens') or book name ('AgnesG') to search within
    }
    \item{subset}{
        subset to search through, one of shortsus/longsus/nonquote/quote/all. Default 'all' (i.e. all text)
    }
}

\value{
    Returns a \code{data.frame}, one entry per book. The first item is the book
    ID in question, the remaining items are the counts in the subsets, the order
    matching the subset querystring parameter.
}

\seealso{
    \url{https://github.com/birmingham-ccr/clic/blob/2.1/server/clic/count.py}
}
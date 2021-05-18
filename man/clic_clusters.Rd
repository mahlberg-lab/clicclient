\name{clic_clusters}
\alias{clic_clusters}

\title{Fetch clusters}
\description{
    Fetches n-grams using the CLiC API \sQuote{cluster} endpoint.
}

\usage{
    clic_clusters(shortname, length, cutoff = 5, subset = NULL, json = FALSE)
}

\arguments{
    \item{shortname}{
        Can be any value from the \sQuote{corpus} or \sQuote{shortname} columns
        returned by \code{\link{clic_lookup}}.
        Can be given as a single string, or a list of strings in which case the
        results will be concatenated.
    }
    \item{length}{
        Cluster length to search for, one of 1/3/4/5 (NB: There is no 2).
    }
    \item{cutoff}{
        The cutoff frequency, if a cluster occurs less times than this then
        it is not returned.
    }
    \item{subset}{
        A string containing Any one of \dQuote{shortsus}, \dQuote{longsus},
        \dQuote{nonquote} and \dQuote{quote}.
    }
    \item{json}{
        JSON format. TRUE/FALSE.
    }
}

\value{
    Returns a \code{data.frame} of clusters to counts.
}

\seealso{
    \url{https://github.com/birmingham-ccr/clic/server/clic/cluster.py}
}


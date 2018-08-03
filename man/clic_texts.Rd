\name{clic_texts}
\alias{clic_texts}

\title{Fetch texts}
\description{
    Fetches texts using the CLiC API.
}

\usage{
    clic_texts(shortname, subset = NULL) 
}

\arguments{
    \item{shortname}{
        Can be any value from the \sQuote{corpus} or \sQuote{shortname} columns
        returned by \code{\link{clic_lookup}}.
        Can be given as a single string, or a list of strings in which case the
        results will be concatenated.
    }
    \item{subset}{
        A string containing Any one of \dQuote{shortsus}, \dQuote{longsus},
        \dQuote{nonquote} and \dQuote{quote}.
    }
}

\value{
    Returns a single string of text.
}


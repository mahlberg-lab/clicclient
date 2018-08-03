\name{clic_tokens}
\alias{clic_tokens}

\title{Fetch tokens}
\description{
    Fetches tokens using the CLiC API \sQuote{subset} endpoint.
}

\usage{
    clic_tokens(shortname, subset = NULL, lowercase = TRUE, punctuation = FALSE) 
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
    \item{lowercase}{
        Boolean indicating if the tokens should be transformed to lower case.
    }
    \item{punctuation}{
        Boolean indicating if punctuation tokens should be included.
        Note that the puctuation tokens may include whitespace.
    }
}

\value{
    Returns a vector of tokens.
}

\seealso{
    \url{https://github.com/birmingham-ccr/clic/server/clic/subset.py}
}


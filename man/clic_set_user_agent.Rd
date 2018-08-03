\name{clic_set_user_agent}
\alias{clic_set_user_agent}
\alias{clic_get_user_agent}

\title{Set User-Agent}
\description{
    Set the \sQuote{User-Agent} string for CLiC API requests.
}

\usage{
    clic_set_user_agent(x)
    clic_get_user_agent()
}

\arguments{
    \item{x}{
        The new \sQuote{User-Agent} string.
    }
}

\details{
   Setting the User-Agent helps us to understand who is using the API.
   Ideally set the User-Agent to something that either identifies you
   or your application.

   The string you supply will be concatenated with the version details of
   this package. The concatenated string is returned.
}

\value{
    Both functions return the User-Agent string.
}


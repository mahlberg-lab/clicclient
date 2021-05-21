pkg_vars <- new.env()

#' Get/Set User-Agent
#' 
#' Get or set the \sQuote{User-Agent} string for CLiC API requests.
#' 
#' Setting the User-Agent helps us to understand who is using the API.
#' Ideally set the User-Agent to something that either identifies you
#' or your application.
#' 
#' The string you supply will be concatenated with the version details of
#' this package. The concatenated string is returned.
#'
#' @param x The new \sQuote{User-Agent} string.
#'
#' @return Both functions return the User-Agent string.
#' @export
#' 
#' @rdname clic_set_user_agent
clic_set_user_agent <- function(x){
    ua <- paste(x, " (clicclient v", packageVersion("clicclient"), ")", sep = "") 
    assign('UA', ua, pos = pkg_vars)
    return(ua)
}

#' @rdname clic_set_user_agent
#' @export
clic_get_user_agent <- function(){
    return(get('UA', pos = pkg_vars))
}

# NOT exported
clic_set_hostname <- function(x){
    assign('HOSTNAME', x, pos = pkg_vars)
    return(x)
}

.onAttach <- function(libname, pkgname) {
    clic_set_user_agent("R CLiC client")
    clic_set_hostname("clic.bham.ac.uk")
}

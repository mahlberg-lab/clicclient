pkg_vars <- new.env()

clic_set_user_agent <- function(x){
    ua <- paste(x, " (clicclient v", packageVersion("clicclient"), ")", sep = "") 
    assign('UA', ua, pos = pkg_vars)
    return(ua)
}

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

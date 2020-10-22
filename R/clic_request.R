# Makes the API requests.
# Returns the endpoint specific data structure.
#
# - endpoint: see the inline docs in /server/clic
# - query: endpoint specific parameters as a querystring
#
clic_request <- function(
    endpoint = c("subset", "corpora", "cluster", "concordance", "subset"),
    query = NULL
) {
    endpoint <- match.arg(endpoint)
    if(is.null(query)) query <- ""
    uri <- modify_url("",
        scheme = "http",
        hostname = get('HOSTNAME', pos = pkg_vars),
        path = sprintf("/api/%s", endpoint),
        query = query
    )
    ua <- paste(get('UA', pos = pkg_vars), " (clicclient v", packageVersion("clicclient"), ")", sep = "") 
    req <- GET(uri, add_headers('User-Agent' = ua, 'Accept' = "application/json"))
    if (http_error(req)) {
        stop(sprintf("Request failed: status %s - URL '%s'", status_code(req), uri))
    }
    # can ignore header so check response
    # https://tools.ietf.org/html/rfc7231#section-5.3.2
    if (http_type(req) != "application/json") {
        stop("API did not return JSON")
    }
    rv <- fromJSON( content(req, as = "text", encoding = "UTF-8") )
    if (!is.null(rv$error)) stop("API returned error: ", rv$error$message)
    if (!is.null(rv$warn)) cat("API returned warning: ", rv$warn$message)
    if (!is.null(rv$info)) cat("API returned info: ", rv$info$message)
    return(rv)
}


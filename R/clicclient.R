#' clicclient: Client for the CLiC API.
#'
#' A set of function wrappers for the CLiC API.
#' 
#' The CLiC web interface can be found at \url{http://clic.bham.ac.uk}.
#' CLiC also provides an \sQuote{application programming interface} (API)
#' that makes the data used by the web interface available in a machine
#' readable form.
#' 
#' This package offers a set of wrapper functions to facilitate
#' interaction with the API from R.
#' 
#' For a list of all documentation use \code{library(help="clicclient")}.
#' 
#' @docType package
#' @name clicclient
#' 
#' @importFrom utils packageVersion head tail
#' @importFrom stats setNames
#' @importFrom data.table rbindlist setkeyv := shift setDF
#' @importFrom httr modify_url GET http_error http_type add_headers status_code content
#' @importFrom jsonlite fromJSON 
NULL
#> NULL
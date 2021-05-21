#' Count words
#' 
#' Word counts within subsets.
#'
#' @param corpora 1+ corpus name (e.g. 'dickens') or book name ('AgnesG') to
#'   search within
#' @param subset subset to search through, one of
#'   shortsus/longsus/nonquote/quote/all. Default 'all' (i.e. all text)
#' @param metadata Return metadata. TRUE/FALSE.
#'
#' @return a \code{data.frame}, one entry per book. The first item is the book
#'   ID in question, the remaining items are the counts in the subsets, the order
#'   matching the subset querystring parameter.
#' @export
#' 
#' @seealso \url{https://github.com/birmingham-ccr/clic/blob/2.1/server/clic/count.py}
clic_count <- function(corpora, subset = c("all", "shortsus", "longsus", "nonquote", "quote"), metadata = FALSE) {

  ql <- setNames(as.list(corpora), rep("corpora", length(corpora)))
  qs <- setNames(as.list(subset), rep("subset", length(subset)))

  if(metadata) {
    qm <- c("book_titles", "chapter_start", "word_count_chapter")
    qq <- setNames(as.list(qm), rep("metadata", length(qm)))
    ql <- c(ql, qs, qq)
    r <- clic_request(endpoint = "count", query = ql)
  } else {
    qq <- c(ql, qs)
    r <- clic_request(endpoint = "count", query = ql)
  }

  x <- data.frame(
    book                 = r$data[, 1],
    total_words          = r$data[, 2],
    in_short_suspensions = r$data[, 3],
    in_long_suspensions  = r$data[, 4],
    in_non_quotes        = r$data[, 5],
    in_quotes            = r$data[, 6],
    stringsAsFactors     = FALSE
  )

  if(metadata) {
    y <- data.frame(
      book = names(r$book_titles),
      title = sapply(r$book_titles, `[`, 1),
      author = sapply(r$book_titles, `[`, 2)
    )

    z <- merge(x, y, by = "book")

    f <- function(x) {
      why <- r$word_count_chapter[[x]]
      end <- why$`_end`
      why$`_end` <- NULL
      why <- as.numeric(why, end)
      return(why)
    }

    z$word_count_chapter <- lapply(z$book, f)

    g <- function(x) {
      why <- r$chapter_start[[x]]
      end <- why$`_end`
      why$`_end` <- NULL
      why <- as.numeric(why, end)
      return(why)
    }

    z$chapter_start <- lapply(z$book, g)
  } else {
    z <- x
  }

  return(z)

}

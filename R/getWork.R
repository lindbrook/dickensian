#' Gutenberg URL
#'
#' Get URL for download.
#' @param id Numeric. Gutenberg ID of work.
#' @return Character. URL as text string.
#' @export

gutenbergURL <- function(id) {
  if (is.numeric(id) == FALSE) {
    stop('"id" must be numeric')
  }

  if (dickensian::bibliography[dickensian::bibliography$id == id, "utf8"]) {
    paste0("http://www.gutenberg.org/ebooks/", id, ".txt.utf-8")
  } else {
    paste0("http://www.gutenberg.org/files/", id, "/", id, "-0.txt")
  }
}

#' Get Work.
#'
#' Fetch work" from gutenberg.org for 'qunateda'.
#' @param id Numeric. Gutenberg ID.
#' @return quanteda object
#' @export
#' @examples
#' # A Christmas Carol:
#' getWork(46)
#'
#' # A Tale of Two Cities:
#' getWork(98)

getWork <- function(id) {
  url <- dickensian::gutenbergURL(id)
  data_character <- quanteda::texts(readtext::readtext(url))

  title <- dickensian::bibliography[dickensian::bibliography$id == id, "gTitle"]
  first <- paste0(toupper(title), "\n")

  last <- dickensian::bibliography[dickensian::bibliography$id == id,
    "last_word"]

  start <- stringi::stri_locate_first_fixed(data_character, first)[, "start"]
  end <- stringi::stri_locate_last_fixed(data_character, last)[, "end"]
  stri_sub(data_character, start, end)
}

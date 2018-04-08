#' Get Work.
#'
#' Fetch work" from gutenberg.org for 'qunateda'.
#' @param id Numeric. Gutenberg ID.
#' @param first Character. First word.
#' @param last Character. Last word.
#' @return quanteda object
#' @export
#' @examples
#' # A Christmas Carol:
#' getWork(46, "PREFACE", "One")

getWork <- function(id, first, last) {
  url <- dickensian::gutenbergURL(id)
  data_character <- quanteda::texts(readtext::readtext(url))
  start <- stringi::stri_locate_first_fixed(data_character, first)[, "start"]
  end <- stringi::stri_locate_last_fixed(data_character, last)[, "end"]
  stri_sub(data_character, start, end)
}

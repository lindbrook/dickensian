#' Sketches by Boz.
#'
#' Fetch "Sketches by Boz" from gutenberg.org
#' @return quanteda object
#' @import quanteda
#' @import stringi
#' @export
sketchesByBoz <- function() {
  url <- "http://www.gutenberg.org/files/882/882-0.txt"
  data_character <- texts(readtext::readtext(url))
  names(data_character) <- "boz"
  txt.start <- "PREFACE"
  txt.stop <- "pleasure"
  start <- stri_locate_first_fixed(data_character, txt.start)[, "start"]
  end <- stri_locate_last_fixed(data_character, txt.stop)[, "start"]
  # kwic(data_char, txt.start)
  # kwic(data_char, txt.stop)
  stri_sub(data_character, start, end)
}

#' Tokenize book by character.
#'
#' tokenize by character
#' @param book. a quanteda object.
#' @return quanteda object
#' @import quanteda
#' @import stringi
#' @export
tokenize <- function(book) {
  tokens(char_tolower(book))
}

#' Tokenize book by word.
#'
#' tokenize by word.
#' @param book. quanteda object
#' @return quanteda object
#' @import quanteda
#' @import stringi
#' @export
tokenWords <- function(book) {
  as.character(tokens(char_tolower(book), remove_punct = TRUE))
}

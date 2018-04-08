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
  end <- stri_locate_last_fixed(data_character, txt.stop)[, "end"]
  stri_sub(data_character, start, end)
}

#' Tokenize book by character.
#'
#' Tokenize by character.
#' @param x A quanteda object.
#' @return quanteda object
#' @import quanteda
#' @import stringi
#' @export

tokenize <- function(x) {
  tokens(char_tolower(x))
}

#' Tokenize book by word.
#'
#' Tokenize by word.
#' @param x A quanteda object.
#' @return quanteda object
#' @import quanteda
#' @import stringi
#' @export

tokenWords <- function(x) {
  as.character(tokens(char_tolower(x), remove_punct = TRUE))
}

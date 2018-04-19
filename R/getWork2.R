#' Download work from gutenberg.org
#'
#' Gets txt file.
#' @param id Numeric. Gutenberg ID of work.
#' @param g_title Character. Start of text (gutenberg title).
#' @param last_text Character. End of text (last word, include punctuation).
#' @param utf_8 Logical. Flag for url.
#' @return quanteda object.
#' @export

getWork2 <- function(id, g_title, last_text, utf_8 = TRUE) {
  if (is.numeric(id) == FALSE) {
    stop('"id" must be numeric')
  }

  if (is.character(g_title) == FALSE) {
    stop('"g_title" must be text string.')
  }

  if (is.character(last_text) == FALSE) {
    stop('"last_text" must be text string.')
  }

  if (utf_8) {
    # url <- paste0("http://www.gutenberg.org/ebooks/", id, ".txt.utf-8")
    url <- paste0("http://www.gutenberg.org/cache/epub/", id, "/pg", id, ".txt")
  } else {
    url <- paste0("http://www.gutenberg.org/files/", id, "/", id, "-0.txt")
  }

  data_character <- quanteda::texts(readtext::readtext(url))
  first <- paste0(toupper(g_title), "\n")
  start <- stringi::stri_locate_first_fixed(data_character, first)[, "start"]
  end <- stringi::stri_locate_last_fixed(data_character, last_text)[, "end"]
  stringi::stri_sub(data_character, start, end)
}

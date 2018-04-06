#' Gutenberg URL
#'
#' Get URL for download.
#' @param id Numeric. Gutenberg ID of work.
#' @return Character. URL as text string.
#' @export

gutenbergURL <-  function(id) {
  if (is.numeric(id) == FALSE) {
    stop('"id" must be numeric')
  }

  if (dickensian::bibliography[dickensian::bibliography$ID == id, "utf8"]) {
    paste0("http://www.gutenberg.org/ebooks/", id, ".txt.utf-8")
  } else {
    paste0("http://www.gutenberg.org/files/", id, "/", id, "-0.txt")
  }
}

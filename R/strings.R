#' Cleans a strings that is supposed to be alphanumeric name
#'
#' @param str String to be cleaned
#'
#' @returns A character vector
#'
#' @noRd
clean_string <- function(str) {
  str |>
    stringr::str_replace(pattern = "(\\s\\/)", replacement = ",") |>
    stringr::str_extract(pattern = "^([[:alnum:]]|[[:space:]]|[,:\\(\\)])+") |>
    trimws()
}

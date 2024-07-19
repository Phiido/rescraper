#' Cleans a string that is expected to be an alphanumeric name.
#'
#' This function takes a string as input and cleans it by removing any
#' non-alphanumeric characters and whitespace followed by a forward slash.
#' The resulting string is then trimmed of leading and trailing whitespace.
#'
#' @param str String to be cleaned.
#'
#' @returns A character vector with the cleaned string.
#'
#' @examples
#' clean_string("Austria [ref]")
#' #> [1] "Austria"
#'
#' clean_string("RTBF / VRT")
#' #> [1] "RTBF, VRT"
#'
#' clean_string("ARD (NDR)")
#' #> [1] "ARD (NDR)"
#'
#' @export
clean_string <- function(str) {
  str |>
    stringr::str_replace(pattern = "(\\s\\/)", replacement = ",") |>
    stringr::str_extract(pattern = "^([[:alnum:]]|[[:space:]]|[,:\\(\\)])+") |>
    trimws()
}

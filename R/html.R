#' Retrieves text from cells derived from HTML elements
#'
#' Extracts the text from an `html_element` object. This is useful for
#' extracting text from HTML cells, which can be complex to manage.
#'
#' @param x an `html_element` object from `rvest` package
#'
#' @returns a character vector with the text from the `html_element`
#'
#' @noRd
get_cell_text <- function(x) {
  x |>
    rvest::html_text2() |>
    stringr::str_split(pattern = "\\n") |>
    unlist() |>
    stringr::str_remove(pattern = "\\[\\d\\]") |>
    stringr::str_remove(pattern = "\\(.*\\)") |>
    stringr::str_remove(pattern = "Semi-finals:,\\s|Final:,\\s") |>
    stringr::str_trim()
}

get_infobox_row <- function(html, tag) {
  html |>
    rvest::html_elements(xpath = glue::glue('//th[contains(., "{tag}")]')) |>
    rvest::html_elements(xpath = "following-sibling::td/child::node()[not(self::style)]") |>
    get_cell_text() |>
    lapply(\(x) x[nzchar(x)]) |> # keep all non-empty strings
    unlist() |>
    toString()
}

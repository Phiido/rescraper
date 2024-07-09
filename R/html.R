#' Extracts HTML from a httr2 json response
#'
#' Extracts the raw and crude HTML part from a [httr2] response that is
#' classified as json.
#'
#' @param response A HTTP response of class `httr2_response`
#'
#' @seealso [httr2::resp_body_json()]
#'
#' @returns A character vector
#'
#' @noRd
extract_html <- function(response) {
  if (!methods::is(response, "httr2_response")) cli::cli_abort("Argument response is of wrong class")


  httr2::resp_body_json(response)[["parse"]][["text"]][[1]]
}

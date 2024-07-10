#' Convert httr2 JSON response to HTML
#'
#' Extracts the raw and crude HTML part from a [httr2] response that is
#' classified as JSON.
#'
#' @param response A character vector with raw JSON
#'
#' @seealso [httr2::resp_body_json()]
#'
#' @return A character vector
#'
#' @noRd
json_to_html <- function(response) {
  httr2::resp_body_json(response)[["parse"]][["text"]][[1]]
}

#' Extract a property from a httr2 JSON response
#'
#' @param response A httr2 response object
#' @param property The property to extract from the query.
#'
#' @returns A character vector containing the extracted property.
#'
#' @noRd
json_extract_query <- function(response, property) {
  if (!methods::is(response, "httr2_response")) cli::cli_abort("Argument response is of wrong class")

  httr2::resp_body_json(response)[["query"]][[1]] |>
    lapply(\(x) {
      if (x[["type"]] == "page") {
        x[[property]]
      }
    }) |>
    unlist()
}

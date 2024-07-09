#' Turn httr2 response from raw json into HTML
#'
#' Extracts the raw and crude HTML part from a [httr2] response that is
#' classified as json.
#'
#' @param response A character vector with raw json
#'
#' @seealso [httr2::resp_body_json()]
#'
#' @returns A character vector
#'
#' @noRd
json_to_html <- function(response) {
  httr2::resp_body_json(response)[["parse"]][["text"]][[1]]
}

#' Extract the query response from a httr2 json response
#'
#' @param response Takes a response object from [httr2]
#' @param property Which property to extract from the query
#'
#' @returns A character vector
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

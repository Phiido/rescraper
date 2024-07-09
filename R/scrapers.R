#' Scrape countries have participated or still participates in Eurovision as an entry
#'
#' @param quiet Should scraping be quiet and not output to console
#'
#' @returns A tibble
#'
#' @export
scrape_countries <- function(quiet = FALSE) {
  if (!quiet) cli::cli_alert_info("Scraping country info")

  page_subdir <- "List_of_countries_in_the_Eurovision_Song_Contest"

  response <- get_endpoint("wiki") |>
    httr2::request() |>
    httr2::req_url_query(
      action = "parse",
      format = "json",
      page = page_subdir,
      section = 1, # Participants
      prop = "text"
    ) |>
    perform_polite_request()

  table <- extract_html(response) |>
    rvest::read_html() |>
    rvest::html_element(xpath = "//table[2]") |>
    rvest::html_table() |>
    janitor::clean_names() |>
    dplyr::rename(name = dplyr::starts_with("country"),
                  broadcaster = dplyr::starts_with("broadcaster")) |>
    dplyr::mutate(name = clean_string(.data$name),
                  broadcaster = clean_string(.data$broadcaster),
                  code = resc::get_countrycode(.data$name), .before = .data$name) |>
    dplyr::select(.data$code:.data$broadcaster) |>
    tibble::as_tibble()

  if (!quiet) cli::cli_alert_success("Successfully scraped countries!")

  return(table)
}

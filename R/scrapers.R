#' Scrape countries have participated or still participates in Eurovision as an entry
#'
#' @returns A tibble
#'
#' @export
scrape_countries <- function() {
  n_countries <- 0L
  cli::cli_progress_step(msg = "Scraping country info..",
                         msg_done = "Scraped country info on {n_countries} countries",
                         msg_failed = "Failed to scrape country info..",
                         spinner = TRUE)

  response <- request_content_page(page = "List_of_countries_in_the_Eurovision_Song_Contest",
                                   api = "wiki",
                                   list(action  = "parse",
                                        format  = "json",
                                        section = 1, # Participants
                                        prop    = "text"))

  table <- json_to_html(response) |>
    rvest::read_html() |>
    rvest::html_element(xpath = "//table[2]") |>
    rvest::html_table() |>
    janitor::clean_names() |>
    dplyr::rename(
      name = dplyr::starts_with("country"),
      broadcaster = dplyr::starts_with("broadcaster")
    ) |>
    dplyr::mutate(
      name = clean_string(.data$name),
      broadcaster = clean_string(.data$broadcaster),
      code = resc::get_countrycode(.data$name), .before = .data$name
    ) |>
    dplyr::select(.data$code:.data$broadcaster) |>
    tibble::as_tibble()

  n_countries <- nrow(table)

  cli::cli_progress_update()

  return(table)
}

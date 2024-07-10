#' Scrapes info about countries in Eurovision Song Contest
#' 
#' This function fetches information about countries that have participated or still participate in
#' the Eurovision Song Contest as an entry.
#'
#' @returns A tibble containing the scraped information.
#'
#' @examples
#' scrape_countries()
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

#' Scrape information about the Eurovision Song Contest events
#'
#' This function fetches information about the Eurovision Song Contest events
#' for the specified years. If no years are specified, all available years
#' are used.
#'
#' @param years A vector of years for which to scrape information. Defaults to
#'   all available years using [resc::get_years()].
#'
#' @return A tibble containing the scraped information. The tibble has the
#'   following columns:
#'   - `year`: The year of the contest.
#'   - `title`: The title of the contest.
#'   - `url`: The URL of the contest's Wikipedia page.
#'   - `host_city`: The city where the contest was held.
#'   - `host_country`: The country where the contest was held.
#'   - `host_country_code`: The country code of the country where the contest
#'     was held.
#'   - `location`: The location where the contest was held (e.g., "Stockholm,
#'     Sweden").
#'   - `date`: The date of the contest.
#'   - `result`: The result of the contest (as a tibble).
#'
#' @export
scrape_contests <- function(years = resc::get_years()) {
  msg <- ""
  n_contests <- 0L

  cli::cli_progress_step("Scraping contest info{msg}",
                         msg_done = "Scraped info on {n_contests} contests",,
                         msg_failed = "Failed to scrape contest info..",
                         spinner = TRUE)

  title_contests <- request_content_page(
    page = "List_of_Eurovision_Song_Contest_entries",
    api  = "wiki",
    list(
      action  = "query",
      format  = "json",
      list    = "categorymembers",
      cmlimit = 150,
      cmtitle = "Category:Eurovision_Song_Contest_by_year",
      cmprop  = "title|type"
    )
  ) |>
    json_extract_query(property = "title")

  title_contests <- title_contests[1] # TODO remove before commit!

  list_tables <- list()

  for (title in title_contests) {
    year <- stringr::str_extract(string = title, pattern = "[0-9]+$")
    msg <- glue::glue(": currently on {year}")
    n_contests <- n_contests + 1

    cli::cli_progress_update()

    # Currently loads the whole page as infobox is before any section
    # however, this may not be a problem as the page is then cached
    # for any other scrapes / queries.
    html_page <- request_content_page(
      page = title,
      api  = "wiki",
      list(
        action  = "parse",
        format  = "json",
        page    = title,
        prop    = "text"
      )
    ) |>
      json_to_html() |>
      rvest::read_html()

    html_infobox <- html_page |>
      rvest::html_element(css = 'table.infobox')

    url_logo <- html_page |>
      rvest::html_element(xpath = '//a[contains(@href, "Logo.svg")]') |>
      rvest::html_attr("href")

    location <- html_infobox |>
      rvest::html_element(xpath = '//th[contains(., "Venue")]/following-sibling::td') |>
      rvest::html_text2()

    venue <- stringr::str_extract(location, pattern = "^.*?(?=\\n)")
    city <- stringr::str_extract(location, pattern = "(?<=\\n).*?(?=,)")
    country <- stringr::str_extract(location, pattern = "(?<=, ).*$")

    list_tables[[year]] <- infobox
  }

  output_table <- tibble::as_tibble(do.call("rbind", 1))

  cli::cli_progress_update()

  return(output_table)
}


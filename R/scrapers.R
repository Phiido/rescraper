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
  cli::cli_progress_step(
    msg = "Scraping country info..",
    msg_done = "Scraped country info on {n_countries} countries",
    msg_failed = "Failed to scrape country info..",
    spinner = TRUE
  )

  response <- request_content_page(
    page = "List_of_countries_in_the_Eurovision_Song_Contest",
    api = "wiki",
    list(
      action = "parse",
      format = "json",
      section = 1, # Participants
      prop = "text"
    )
  )


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
#'   - `edition`: The contest edition.
#'   - `slogan`: The contest slogan
#'   - `url_logo`: URL of the contest logo.
#'   - `country_code`: Country code for the hosting country
#'   - `city`: The city where the contest was held.
#'   - `venue`: The venue that the contest was held at
#'   - `presenters`: Names of presenters
#'   - `musical_director`: Name of musical director
#'   - `director`: Name of director
#'   - `executive_producers`: Names of executive producers
#'   - `executive_supervisors`: Names of executive supervisors
#'
#' @export
scrape_contests <- function(years = resc::get_years()$all) {
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
      cmlimit = 150, # will last until year 2109
      cmtitle = "Category:Eurovision_Song_Contest_by_year",
      cmprop  = "title|type"
    )
  ) |>
    json_extract_query(property = "title")

  years <- stringr::str_extract(string = title_contests, pattern = "[0-9]+$") |>
    as.numeric()

  years <- years[!years %in% resc::get_years()$cancelled]
  editions <- pmin(years - 1955, seq_along(years))

  list_tables <- vector("list", length(years))

  for (i in seq_along(years)) {
    this_title <- title_contests[i]
    this_year <- years[i]
    this_edition <- editions[i]

    msg <- glue::glue(" {this_year}")
    n_contests <- n_contests + 1

    cli::cli_progress_update()

    # Note: currently loads the whole page as infobox is before any section.
    # However, this may not be a problem as the page is then cached for any
    # other scrapes / queries.
    html_page <- request_content_page(
      page = this_title,
      api = "wiki",
      list(
        action  = "parse",
        format  = "json",
        page    = this_title,
        prop    = "text"
      )
    ) |>
      json_to_html() |>
      rvest::read_html()

    html_infobox <- html_page |>
      rvest::html_element(css = "table.infobox")

    this_slogan <- html_page |>
      rvest::html_element(xpath = '//td[contains(@class, "infobox-subheader")]') |>
      rvest::html_text()

    cli::cli_progress_update()

    logo_url <- html_page |>
      rvest::html_element(xpath = '//td[@class="infobox-image"]/span/a') |>
      rvest::html_attr("href")

    location <- html_infobox |>
      rvest::html_element(xpath = '//th[contains(., "Venue")]/following-sibling::td') |>
      rvest::html_text2()

    country <- stringr::str_extract(location, pattern = "(?<=, ).*$")

    cli::cli_progress_update()

    this_contest <- tibble::tibble_row(
      year = this_year,
      edition = this_edition,
      slogan = this_slogan,
      url_logo = glue::glue('{get_domain_name("wiki")}{logo_url}'),
      country_code = resc::get_countrycode(country),
      city = stringr::str_extract(location, pattern = "(?<=\\n).*?(?=,)"),
      venue = stringr::str_extract(location, pattern = "^.*?(?=\\n)"),
      presenters = get_infobox_row(html_infobox, tag = "Presenter"),
      musical_director = get_infobox_row(html_infobox, tag = "Musical director"),
      director = get_infobox_row(html_infobox, tag = "Directed by"),
      executive_producers = get_infobox_row(html_infobox, tag = "Executive producer"),
      executive_supervisor = get_infobox_row(html_infobox, tag = "Executive supervisor")
    )

    cli::cli_progress_update()

    list_tables[[as.character(this_year)]] <- this_contest
  }

  output_table <- do.call(dplyr::bind_rows, list_tables)

  cli::cli_progress_update()

  return(output_table)
}

#' Scrapes the voting system information from the Eurovision Song Contest.
#'
#' This function scrapes the voting system information from the Eurovision
#' Song Contest wiki page. The information is returned as a `list` with the
#' following columns:
#'
#' - `year_start`: Starting year for ruleset.
#' - `year_end`: End year for ruleset.
#' - `point_allocation`: How points are allocated from each country.
#' - `use`: How the tie breaking rule is used.
#' - `description`: A description of the ruleset.
#'
#' @return A named list with the voting system rules and tie breaking rules.
#'
#' @export
scrape_voting_systems <- function() {

  cli::cli_progress_step("Scraping voting system info",
                         msg_done = "Scraped info on voting system info",,
                         msg_failed = "Failed to scrape voting system info..",
                         spinner = TRUE)

  page_title <- "Voting_at_the_Eurovision_Song_Contest"

  html_tables <- request_content_page(
    page = page_title,
    api = "wiki",
    list(
      action  = "parse",
      format  = "json",
      page    = page_title,
      prop    = "text"
    )
  ) |>
    json_to_html() |>
    rvest::read_html() |>
    rvest::html_elements(xpath = '//table[contains(@class, "wikitable")]')

  # warnings present regarding tidyselect and the use of .data
  # and numeric coercion from elements with alpha characters
  suppressWarnings({
    table_voting <- html_tables[[1]] |>
      rvest::html_table() |>
      janitor::clean_names() |>
      dplyr::rename(point_allocation = .data$points, description = .data$voting_system) |>
      dplyr::mutate(year = stringr::str_trim(stringr::str_remove(.data$year, "\\(.*\\)"))) |>
      dplyr::mutate(
        year_start = stringr::str_extract(.data$year, pattern = "^[0-9]{4}"),
        year_end = stringr::str_extract(.data$year, pattern = "[0-9]{4}$"),
        .before = .data$year
      ) |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("year"), as.numeric)) |>
      dplyr::mutate(description = stringr::str_remove_all(.data$description, pattern = "\\[.*\\]")) |>
      dplyr::select(!.data$year)

    table_tie <- html_tables[[7]] |>
      rvest::html_table() |>
      janitor::clean_names() |>
      dplyr::mutate(
        year_start = stringr::str_extract(.data$year, pattern = "^[0-9]{4}"),
        year_end = stringr::str_extract(.data$year, pattern = "[0-9]{4}$"),
        .before = .data$year
      ) |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("year"), as.numeric)) |>
      dplyr::mutate(description = stringr::str_remove_all(.data$description, pattern = "\\[.*\\]")) |>
      dplyr::select(!.data$year)
  })

  voting_system <- list(voting = table_voting, tie_break = table_tie)

  return(voting_system)
}

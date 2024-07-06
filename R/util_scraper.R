#' Get user agent used by rescraper
#'
#' @returns a list
#'
#' @noRd
get_user_agent <- function() {
  pkg <- utils::packageDescription("rescraper")
  user_agent_name <- glue::glue("{pkg$Package}/{pkg$Version} ({pkg$URL})")

  list(
    name = user_agent_name,
    delay = 5
  )
}

#' Creates a endpoint url for a API.
#'
#' Currently supports the following APIs:
#'
#' * `wiki` - Wikimedia API
#'
#' @param api which api to target
#' @param page which subdirectory to target
#'
#' @returns a string
#'
#' @examples
#' create_api_url(api = "wiki", subdir = "Eurovision")
#'
#' @noRd
create_api_url <- function(api, page) {
  base_url <- switch(api,
    "wiki" = "https://en.wikipedia.org/w/api.php?action=parse&format=json&page="
  )
  glue::glue("{base_url}{page}")
}

#' Create a HTTP request for a Wikipedia page
#'
#' Sends a polite request through the Wikimedia API.
#'
#' @param page the subdirectory part of the URL
#'
#' @returns a HTTP request
#'
#' @noRd
request_wiki_page <- function(page) {
  parsed_url <- httr2::url_parse(page)

  if ("query" %in% names(parsed_url)) page <- parsed_url$query$page

  polite_request(url = create_api_url(api = "wiki", page = page))
}

#' A semi polite HTTP request
#'
#' Uses [httr2::request()] and ensures adequate time between requests. Does not
#' take into account of robots.txt. However, as very specific URLs are scraped,
#' robots.txt has been manually checked.
#'
#' @param url string with url
#' @param delay how long delay between requests in seconds
#' @param user_agent which user-agent to use; Default `get_user_agent()$delay`
#' @param dry whether `httr2::request()` should do a dry run
#'
#' @returns a list from `httr2`
#'
#' @noRd
polite_request <- memoise::memoise(
  function(url,
           delay = get_user_agent()$delay,
           user_agent = get_user_agent()$name,
           dry = FALSE) {
    if (delay < 1) {
      cli::cli_alert_warning("Delay of {delay} is too short, reverting to 1 second")
      delay <- 1
    }

    Sys.sleep(delay)

    request <- httr2::request(url) |>
      httr2::req_user_agent(string = user_agent)

    if (dry) {
      httr2::req_dry_run(request)
    } else {
      httr2::req_perform(request)
    }
  }
)

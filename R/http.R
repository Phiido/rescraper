#' Get user agent used by rescraper
#'
#' @returns a list with user agent name and delay
#'
#' @noRd
get_user_agent <- function() {
  pkg <- utils::packageDescription("rescraper")
  user_agent_name <- glue::glue("{pkg$Package}/{pkg$Version} ({pkg$URL})")

  list(
    name = user_agent_name,
    delay = 1
  )
}

#' Get a endpoint using shorthands
#'
#' Simply returns a endpoint URL based on shorthand names.
#'
#' Currently only takes `"wiki"` as host.
#'
#' @param host Which host to target
#'
#' @returns A string
#'
#' @noRd
get_endpoint <- function(host) {
  switch(host,
    "wiki" = "https://en.wikipedia.org/w/api.php"
  )
}

#' A semi polite HTTP request
#'
#' Uses [httr2::request()] and ensures adequate time between requests. Does not
#' take into account of robots.txt. However, as very specific URLs are scraped,
#' robots.txt has been manually checked.
#'
#' @param url String with url
#' @param delay How long delay between requests in seconds; Default `get_user_agent()$delay`
#' @param user_agent Which user-agent to use; Default `get_user_agent()$name`
#'
#' @returns
#' Either a:
#' * a HTTP response
#' * error status code
#' * a class of `httr2_failure`
#'
#' @seealso [httr2::req_perform()]
#'
#' @noRd
perform_polite_request <- function(request,
                                   user_agent = get_user_agent()$name,
                                   rate = get_user_agent()$delay) {
  response <- request |>
    httr2::req_user_agent(string = user_agent) |>
    httr2::req_throttle(rate = rate) |>
    httr2::req_cache(path = cache_get()) |>
    httr2::req_perform()

  return(response)
}

#' Return the path name for the rescraper cache directory
#'
#' @returns A `fs::path` object
#'
#' @noRd
cache_path <- function() {
  fs::path(tools::R_user_dir(package = "rescraper", which = "cache"))
}

#' Get the path for the rescraper cache directory
#'
#' Creates the directory if it doesn't exist.
#'
#' @returns A `fs::path` object
#'
#' @noRd
cache_get <- function() {
  path <- cache_path()

  if (!fs::dir_exists(path)) fs::dir_create(path)

  return(path)
}

#' Remove the rescraper cache directory
#'
#' @returns NULL
#'
#' @noRd
cache_remove <- function() {
  fs::dir_delete(cache_path())
}

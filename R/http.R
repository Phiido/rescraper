#' Get the User Agent for rescraper
#' 
#' @returns A named list with the following elements:
#'   - `name`: a string representing the User-Agent HTTP header
#'   - `delay`: a numeric value for throttling HTTP requests
#'
#' @details The User-Agent HTTP header is used by rescraper to identify itself when
#'   making HTTP requests. The delay is used by rescraper's HTTP request
#'   functionality to ensure a minimum time between requests.
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
#' This function maps a shorthand (e.g., "wiki") to its corresponding endpoint
#' URL. Currently, only the shorthand "wiki" is supported, which corresponds
#' to the URL "https://en.wikipedia.org/w/api.php".
#'
#' @param host A character string representing the shorthand for the host.
#'   Currently, only the shorthand "wiki" is supported.
#'
#' @return A character string representing the URL of the endpoint.
#'
#' @noRd
get_endpoint <- function(host) {

  name <- get_domain_name(host)

  switch(host,
    "wiki" = glue::glue("{name}/w/api.php")
  )
}

#' Return the domain name associated with a given shorthand
#'
#' This function maps a shorthand (e.g., "wiki") to its corresponding domain
#' name.
#'
#' @param host A character string representing the shorthand for the domain.
#'   Currently, only the shorthand "wiki" is supported, which corresponds
#'   to "https://en.wikipedia.org".
#' 
#' @returns A character string representing the corresponding domain name.
#' 
#' @noRd
get_domain_name <- function(host) {
  switch(host,
         "wiki" = "https://en.wikipedia.org")
}

#' Send a polite HTTP request using httr2::request
#'
#' Uses [httr2::request()] to send HTTP requests. Ensures adequate time between
#' requests by specifying a delay. This function does not check robots.txt.
#' However, as very specific URLs are scraped, robots.txt has been manually checked.
#'
#' @param url A string representing the URL to request.
#' @param delay A numeric value representing the delay in seconds between requests.
#'   Defaults to the delay specified in the user agent.
#' @param user_agent A string representing the User-Agent HTTP header.
#'   Defaults to the user agent specified in the package.
#'
#' @returns
#' Returns a `httr2_response` object if the request was successful or an object
#' of class `httr2_failure` if the request failed. See [httr2::req_perform()] for
#' more details on the returned object.
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

#' Send a request query for a page
#'
#' This function sends a query to a specified API for a given page. It takes
#' the page name, the API to target, and additional parameters to be sent to
#' [httr2::req_url_query()].
#'
#' @param page A character string representing the name of the page to query.
#' @param api A character string representing the API to target.
#' @param ... Additional parameters to be sent to [httr2::req_url_query()].
#'
#' @returns A [httr2] response object.
#'
#' @noRd
request_content_page <- function(page, api, ...) {
  params <- unlist(...)
  params[["page"]] <- page

  get_endpoint(api) |>
    httr2::request() |>
    httr2::req_url_query(!!!params) |>
    perform_polite_request()
}

#' Create path name for the rescraper cache directory
#'
#' Returns the path name for the cache directory used by rescraper. The cache
#' directory is located in the user-specific R directory and is used to store
#' cached responses from the API.
#'
#' @returns A `fs::path` object representing the path name of the cache directory
#'
#' @noRd
cache_path <- function() {
  fs::path(tools::R_user_dir(package = "rescraper", which = "cache"))
}

#' Get the path for the rescraper cache directory
#'
#' Creates the directory if it doesn't exist. The cache directory is located in
#' the user-specific R directory and is used to store cached responses from the
#' API.
#'
#' @return A `fs::path` object representing the path to the cache directory
#'
#' @noRd
cache_get <- function() {
  path <- cache_path()

  if (!fs::dir_exists(path)) fs::dir_create(path)

  return(path)
}

#' Remove the rescraper cache directory
#'
#' Removes the cache directory used by rescraper. This directory is located in
#' the user-specific R directory and is used to store cached responses from the
#' API. This function is useful for cleaning up the cache or when you want to
#' start fresh.
#'
#' @returns NULL
#'
#' @noRd
cache_remove <- function() {
  fs::dir_delete(cache_path())
}

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

  name <- get_domain_name(host)

  switch(host,
    "wiki" = glue::glue("{name}/w/api.php")
  )
}

#' Return the domain name using a shorthand
#'
#' @param host
#'
#' @returns A string with domain name
#'
#' @noRd
get_domain_name <- function(host) {
  switch(host,
         "wiki" = "https://en.wikipedia.org")
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

#' Send a request query for a page
#'
#' @param page Which page to query
#' @param api Which api to target
#' @param ... Additional parameters to be sent to [httr2::req_url_query()]
#'
#' @returns A [httr2] response
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

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
                                   delay = get_user_agent()$delay,
                                   user_agent = get_user_agent()$name) {
  if (delay < 1) {
    cli::cli_alert_warning("Delay of {delay} is too short, reverting to 1 second")
    delay <- 1
  }

  Sys.sleep(delay)

  response <- request |>
    httr2::req_user_agent(string = user_agent) |>
    httr2::req_perform()

  return(response)
}

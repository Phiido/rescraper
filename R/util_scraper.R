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

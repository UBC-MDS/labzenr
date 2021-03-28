#' Tally Available Points in Lab
#'
#'
#' @inheritParams extract_points
#' @inheritParams parse_lab
#'
#' @importFrom dplyr group_by if_else filter summarise mutate across one_of
#' @importFrom rlang sym
#'
#' @return A dataframe indicating total optional and required number of points.
#' @export
#'
#' @examples
#' # R markdown
#' notebook <- system.file("extdata", "dummylab.Rmd", package = "labzenr")
#' count_points(notebook)
#'
#' # Python notebook
#' notebook <- system.file("extdata", "dummylab.ipynb", package = "labzenr")
#' count_points(notebook)
count_points <- function(notebook = NULL, margins = TRUE) {
  # find notebook if not provided
  notebook <- notebook %||% find_assignment()

  dat <- extract_points(notebook, margins = margins)
  tab <- dat %>%
    mutate(
      type = if_else(dat$optional, "Optional", "Non-Optional"),
      across(!!sym("type"), factor,
        levels = c("Non-Optional", "Optional")
      )
    ) %>%
    group_by(!!sym("type"), .drop = FALSE) %>%
    summarise(across(one_of(c("total", "prop")), sum), .groups = "drop")

  if (margins) {
    tab <- janitor::adorn_totals(tab, "row")
  }

  tab
}

#' Check for a Repo Link
#'
#' Check whether the user has included the github repo link in his/her
#' repository
#'
#' @inheritParams parse_lab
#' @return A logical which indicates whether the repo link exists or not
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Python notebook example should work as it DOES not have a link
#' notebook <- system.file("extdata", "dummylab.ipynb", package = "labzenr")
#' labzenr:::check_repo_link(notebook)
#'
#' # R markdown example should fail as it does NOT have a link
#' notebook <- system.file("extdata", "dummylab.Rmd", package = "labzenr")
#' labzenr:::check_repo_link(notebook)
#' }
check_repo_link <- function(notebook = NULL) {

  # parse lab using parse_lab function
  parsed_lab <- parse_lab(notebook)

  # regex for github link
  site <- "(https://)?(www.)?github\\.ubc\\.ca"
  regex <- paste0(site, "\\/MDS-\\d{4}-\\d{2}\\/DSCI_\\d{3}_lab\\d_[a-z]+")

  # Check whether each element contains the link
  link_ind <- stringr::str_detect(parsed_lab, regex)

  # Extract the link
  link_cell <- parsed_lab[link_ind]
  link <- stringr::str_extract(link_cell, regex)

  if (length(link) >= 1) {
    usethis::ui_done("You included the repo link {ui_field(link)}")
    return(invisible(TRUE))
  } else {
    usethis::ui_oops("No Github repo link found")
    return(invisible(FALSE))
  }
}

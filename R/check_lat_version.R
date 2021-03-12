#' Check whether the user has pushed the latest version in his/her repository
#'
#' @return A logical which indicates whether the last pushed version is latest
#'   or not
#' @export
#' @examples
#' \dontrun{
#' check_lat_version()
#' }
check_lat_version <- function() {

  tryCatch(
    expr = {
      message(usethis:::check_branch_pushed())
      usethis::ui_done("Remote has the latest commit")
      return(invisible(TRUE))
    },
    error = function(e){
      usethis::ui_oops('Remote does not have the latest commit')
      return(invisible(FALSE))
    }
  )
}


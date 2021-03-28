#' Check MDS Lab Mechanics
#'
#' Perform mechanics checks on a MDS Lab. This function checks that you have a
#' Github repo link, that you have pushed your latest commit, and that you have
#' at least three commit messages authored by you in your history.
#'
#' @inheritParams parse_lab
#' @inheritParams check_commits
#' @return The function prints the results of the mechanics checks to screen.
#'  Silently returns TRUE if all the checks are passed.
#' @importFrom rlang %||%
#' @export
#'
#' @examples
#' \dontrun{
#' check_mechanics()
#' }
check_mechanics <- function(notebook = NULL, repo = ".",
                            branch = usethis::git_branch_default()) {
  lab <- notebook %||% find_assignment()

  c1 <- check_repo_link(lab)
  c2 <- check_lat_version(repo = repo)
  c3 <- check_commits(
    repo = repo, branch = branch
  )

  invisible(c1 & c2 & c3)
}

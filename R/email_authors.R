
#' Write an email to project authors
#'
#' Invokes \code{utils::\link[utils]{browseURL}("mailto://[author emails]")} for
#' a specified project, or for the currently open project if \code{project} is
#' left as \code{NULL}.
#'
#' The success of this function depends on the platform and the specified
#' \code{browser}. See the \strong{Details} and \strong{URL schemes} sections of
#' \code{utils::\link[utils]{browseURL}()}.
#'
#' If \code{project = NULL}, the function selects the project in the
#' \code{\link{projects}()} table whose \code{path} is equal to
#' \code{rstudioapi::\link[rstudioapi]{getActiveProject}()}.
#'
#' @param project Project \code{id} or unambiguous substring of the project name
#'   from the \code{\link{projects}()} table. Defaults to \code{NULL} (see
#'   \strong{Details}).
#' @param browser,encodeIfNeeded See \code{utils::\link[utils]{browseURL}()}.
#'
#' @seealso \code{utils::\link[utils]{browseURL}()};
#'   \code{rstudioapi::\link[rstudioapi]{getActiveProject}()} for information on
#'   \code{browser} and \code{encodeIfNeeded} arguments.
#'
#' @examples
#' # Wrapped in if (interactive()) because this function is interactive by nature.
#' if (interactive()) {
#'
#'   # If you have a projects() project open, just run it:
#'   email_authors()
#'
#'   # Otherwise, specify a project:
#'
#'   ###########################################################################
#'   # Setup
#'   old_home <- Sys.getenv("HOME")
#'   old_ppath <- Sys.getenv("PROJECTS_FOLDER_PATH")
#'   temp_dir <- tempfile("dir")
#'   dir.create(temp_dir)
#'   Sys.unsetenv("PROJECTS_FOLDER_PATH")
#'   Sys.setenv(HOME = temp_dir)
#'   setup_projects(path = temp_dir)
#'   new_author("Rhonda", "Rondale", email = "ronda.rondale@co.uk")
#'   new_author("Betty", "Betts", email = "betty@co.uk")
#'   new_project("Inventing the Ring of Power", authors = c("Betty", "Ron"))
#'   ###########################################################################
#'
#'   email_authors("Ring of Power")
#'
#'   ###########################################################################
#'   # Cleanup (or just restart R)
#'   Sys.setenv(HOME = old_home, PROJECTS_FOLDER_PATH = old_ppath)
#' }
#' @importFrom rlang .data
#' @export
email_authors <- function(project = NULL,
                          browser = getOption("browser"),
                          encodeIfNeeded = FALSE) {

  p_path <- get_p_path()

  projects_table <- projects_internal(p_path, archived = TRUE)
  authors_table <- authors_internal(p_path)
  pa_assoc <- pa_assoc_internal(p_path)

  if (is.null(project)) {
    project_id <-
      dplyr::filter(
        projects_table,
        .data$path == rstudioapi::getActiveProject()
      )$id

    if (length(project_id) != 1L) {
      if (length(project_id) == 0L) {
        stop(
          "No project found in the projects() table with the path:\n",
          rstudioapi::getActiveProject()
        )
      } else {
        stop(
          "Multiple projects found in the projects() table with the path:\n",
          rstudioapi::getActiveProject()
        )
      }
    }

  } else {
    project_id <-
      validate_unique_entry(
        x     = project,
        table = projects_table,
        what  = "project"
      )$id
  }

  author_ids <- dplyr::filter(pa_assoc, .data$id1 == project_id)$id2

  if (length(author_ids) == 0L) {
    print(dplyr::filter(projects_table, .data$id == project_id))
    stop("\nThe above project has no authors.")
  }

  author_emails <-
    authors_table %>%
    dplyr::filter(.data$id %in% author_ids & !is.na(.data$email)) %>%
    `[[`("email")

  if (length(author_emails) == 0L) {
    print(dplyr::filter(projects_table, .data$id == project_id))
    print(dplyr::filter(authors_table, .data$id %in% author_ids))
    stop("\nThe above authors of the above project have no email addresses.")
  }

  url <- paste0("mailto://", paste(author_emails, collapse = ", "))

  utils::browseURL(url, browser, encodeIfNeeded)
}

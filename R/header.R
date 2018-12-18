#' @title Print project header to console
#' @description This function displays the report header for a project. The
#'   project header consists of: 1) the project title; 2) the author list; 3)
#'   the list of author affiliations; and 4) corresponding author information.
#'   The function is helpful when, after editing details of the project (e.g.,
#'   any of the above information), you want to update your markdown documents.
#'   The displayed markdown can be pasted directly in place of the header within
#'   the markdown documents (specifically \emph{01_protocol.Rmd} and
#'   \emph{04_report.Rmd}).
#'
#' @param project Project \code{id} or unambiguous substring of the project name
#'   from the \code{\link{projects}()} tibble.
#' @param archived Logical indicating whether or not the function should
#'   consider archived projects when determining which project the user is
#'   referring to in the \code{project} argument. \code{FALSE} by default.
#'
#'   See the \strong{Details} section of \code{\link{archive_project}()} for
#'   more information on the "archived" status of a project.
#'
#' @examples
#' \donttest{
#' # Included in \donttest{} to save time on example checking.
#'
#' # SETUP
#' old_path <- Sys.getenv("PROJECTS_FOLDER_PATH")
#' setup_projects(path = tempdir(), .Renviron_path = fs::path_temp(".Renviron"))
#' new_affiliation(department_name = "Math Dept.",
#'                 institution_name = "Springfield College",
#'                 address = "123 College St, Springfield, AB")
#' new_affiliation(department_name = "Art Department",
#'                 institution_name = "Springfield College",
#'                 address = "321 University Boulevard, Springfield, AB",
#'                 id = 42)
#' new_affiliation(department_name = "Central Intelligence Agency",
#'                 institution_name = "United States Government",
#'                 address = "888 Classified Dr, Washington DC")
#' new_affiliation(department_name = "Pyrotechnics",
#'                 institution_name = "ACME")
#' new_author(given_names = "Rosetta", last_name = "Stone",
#'            affiliations = c(42, "Math"), degree = "PhD",
#'            email = "slab@rock.net", phone = "867-555-5309", id = 8888)
#' new_author(given_names = "Spiro", last_name = "Agnew", degree = "LLB",
#'            affiliations = "Art D", id = 13)
#' new_author(given_names = "Plato", id = 303)
#' new_project(title = "Test Project 1", authors = c(13, "303", "Stone"),
#'             corresp_auth = "Stone")
#' #############################################################################
#'
#' header(1)
#'
#' #############################################################################
#' # CLEANUP
#' Sys.setenv(PROJECTS_FOLDER_PATH = old_path)
#' fs::file_delete(c(fs::path_temp("projects"), fs::path_temp(".Renviron")))
#' }
#' @name header
#' @importFrom rlang .data
#' @export
header <- function(project, archived = FALSE) {

  p_path  <- p_path_internal()

  projects_tibble <- get_rds(make_rds_path("projects", p_path))

  project <- validate_entry(project,
                            what       = "project",
                            rds_tibble = projects_tibble,
                            max.length = 1,
                            archived   = archived)

  print_header_internal(project_id  = project,
                        p_path      = p_path,
                        project_row = dplyr::filter(projects_tibble,
                                                    .data$id == project))
}

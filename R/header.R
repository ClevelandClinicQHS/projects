#' @importFrom rlang .data
#' @name header
#' @export
#'
#' @title Print project header to console
#' @description This function displays the report header for a project. The project header consists of: 1) the project title; 2) the author list; 3) the list of author affiliations; and 4) corresponding author information. The function is helpful when, after editing details of the project (e.g., any of the above information), you want to update your markdown documents. The displayed markdown can be pasted directly in place of the header within the markdown documents (specifically 04_report.Rmd).
#'
#' @param project Project ID or unambiguous substring of the project name from the projects tibble.
#'
#' @examples
#' \dontrun{
#' header(project = 1)
#' }
header <- function(project) {

  p_path  <- p_path_internal()

  projects_tibble <- get_rds(make_rds_path("projects", p_path))

  project <- validate_entry(project,
                            what       = "project",
                            max.length = 1,
                            rds_tibble = projects_tibble)

  print_header_internal(project_id  = project,
                        p_path      = p_path,
                        project_row = dplyr::filter(projects_tibble,
                                                    .data$id == project))
}


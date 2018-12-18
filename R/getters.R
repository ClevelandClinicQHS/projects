################################################################################
################################################################################

#' Get file path of main projects folder
#'
#' Returns the file path of the main projects folder if it has been established.
#'
#' The file path is returned as a simple character string. It simply returns the
#' value of \code{\link[base]{Sys.getenv}("PRJOECTS_FOLDER_PATH")}, provided
#' that its value is a file path of a directory that actually exists (i.e.,
#' \code{\link{setup_projects}()} has been successfully run).
#'
#' If it can't find a directory with that path, it returns this string:
#'
#' \code{"projects" folder not found. Please run \link{setup_projects}()}
#'
#' @examples
#' projects_folder()
#' @export
projects_folder <- function() {
  p_path_internal(error = FALSE)
}


p_path_internal <- function(error = TRUE) {

  path <- Sys.getenv("PROJECTS_FOLDER_PATH")

  if(fs::dir_exists(path)) {
    return(path)
  }
  else {
    notice <- '"projects" folder not found. Please run setup_projects()'
    if(error) {
      stop(notice)
    }
    else {
      return(notice)
    }
  }
}


################################################################################
################################################################################

#' @rdname display_metadata
#' @importFrom rlang .data
#' @export
affiliations <- function(affiliation, authors = FALSE) {

  p_path              <- p_path_internal()
  affiliations_tibble <-
    "affiliations" %>%
    make_rds_path(p_path) %>%
    get_rds() %>%
    dplyr::arrange(.data$department_name, .data$institution_name)

  if(!missing(affiliation)) {
    affiliation         <-
      validate_entry(affiliation,
                     what        = "affiliation",
                     rds_tibble  = affiliations_tibble,
                     allow_dups = TRUE) %>%
      unique()

    affiliations_tibble <-
      dplyr::filter(affiliations_tibble, .data$id %in% affiliation)
  }

  if(authors) {
    authors_tibble <-
      "authors" %>%
      make_rds_path(p_path) %>%
      get_rds()

    author_affiliation_assoc <-
      "author_affiliation_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds()

    affiliations_tibble <-
      affiliations_tibble %>%
      dplyr::left_join(author_affiliation_assoc, by = c("id" = "id2")) %>%
      dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>%
      dplyr::rename("author_id" = "id1")
  }

  return(dplyr::arrange(affiliations_tibble, .data$id))
}




#' @rdname display_metadata
#' @importFrom rlang .data
#' @export
authors <- function(author, affiliations = FALSE, projects = FALSE) {

  p_path         <- p_path_internal()
  authors_tibble <-
    "authors" %>%
    make_rds_path(p_path) %>%
    get_rds() %>%
    dplyr::arrange(.data$last_name, .data$given_names)

  if(!missing(author)) {
    author         <-
      validate_entry(author,
                     what       = "author",
                     rds_tibble = authors_tibble,
                     allow_dups = TRUE) %>%
      unique()

    authors_tibble <- dplyr::filter(authors_tibble, .data$id %in% author)
  }

  if(affiliations) {
    affiliations_tibble <-
      "affiliations" %>%
      make_rds_path(p_path) %>%
      get_rds()

    author_affiliation_assoc <-
      "author_affiliation_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds()

    authors_tibble <-
      authors_tibble %>%
      dplyr::left_join(author_affiliation_assoc, by = c("id" = "id1")) %>%
      dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>%
      dplyr::rename("affiliation_id" = "id2")
  }

  if(projects) {
    projects_tibble <-
      "projects" %>%
      make_rds_path(p_path) %>%
      get_rds()

    project_author_assoc <-
      "project_author_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds()

    authors_tibble <-
      authors_tibble %>%
      dplyr::left_join(project_author_assoc, by = c("id" = "id2")) %>%
      dplyr::left_join(projects_tibble, by = c("id1" = "id"),
                        suffix = c("_of_author", "_of_project")) %>%
      dplyr::rename("project_id" = "id1")
  }

  return(dplyr::arrange(authors_tibble, .data$id))
}




#' View the \code{projects()}, \code{authors()}, and \code{affiliations()}
#' tables
#'
#' Returns a tibble of the projects/authors/affiliations, filtered and joined
#' according to the entirely optional arguments.
#'
#' If one or more of the \code{projects}, \code{authors}, or \code{affiliations}
#' arguments to set to \code{TRUE}, a \code{\link[dplyr]{left_join}} is
#' performed, with the "left" table being the one sharing the name of the
#' function being used. As such, rows that don't have matches in any other
#' tables will still show up in the output, and rows that have multiple matches
#' in other tables will yield multiple rows in the output. The "right" table's
#' \code{id} column will be renamed.
#'
#' Since all these functions return \code{\link[tibble]{tibble}}s, the user can
#' further manipulate them using \code{\link[dplyr]{dplyr}} functions like
#' \code{\link[dplyr]{select}} and \code{\link[dplyr]{filter}}. See the last
#' example.
#'
#' @param project,author,affiliation An optional (unique) vector of \code{id}s
#'   and/or names. Only rows matching one or more entries will be returned.
#' @param projects,authors,affiliations Logical values indicating whether or not
#'   to perform a left join with another metadata tibble. All \code{FALSE} by
#'   default.
#' @param archived Logical, indicating whether or not to include projects that
#'   have been archived using \code{\link{archive_project}()}. False by default.
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
#' new_author(given_names = "Spiro", last_name = "Agnew", degree = "LLB",
#'            affiliations = "Art D", id = 13)
#' new_author(given_names = "Plato", id = 303)
#' new_author(given_names = "Condoleezza", last_name = "Rice",
#'            affiliations = c(1, 42, "Agency", "ACME"))
#' new_project(title = "Test project 1", current_owner = "Plato")
#' new_project(title = "Test project 2", current_owner = "eezza")
#' new_project(title = "Test project 3", current_owner = "Plato")
#' new_project(title = "Fun project 4",  current_owner = "Rice")
#' new_project(title = "Fun project 5",  current_owner = "Rice")
#' #############################################################################
#'
#' # View entire affiliations table
#' affiliations()
#'
#' # View affiliations table joined to authors table
#' # Notice that multiple rows are created for each affiliation-author combination
#' affiliations(authors = TRUE)
#'
#' # Using dplyr functions to query the tables:
#'
#' # View authors table joined to affiliations table, and filter out duplicate
#' # author ids, leaving one row for each author, each including the author's
#' # primary (i.e., first) affiliation
#' authors(affiliations = TRUE) %>%
#'   dplyr::distinct(id, .keep_all = TRUE) %>%
#'   dplyr::select(id, given_names, last_name, email, phone, address)
#'
#' # View all projects with "Test" in their title
#' projects(project = "Test")
#'
#' # View projects table, including only projects with Plato as current owner
#' projects() %>% dplyr::filter(current_owner == 303)
#'
#' # Wrapped in if(interactive()) because it requires interactive console input
#' # and fails automated testing.
#' if(interactive()) {
#'   # Archive Fun project 5
#'   archive_project("Fun project 5")
#'
#'   # Default behavior is to not include archived projects in projects() table
#'   projects("Fun")
#'   projects("Fun", archived = TRUE)
#' }
#'
#' #############################################################################
#' # CLEANUP
#' Sys.setenv(PROJECTS_FOLDER_PATH = old_path)
#' fs::file_delete(c(fs::path_temp("projects"), fs::path_temp(".Renviron")))
#' }
#' @name display_metadata
#' @importFrom rlang .data
#' @export
projects <- function(project, authors = FALSE, archived = FALSE) {

  p_path          <- p_path_internal()
  projects_tibble <- get_rds(make_rds_path("projects", p_path))

  if(!missing(project)) {

    project         <-
      validate_entry(project,
                     what       = "project",
                     rds_tibble = projects_tibble,
                     allow_dups = TRUE,
                     archived   = archived) %>%
      unique()

    projects_tibble <- dplyr::filter(projects_tibble, .data$id %in% project)
  }
  else if(!archived) {
    projects_tibble <- remove_archived(projects_tibble)
  }

  if(authors) {

    authors_tibble <-
      "authors" %>%
      make_rds_path(p_path) %>%
      get_rds()

    project_author_assoc <-
      "project_author_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds()

    projects_tibble <-
      projects_tibble %>%
      dplyr::left_join(project_author_assoc, by = c("id" = "id1")) %>%
      dplyr::left_join(authors_tibble, by = c("id2" = "id"),
                       suffix = c("_of_project", "_of_author")) %>%
      dplyr::rename("author_id" = "id2")
  }

  return(dplyr::arrange(projects_tibble, .data$id))
}

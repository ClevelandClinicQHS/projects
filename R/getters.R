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
  p_path(error = FALSE)
}


p_path <- function(error = TRUE) {

  path <- Sys.getenv("PROJECTS_FOLDER_PATH")

  if (fs::dir_exists(path)) {
    path
  }
  else {
    notice <- '"projects" folder not found. Please run setup_projects()'
    if (error) {
      stop(notice)
    }
    else {
      notice
    }
  }
}




################################################################################
################################################################################

#' @rdname display_metadata
#' @importFrom rlang .data
#' @export
affiliations <- function(affiliation, authors = FALSE) {

  p_path             <- p_path()

  affiliations_table <-
    affiliations_internal(p_path) %>%
    dplyr::arrange(.data$department_name, .data$institution_name)

  if (!missing(affiliation)) {
    affiliations_table <- affiliations_table %>%
      validate_entry_list(x = affiliation, table = ., what  = "affiliation")
  }

  if (authors) {
    affiliations_table <- affiliations_table %>%
      dplyr::left_join(aa_assoc_internal(p_path), by = c("id" = "id2")) %>%
      dplyr::left_join(authors_internal(p_path), by = c("id1" = "id")) %>%
      dplyr::rename("author_id" = "id1")
  }

  dplyr::arrange(affiliations_table, .data$id)
}



#' @rdname display_metadata
#' @importFrom rlang .data
#' @export
authors <- function(author, affiliations = FALSE, projects = FALSE) {

  p_path        <- p_path()

  authors_table <-
    authors_internal(p_path) %>%
    dplyr::arrange(.data$last_name, .data$given_names)

  if (!missing(author)) {
    authors_table <- authors_table %>%
      validate_entry_list(x = author, table = ., what = "author")
  }

  if (affiliations) {
    authors_table <-
      authors_table %>%
      dplyr::left_join(aa_assoc_internal(p_path), by = c("id" = "id1")) %>%
      dplyr::left_join(affiliations_internal(p_path), by = c("id2" = "id")) %>%
      dplyr::rename("affiliation_id" = "id2")
  }

  if (projects) {
    authors_table <-
      authors_table %>%
      dplyr::left_join(pa_assoc_internal(p_path), by = c("id" = "id2")) %>%
      dplyr::left_join(
        projects_internal(p_path),
        by = c("id1" = "id"),
        suffix = c("_of_author", "_of_project")
      ) %>%
      dplyr::rename("project_id" = "id1")
  }

  dplyr::arrange(authors_table, .data$id)
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
#' # Wrapped in if (interactive()) because it requires interactive console input
#' # and fails automated testing.
#' if (interactive()) {
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
projects <- function(project,
                     all_stages  = FALSE,
                     exclude     = NULL,
                     archived    = all_stages,
                     verbose     = FALSE,
                     authors     = FALSE) {

  p_path          <- p_path()
  projects_path   <- make_rds_path("projects", p_path)
  projects_table  <- get_rds(projects_path)

  if (!is.null(exclude)) {
    exclude <- vapply(exclude, validate_stage, FUN.VALUE = character(1L))
    if (anyDuplicated(exclude)) {
      stop("Duplicate stages detected in \"exclude\" argument.")
    }
  }

  if (!all_stages) {
    exclude <- unique(c(exclude, "0: idea", "6: accepted"))
  }

  if (!archived) {
    projects_table <- remove_archived(projects_table)
  }

  if (!is.null(exclude)) {
    projects_table <- projects_table[!(projects_table$stage %in% exclude), ]
  }

  if (!missing(project)) {
    projects_table <- projects_table %>%
      validate_entry_list(project, table = ., what = "project")
  }

  if (authors) {
    projects_table <- projects_table %>%
      dplyr::left_join(
        pa_assoc_internal(p_path),
        by = c("id" = "id1")
      ) %>%
      dplyr::left_join(
        authors_internal(p_path),
        by     = c("id2" = "id"),
        suffix = c("_of_project", "_of_author")
      ) %>%
      dplyr::rename("author_id" = "id2")
  }

  projects_table <- dplyr::arrange(projects_table, .data$id)

  if (!verbose) {
    projects_table <- projects_table %>%
      dplyr::select(
        -"short_title",
        -"deadline_type",
        -"deadline",
        -"path",
        -"corresp_auth",
        -"creator"
      )
  }

  projects_table
}



#' @export
ideas <- function(project, archived = FALSE, verbose  = FALSE,
                  authors  = FALSE) {
  projects(
    project    = project,
    all_stages = TRUE,
    archived   = archived,
    exclude    = 1L:6L,
    verbose    = verbose,
    authors    = authors
  )
}



#' @export
manuscripts <- function(project, archived = FALSE, verbose = FALSE,
                        authors = FALSE) {
  projects(
    project    = project,
    all_stages = TRUE,
    archived   = archived,
    exclude    = c(0L:3L, 6L),
    verbose    = verbose,
    authors    = authors
  )
}

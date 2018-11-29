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
#'
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
#'   have been archived using \code{\link{archive_project}()}. They are not
#'   displayed by default.
#'
#' @examples
#' \dontrun{
#' projects("Cholesterol") %>% dplyr::filter(current_owner == 10)
#'
#' authors(affiliations = TRUE) %>%
#'   dplyr::distinct(id, .keep_all = TRUE) %>%
#'   dplyr::select(id, given_names, last_name, email, phone, address)
#'
#' affiliations()
#' }
#'
#' @name display_metadata
#' @importFrom rlang .data
#' @export
projects <- function(project, authors = FALSE, archived = FALSE) {

  p_path          <- p_path_internal()
  projects_tibble <-
    "projects" %>%
    make_rds_path(p_path) %>%
    get_rds()

  if(!missing(project)) {

    project         <-
      validate_entry(project,
                     what       = "project",
                     rds_tibble = projects_tibble,
                     allow_dups = TRUE) %>%
      unique()

    projects_tibble <- dplyr::filter(projects_tibble, .data$id %in% project)
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

  if(!archived) {
    projects_tibble <-
      dplyr::filter(projects_tibble,
                    fs::path_file(fs::path_dir(.data$path)) != "archive")
  }

  return(dplyr::arrange(projects_tibble, .data$id))
}

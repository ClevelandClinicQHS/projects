################################################################################
################################################################################
# This is a user-friendly wrapper for p_path_internal() below it
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
    notice <- '"projects" folder not found. Please run initialize()'
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
# This is a user-friendly wrapper for get_default_author_internal() below it
#' @export
default_author <- function() {
  get_default_author_internal(get_rds(make_rds_path("authors")))
}



#' @importFrom rlang .data
get_default_author_internal <- function(authors_tibble) {

  default_author <- dplyr::filter(authors_tibble, .data$default)$id

  if(length(default_author) == 0) {
    message("Default author not set. Add one with new_author() or set an ",
            "existing author as default with edit_author(), setting default = ",
            "TRUE")
    default_author <- NA_integer_
  }

  return(default_author)
}

################################################################################
################################################################################

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
    affiliation         <- validate_entry(affiliation,
                                          what        = "affiliation",
                                          rds_tibble  = affiliations_tibble)
    affiliations_tibble <-
      dplyr::filter(affiliations_tibble, .data$id %in% affiliation)
  }

  if(authors) {
    authors_tibble <-
      "authors" %>%
      make_rds_path(p_path) %>%
      get_rds() #%>%
      #dplyr::select("id", "last_name", "given_names")

    author_affiliation_assoc <-
      "author_affiliation_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds() #%>%
      #dplyr::filter(.data$id2 %in% affiliations_tibble$id) %>%
      #dplyr::arrange(.data$id2, .data$id1)

    affiliations_tibble <-
      affiliations_tibble %>%
      dplyr::left_join(author_affiliation_assoc, by = c("id" = "id2")) %>%
      dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>%
      dplyr::rename("author_id" = "id1")

        # dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>%
        # dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>%
        # dplyr::rename(id = id2, author_id = id1) %>%
        # dplyr::full_join(affiliations_tibble) %>%
        # dplyr::select(id,        department_name:address,
        #               author_id, last_name:given_names) %>%
        # suppressMessages())
  }

  return(affiliations_tibble)
}




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
    author         <- validate_entry(author,
                                     what       = "author",
                                     rds_tibble = authors_tibble)

    authors_tibble <- dplyr::filter(authors_tibble, .data$id %in% author)
  }

  if(affiliations) {
    affiliations_tibble <-
      "affiliations" %>%
      make_rds_path(p_path) %>%
      get_rds() #%>%
      #dplyr::select("id", "department_name", "institution_name")

    author_affiliation_assoc <-
      "author_affiliation_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds() #%>%
      # dplyr::filter(.data$id1 %in% authors_tibble$id) %>%
      # dplyr::arrange(.data$id1, .data$id2)

    authors_tibble <-
      authors_tibble %>%
      dplyr::left_join(author_affiliation_assoc, by = c("id" = "id1")) %>%
      dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>%
      dplyr::rename("affiliation_id" = "id2")

      # author_affiliation_assoc %>%
      # dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>%
      # dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>%
      # dplyr::rename(id = id1, affiliation_id = id2) %>%
      # dplyr::full_join(authors_tibble) %>%
      # dplyr::select(id,             last_name:email,
      #               affiliation_id, department_name:institution_name) %>%
      # suppressMessages
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

      # project_author_assoc %>%
      # dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>%
      # dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>%
      # dplyr::rename(id = id1, affiliation_id = id2) %>%
      # dplyr::full_join(authors_tibble) %>%
      # dplyr::select(id,             last_name:email,
      #               affiliation_id, department_name:institution_name) %>%
      # suppressMessages
  }

  return(authors_tibble)
}




#' View the tibble of all projects.
#'
#' Returns a tibble of all projects in the main projects folder, with the option
#' to filter by project and/or display the projects' authors as well.
#'
#' @param project An optional vector of project
#'   \code{titles}/\code{short_titles}/\code{ids}. If each element matches a
#'   project in the database, only those projects will be returned.
#' @param authors Logical indicating whether or not projects' authors will be
#'   included in the resulting tibble. If \code{TRUE}, a left join is performed,
#'   so that projects with multiple authors will have multiple rows in the
#'   resulting tibble.
#'
#' @aliases
#'
#' @importFrom rlang .data
#' @export
projects <- function(project, authors = FALSE) {

  p_path          <- p_path_internal()
  projects_tibble <-
    "projects" %>%
    make_rds_path(p_path) %>%
    get_rds() %>%
    dplyr::arrange(.data$id)

  if(!missing(project)) {

    project         <- validate_entry(project,
                                      what       = "project",
                                      rds_tibble = projects_tibble)

    projects_tibble <- dplyr::filter(projects_tibble, .data$id %in% project)
  }

  if(authors) {

    authors_tibble <-
      "authors" %>%
      make_rds_path(p_path) %>%
      get_rds() #%>%
    #dplyr::select("id", "last_name", "given_names")

    project_author_assoc <-
      "project_author_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds() #%>%
    # dplyr::filter(.data$id1 %in% projects_tibble$id) %>%
    # dplyr::arrange(.data$id1, .data$id2)

    projects_tibble <-
      projects_tibble %>%
      dplyr::left_join(project_author_assoc, by = c("id" = "id1")) %>%
      dplyr::left_join(authors_tibble, by = c("id2" = "id"),
                       suffix = c("_of_project", "_of_author")) %>%
      dplyr::rename("author_id" = "id2")

    # project_author_assoc %>%
    # dplyr::left_join(projects_tibble, by = c("id1" = "id")) %>%
    # dplyr::mutate(tmp = TRUE) %>%
    # dplyr::left_join(authors_tibble, by = c("id2" = "id")) %>%
    # dplyr::rename(id                   = id1,
    #               investig_id          = id2,
    #               investig_last_name   = last_name,
    #               investig_given_names = given_names) %>%
    # dplyr::full_join(projects_tibble) %>%
    # dplyr::select(.data$id, .data$title:.data$tmp, .data$investig_id,
    #               .data$investig_last_name:.data$investig_given_names) %>%
    # dplyr::select(-.data$tmp) %>%
    # dplyr::arrange(.data$id) %>%
    # suppressMessages
  }

  return(projects_tibble)
}

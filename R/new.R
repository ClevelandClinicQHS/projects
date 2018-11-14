#' Create a new project
#'
#' \code{new_project()} creates a new R project folder that is automatically
#' filled with a .Rproj file, helpful subdirectories, and .Rmd files to get your
#' project workflow started.
#'
#' The folder and .Rproj file both have names of the form "pXXXX", where "XXXX"
#' denotes the project \code{id} number. The folder will be an immediate
#' subdirectory of the main projects folder (see \code{\link{setup_projects()}})
#' unless the argument \code{path} specifies a deeper subdirectory. The user may
#' enter various metadata about the project that is stored and may be called
#' forth using the \code{\link{projects()}} function. Some of this metadata will
#' automatically be added to a \code{\link{header}} atop the automatically
#' created .Rmd files called progs/01_protocol.Rmd and progs/04_report.Rmd.
#'
#' @param title Title of project. Coerced to title case using
#'   \code{tools::toTitleCase()}.
#' @param short_title A nickname for the project that can be used to more easily
#'   select the project when other functions are called (e.g.,
#'   \code{\link{projects()}} or \code{\link{edit_project()}}).
#' @param authors A vector of author
#'   \code{last_names}/\code{given_names}/\code{ids}. Order will be preserved.
#'   Each element must match a row in the \code{\link{authors()}} table.
#' @param current_owner A \code{last_names}/\code{given_names}/\code{ids} of one
#'   of the authors in the \code{\link{authors()}} table.
#' @param creator A \code{last_names}/\code{given_names}/\code{ids} of one of
#'   the authors in the \code{\link{authors()}} table. If left blank (i.g.,
#'   \code{NA}), it will be filled with the \code{id} of the \code{default}
#'   author.
#' @param corresp_auth A \code{last_name}/\code{given_names}/\code{id} of one of
#'   the authors in the \code{\link{authors()}} table. If not left blank, (i.e.,
#'   \code{NA}), this author's information
#' @param stage
#' @param deadline_type
#' @param deadline
#' @param id
#' @param status
#' @param path
#' @param protocol
#' @param make_directories
#' @param use_bib
#'
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
new_project <- function(title    = NA,                     short_title   = NA,
                        authors,                           current_owner = NA,
                        creator  = NA,                     corresp_auth  = NA,
                        stage    = NA,                     deadline_type = NA,
                        deadline = NA,                     id            = NA,
                        status   = "just created",         path          = "",
                        protocol          = c("STROBE", "CONSORT"),
                        make_directories  = FALSE,
                        use_bib           = FALSE,
                        datawork_template = NULL,
                        analysis_template = NULL) {

  p_path                   <- p_path_internal()

  projects_path            <- make_rds_path("projects", p_path)
  projects_tibble          <- get_rds(projects_path)

  authors_tibble           <- get_rds(make_rds_path("authors", p_path))

  pa_assoc_path            <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_tibble          <- get_rds(pa_assoc_path)

  affiliations_tibble      <- get_rds(make_rds_path("affiliations", p_path))

  aa_assoc_tibble <- get_rds(make_rds_path("author_affiliation_assoc", p_path))

  #######################
  # Validation of id, path
  id              <- validate_new(id         = id,
                                  what       = "project",
                                  rds_tibble = projects_tibble)

  path            <- validate_directory(path             = path,
                                        p_path           = p_path,
                                        make_directories = make_directories)

  pXXXX_name      <- make_project_name(id)
  pXXXX_path      <- make_project_path(pXXXX_name, path)
  ########################
  ########################

  title <- tools::toTitleCase(title)

  ########################
  # Validation of authors, creator, corresp_auth, current_owner

  if(nrow(authors_tibble) == 0 && !all(is.na(creator), is.na(corresp_auth),
                                       is.na(current_owner), missing(authors))){
    stop("Can't set authors, creator, corresp_auth, or current owner until an",
         " author is created. Run new_author()")
  }

  if(!is.na(corresp_auth)) {
    corresp_auth <- validate_entry(corresp_auth,
                                   what        = "author",
                                   max.length = 1,
                                   rds_tibble = authors_tibble)
  }

  # If creator is left blank, the default author is used. If the default author
  # is not set, it will be NA.
  if(is.na(creator)) {
    creator <- get_default_author_internal(authors_tibble = authors_tibble)
  }
  else {
    creator <- validate_entry(creator,
                              what       = "author",
                              max.length = 1,
                              rds_tibble = authors_tibble)
  }

  if(!is.na(current_owner)) {
    current_owner <- validate_entry(current_owner,
                                    what       = "author",
                                    max.length = 1,
                                    rds_tibble = authors_tibble)
  }

  if(missing(authors)) {
    if(is.na(current_owner)) {
      if(!is.na(creator)) {
        authors <- current_owner <- creator
      }
    }
    else {
      authors <- current_owner
    }
  }
  else {
    authors <- validate_entry(authors,
                              what       = "author",
                              rds_tibble = authors_tibble,
                              max.length = 9999)
    if(is.na(current_owner)) {
      current_owner <- authors[1]
    }
    else if(!(current_owner %in% authors)) {
      authors <- c(authors[1],
                   authors[-c(1, length(authors))],
                   current_owner,
                   authors[-1][length(authors) - 1])
    }
  }
  ############################
  ############################

  protocol_report <-
    build_protocol_report(protocol            = protocol,
                          p_path              = p_path,
                          project_id          = id,
                          title               = title,
                          corresp_auth        = corresp_auth,
                          authors_tibble      = authors_tibble,
                          affiliations_tibble = affiliations_tibble,
                          project_authors     = authors,
                          aa_assoc_tibble     = aa_assoc_tibble,
                          use_bib             = use_bib,
                          pXXXX_name          = pXXXX_name)

  datawork <- build_datawork_analysis(template   = datawork_template,
                                      what       = "datawork",
                                      p_path     = p_path,
                                      pXXXX_name = pXXXX_name)

  analysis <- build_datawork_analysis(template   = datawork_template,
                                      what       = "analysis",
                                      p_path     = p_path,
                                      pXXXX_name = pXXXX_name)

  # Add new row to project list
  new_project_row <- change_table(action        = "new",
                                  rds_path      = projects_path,
                                  rds_tibble    = projects_tibble,
                                  id            = id,
                                  title         = title,
                                  short_title   = as.character(short_title),
                                  current_owner = as.integer(current_owner),
                                  creator       = as.integer(creator),
                                  corresp_auth  = as.integer(corresp_auth),
                                  stage         = as.character(stage),
                                  deadline_type = as.character(deadline_type),
                                  deadline      = as.Date(deadline),
                                  status        = as.character(status),
                                  path          = pXXXX_path)

  # Add row(s) to project-author association table
  if(!missing(authors)) {
    pa_assoc_tibble <-
      change_assoc(assoc_path   = pa_assoc_path,
                   assoc_tibble = pa_assoc_tibble,
                   new          = TRUE,
                   id1          = id,
                   id2          = authors)
  }

  # Write the files
  write_project_files(pXXXX_path      = pXXXX_path,
                      protocol_report = protocol_report,
                      datawork        = datawork,
                      analysis        = analysis,
                      use_bib         = use_bib,
                      pXXXX_name      = pXXXX_name)



  message("\nProject ", id, " has been created at ", pXXXX_path)
  print(dplyr::select(new_project_row,
                      -c("current_owner", "creator", "corresp_auth")))

  message("\nNew project's authors:")
  if(missing(authors)) {
    print("None.")
  }
  else {
    print(pa_assoc_tibble %>%
            dplyr::filter(.data$id1 == id) %>%
            dplyr::left_join(authors_tibble,
                             by = c("id2" = "id")) %>%
            dplyr::select(-.data$id1) %>%
            dplyr::rename(author_id = id2))
  }

  message("\nCurrent owner:")
  if(is.na(current_owner)) {
    print("None.")
  }
  else {
    print(dplyr::filter(authors_tibble, .data$id == current_owner))
  }

  message("\nCreator:")
  if(is.na(creator)) {
    print("None.")
  }
  else {
    print(dplyr::filter(authors_tibble, .data$id == creator))
  }

  message("\nCorresponding author:")
  if(is.na(corresp_auth)) {
    print("None.")
  }
  else {
    print(dplyr::filter(authors_tibble, .data$id == corresp_auth))
  }
}
################################################################################



################################################################################
#' @importFrom rlang .data
#' @export
new_author <- function(given_names = NA,    last_name    = NA,
                       title       = NA,    affiliations,
                       degree      = NA,    email        = NA,
                       phone       = NA,    id           = NA,
                       default     = FALSE) {

  p_path         <- p_path_internal()

  authors_path   <- make_rds_path("authors", p_path)
  authors_tibble <- get_rds(authors_path)

  id             <- validate_new(id         = id,
                                 what       = "author",
                                 rds_tibble = authors_tibble)

  if(!missing(affiliations)) {
    aa_assoc_path       <- make_rds_path("author_affiliation_assoc", p_path)
    aa_assoc_tibble     <- get_rds(aa_assoc_path)
    affiliations_tibble <- get_rds(make_rds_path("affiliations", p_path))

    if(nrow(affiliations_tibble) == 0) {
      stop("Can't set affiliations until an affiliation is created. ",
           "Run new_affiliation()")
    }

    affiliations        <- validate_entry(affiliations,
                                          what        = "affiliation",
                                          rds_tibble  = affiliations_tibble)
  }

  #######################
  # Handling of default
  if(!is.logical(default)) {
    stop("The argument 'default' must be either TRUE or FALSE")
  }

  if(nrow(authors_tibble) == 0 && !default) {
    default <-
      user_prompt(msg   = paste0("This is the first author. Do you want to ",
                                 "make this author the default author? (y/n)"),
                  error = FALSE)
  }
  ######################
  ######################

  new_author_row <- change_table(action      = "new",
                                 rds_path    = authors_path,
                                 rds_tibble  = authors_tibble,
                                 id          = id,
                                 given_names = given_names,
                                 last_name   = last_name,
                                 title       = title,
                                 degree      = degree,
                                 email       = tolower(email),
                                 phone       = phone,
                                 default     = default)

  message("New author:")
  print(new_author_row)

  message("\nNew author's affiliations:")
  if(!missing(affiliations)) {
    new_author_affiliations <-
      change_assoc(assoc_path   = aa_assoc_path,
                   assoc_tibble = aa_assoc_tibble,
                   new          = TRUE,
                   id1          = id,
                   id2          = affiliations) %>%
      dplyr::filter(.data$id1 == id)

    #message("\nNew author's affiliations:")
    print(new_author_affiliations %>%
            dplyr::filter(.data$id1 == id) %>%
            dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>%
            dplyr::select(-"id1") %>%
            dplyr::rename("affiliation_id" = "id2"))
  }
  else {
    print("None.")
  }
}
################################################################################


################################################################################
#' @export
new_affiliation <- function(department_name  = NA, institution_name = NA,
                            address          = NA, id               = NA) {

  affiliations_path   <- make_rds_path("affiliations")
  affiliations_tibble <- get_rds(affiliations_path)

  id                  <- validate_new(id         = id,
                                      what       = "affiliations",
                                      rds_tibble = affiliations_tibble)

  message("New affiliation:")
  print(change_table(action           = "new",
                     rds_path         = affiliations_path,
                     rds_tibble       = affiliations_tibble,
                     id               = id,
                     department_name  = department_name,
                     institution_name = institution_name,
                     address          = address))
}
################################################################################

#' @name new_edit_delete
#'
#' @title Create or edit projects, authors, and affiliations
#'
#' These functions create or edit rows in the \code{\link{projects}},
#' \code{\link{authors}}, and \code{\link{affiliations}} tibbles, which are
#' stored in the \emph{.metadata} subdirectory of the main
#' \code{\link{projects_folder}}.
#'
#' \code{new_project()} creates a new R project folder that is automatically
#' filled with a .Rproj file, helpful subdirectories, and .Rmd files to get your
#' project workflow started. The \code{edit_()} functions and the other
#' \code{new_()} functions only create or edit rows in the \emph{.metadata}
#' tibbles.
#'
#' Newly created project folders (and the .Rproj files they contain) both have
#' names of the form "p\emph{XXXX}", where "\emph{XXXX}" denotes the project
#' \code{id} number. The folder will be an immediate subdirectory of the main
#' projects folder (see \code{\link{setup_projects()}}) unless the argument
#' \code{path} specifies a deeper subdirectory. The user may enter various
#' metadata about the project that is stored and may be called forth using the
#' \code{\link{projects()}} function. Some of this metadata will automatically
#' be added to the \code{\link{header}} atop the automatically created
#' \emph{.Rmd} files called \emph{progs/01_protocol.Rmd} and
#' \emph{progs/04_report.Rmd}.
#'
#' @param id An integer that will become the item's permanent identification
#'   number. Must be in the range 1-9999 or left blank. If left blank, the
#'   lowest available integer in the aforemntioned range will be selected.
#'
#'   For \code{new_project}, this number will also determine the project
#'   folder's and \emph{.Rproj} file's names, which are of the form
#'   "p\emph{XXXX}". If the \code{id} number is not four digits long, it will be
#'   padded on the left side with 0s.
#' @param title For the \code{_project()} functions, the title of the project;
#'   for \code{new_project()} only, the user input is coerced to title case
#'   using \code{tools::toTitleCase()}.
#'
#'   For the \code{_author()} functions, the job title of the author.
#' @param short_title A nickname for the project. Can be used in other
#'   \code{projects} package functions whenever needing to specify a project.
#' @param given_names,last_name,department_name,institution_name Each a single
#'   character string. Can be used whenever needing to specify a specific
#'   author/affiliation.
#' @param degree A character string (preferably an abbreviation) denoting the
#'   author's academic degree(s). Will be written next to author names in the
#'   \code{\link{header}}.
#' @param email,phone A charcter string denoting the email/phone of the author.
#'   Email will be coerced to lowercase. When a project is given a
#'   \code{corresp_auth}, these items will be included in the
#'   \code{\link{header}}.
#' @param address A character string indicating the address of the affiliation.
#' @param author,affiliation The \code{id} or unambiguous
#'   \code{given_names}/\code{last_name} or
#'   \code{department_name}/\code{institution_name} of a specific
#'   author/affiliation to edit.
#' @param authors,affiliations For \code{new_project()}/\code{new_author()}, a
#'   vector of \code{id}s or unambiguous \code{given_names}/\code{last_name} or
#'   \code{department_name}/\code{institution_name} of
#'   \code{\link{authors}}/\code{\link{affiliations}}. Order will be preserved.
#'
#'   For \code{edit_project()}/\code{edit_author()}, a formula specifying
#'   \code{\link{authors}}/\code{\link{affiliations}} to add or remove from the
#'   project/author. Formulas must have no lefthand side (i.e., begin with
#'   \code{~}) and use \code{+} to add authors and \code{-} to remove authors.
#'   Authors may be specified by \code{id} or name.
#'
#'   Each element must match a row in the \code{\link{authors}} tibble.
#' @param creator,default \code{creator} is an \code{id} or unambiguous
#'   \code{last_name}/\code{given_names} of one of the authors in the
#'   \code{\link{authors}} table. If left blank (e.g., \code{NA}), it will be
#'   filled with the \code{id} of the \code{default} author.
#'
#'   \code{default}, when \code{TRUE}, flags the default author. Setting an
#'   author with \code{default = TRUE} will automatically set all other authors
#'   to \code{default = FALSE}.
#' @param corresp_auth,current_owner An \code{id} or unambiguous
#'   \code{last_name}/\code{given_names} of one of the authors in the
#'   \code{\link{authors}} table.
#'
#'   If \code{corresp_auth} is specified, all of this author's contact
#'   information will be especially included in the project's
#'   \code{\link{header}}.
#' @param stage A factor with the levels \code{c("design", "data collection",
#'   "analysis", "manuscript", "under review", "accepted")}, communicating the
#'   stage the project is in.
#' @param deadline_type A free text field, intended to communicate the meaning
#'   of the next field, \code{deadline}.
#' @param deadline A \code{Date} or a character string that can be coerced to a
#'   \code{Date}.
#' @param status A free text field, intended to communicate the most current
#'   condition the project is in.
#'
#'   For \code{new_project()}, default is \code{"just created"}.
#' @param path A character string that can be read as a file path. It may be a
#'   relative path to be added to the end of the main
#'   \code{\link{project_folder}}, or it may contain the
#'   \code{\link{project_folder}}'s path as its parent path. Either way, the
#'   result is that the new project folder will be a subdirectory of the main
#'   \code{\link{project_folder}}. See also \code{\link{setup_projects()}}.
#' @param protocol A character string matching one of the included
#'   protocol/report templates (viz., one of \code{c("STROBE", "CONSORT")}), or
#'   the file name of a custom template in the .templates subdirectory within
#'   the main \code{\link{projects_folder}}. If using a custom template, make
#'   sure to match the case and file extension exactly.
#' @param make_directories Logical, indicating whether or \code{new_project()}
#'   should create subdirectories specified in the \code{path} argument that do
#'   not already exist. Ignored if \code{path} is left blank or if all
#'   directories in \code{path} already exist.
#' @param use_bib Logical. If \code{TRUE}, a blank \emph{.bib} file will be
#'   written into the \strong{progs} subdirectory of the newly created project
#'   folder. Its name will be of the form "\emph{pXXXX.bib}", and the YAML
#'   header of \emph{progs/01_protocol.Rmd} and \emph{progs/04_report.Rmd} will
#'   include the line "bibliography: pXXXX.bib".
#' @param datawork_template,analysis_template Optional file names of custom
#'   templates to be used to create \emph{progs/02_datawork.Rmd} and/or
#'   \emph{progs/03_analysis.Rmd}. Make sure to match the case and file
#'   extension exactly.
#'
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
new_project <- function(title             = NA,
                        short_title       = NA,
                        authors,
                        current_owner     = NA,
                        creator           = NA,
                        corresp_auth      = NA,
                        stage             = c("1: design", "2: data collection",
                                              "3: analysis", "4: manuscript",
                                              "5: under review", "6: accepted"),
                        deadline_type     = NA,
                        deadline          = NA,
                        id                = NA,
                        status            = "just created",
                        path              = "",
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

  if(!is.na(title)) {
    title <- tools::toTitleCase(title)
  }

  stage <- factor(match.arg(stage), levels = eval(formals()$stage))

  ########################
  # Validation of authors, creator, corresp_auth, current_owner

  if(nrow(authors_tibble) == 0 && !all(is.na(creator), is.na(corresp_auth),
                                       is.na(current_owner), missing(authors))){
    stop("Can't set authors, creator, corresp_auth, or current owner until an",
         " author is created. Run new_author()")
  }

  if(!is.na(corresp_auth)) {
    corresp_auth <- validate_entry(corresp_auth,
                                   what       = "author",
                                   rds_tibble = authors_tibble,
                                   max.length = 1)
  }

  # If creator is left blank, the default author is used. If the default author
  # is not set, it will be NA.
  if(is.na(creator)) {
    creator <- get_default_author_internal(authors_tibble = authors_tibble)
  }
  else {
    creator <- validate_entry(creator,
                              what       = "author",
                              rds_tibble = authors_tibble,
                              max.length = 1)
  }

  if(!is.na(current_owner)) {
    current_owner <- validate_entry(current_owner,
                                    what       = "author",
                                    rds_tibble = authors_tibble,
                                    max.length = 1)
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
                              rds_tibble = authors_tibble)
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
                                  stage         = stage,
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
#' @rdname new_edit_delete
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
#' @rdname new_edit_delete
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

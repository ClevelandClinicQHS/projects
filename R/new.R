#' Create, edit or delete projects, authors and affiliations
#'
#' These functions create, edit, or delete rows in the \code{\link{projects}()},
#' \code{\link{authors}()}, and \code{\link{affiliations}()} tibbles, which are
#' stored in the \emph{.metadata} subdirectory of the main
#' \code{\link{projects_folder}}.
#'
#' \code{new_project()} creates a new R project folder that is automatically
#' filled with a \emph{.Rproj} file, helpful subdirectories, and \emph{.Rmd}
#' files to get your project workflow started; \code{delete_project()} deletes
#' them. The \code{edit_\*()} functions and the other \code{new_\*()} and
#' \code{delete_\*()} functions only create or edit rows in the \emph{.metadata}
#' tibbles.
#'
#' Newly created project folders (and the \emph{.Rproj} files they contain) both
#' have names of the form "p\emph{XXXX}", where "\emph{XXXX}" denotes the
#' project \code{id} number. The folder will be an immediate subdirectory of the
#' main \code{\link{projects_folder}} (see also \code{\link{setup_projects}()})
#' unless the argument \code{path} specifies a deeper subdirectory. The user may
#' enter various metadata about the project that is stored and may be called
#' forth using the \code{\link{projects}()} function. Some of this metadata will
#' automatically be added to the \code{\link{header}} atop the automatically
#' created \emph{.Rmd} files called \emph{progs/01_protocol.Rmd} and
#' \emph{progs/04_report.Rmd}.
#'
#' @param id An integer that will become the item's permanent identification
#'   number. Must be in the range 1-9999 or left blank. If left blank, the
#'   lowest available integer in the aforementioned range will be selected.
#'
#'   For \code{new_project}, this number will also determine the project
#'   folder's and \emph{.Rproj} file's names, which are of the form
#'   "p\emph{XXXX}". If the \code{id} number is not four digits long, it will be
#'   padded on the left side with 0s.
#' @param title For the \code{\*_project()} functions, the title of the project;
#'   for \code{new_project()} only, the user input is coerced to title case
#'   using \code{tools::toTitleCase()}.
#'
#'   For the \code{\*_author()} functions, the job title of the author.
#' @param short_title A nickname for the project. Can be used in other
#'   \code{projects} package functions whenever needing to specify a project.
#' @param given_names,last_name,department_name,institution_name Each a single
#'   character string. Can be used whenever needing to specify a specific
#'   author/affiliation.
#' @param degree A character string (preferably an abbreviation) denoting the
#'   author's academic degree(s). Will be written next to author names in the
#'   \code{\link{header}}.
#' @param email,phone A character string denoting the email/phone of the author.
#'   Email will be coerced to lowercase. When a project is given a
#'   \code{corresp_auth}, these items will be included in the
#'   \code{\link{header}}.
#' @param address A character string indicating the address of the affiliation.
#' @param project,author,affiliation The \code{id} or unambiguous names of a
#'   project/author/affiliation to edit or delete.
#' @param authors,affiliations For \code{new_project()}/\code{new_author()}, a
#'   vector of \code{id}s or unambiguous \code{given_names}/\code{last_name} or
#'   \code{department_name}/\code{institution_name} of
#'   \code{\link{authors}}/\code{\link{affiliations}}. Order will be preserved.
#'
#'   For \code{edit_project()}/\code{edit_author()}, a formula specifying
#'   \code{\link{authors}}/\code{\link{affiliations}} to add or remove from the
#'   project/author. Formulas must have no left-hand side (i.e., begin with
#'   \code{~}) and use \code{+} to add authors and \code{-} to remove authors.
#'   Authors may be specified by \code{id} or name.
#'
#'   Each element must match a row in the \code{\link{authors}} tibble.
#' @param corresp_auth,current_owner An \code{id} or unambiguous
#'   \code{last_name}/\code{given_names} of one of the authors in the
#'   \code{\link{authors}} table.
#'
#'   If \code{corresp_auth} is specified, all of this author's contact
#'   information will be especially included in the project's
#'   \code{\link{header}}.
#' @param creator The author who created the project. If it is equal to
#'   \code{Sys.info()["user"]} (the default value), it is kept as is. Otherwise
#'   it will be validated against the \code{authors()} tibble and populated with
#'   the matching author \code{id}.
#' @param status A free text field, intended to communicate the most current
#'   condition the project is in.
#'
#'   For \code{new_project()}, default is \code{"just created"}.
#' @param deadline_type A free text field, intended to communicate the meaning
#'   of the next field, \code{deadline}.
#' @param deadline A \code{Date} or a character string that can be coerced to a
#'   \code{Date}.
#' @param path A character string that can be read as a file path. Can be
#'   either:
#'
#'   1) the \emph{absolute} path of the \code{\link{projects_folder}} (default)
#'
#'   2) an \emph{absolute} path pointing to a subfolder within the
#'   \code{\link{projects_folder}}
#'
#'   3) a \emph{relative} path (leading \code{"."} optional) that will be
#'   appended onto the end of the \code{\link{projects_folder}}.
#'
#'   In any case, the result is that the new project folder will be a
#'   subdirectory of the main \code{\link{projects_folder}}. See also
#'   \code{\link{setup_projects}()}.
#' @param make_directories Logical, indicating whether or not
#'   \code{new_project()} should create subdirectories specified in the
#'   \code{path} argument that do not already exist. Ignored if \code{path} is
#'   left as the default or if all directories in \code{path} already exist.
#' @param stage A factor with the levels \code{c("1: design", "2: data
#'   collection", "3: analysis", "4: manuscript", "5: under review", "5:
#'   accepted")}, communicating the stage the project is in.
#' @param protocol,datawork,analysis,report,css,Rproj A character string
#'   matching the name of a corresponding template file in the \emph{.templates}
#'   subdirectory of the main \code{\link{projects_folder}}. Default templates
#'   are placed there when \code{\link{setup_projects}()} is run, and the user
#'   can edit these if desired.
#'
#'   Multiple default \code{protocol} templates are available.
#'   \emph{01_protocol.Rmd}, which by default is the same as
#'   \emph{STROBE_protocol.Rmd}, will be selected if \code{protocol} is
#'   unspecified. Users can edit these default templates.
#'
#'   If using a custom template, make sure to match the case and file extension
#'   exactly.
#' @param use_bib Logical. If \code{TRUE}, a blank \emph{.bib} file will be
#'   written into the \strong{progs} subdirectory of the newly created project
#'   folder. Its name will be of the form \emph{pXXXX.bib}, and the YAML header
#'   of \emph{progs/01_protocol.Rmd} and \emph{progs/04_report.Rmd} will include
#'   the line \code{bibliography: pXXXX.bib}.
#' @param reprint_header Logical, indicating whether or not to reprint the
#'   project \code{\link{header}} after editing project information.
#' @param archived Logical indicating whether or not the function should
#'   consider archived projects when determining which project the user is
#'   referring to in the \code{project} argument. \code{FALSE} by default.
#'
#'   See the \strong{Details} section of \code{\link{archive_project}()} for
#'   more information on the "archived" status of a project.
#'
#' @examples
#' # SETUP
#' old_path <- Sys.getenv("PROJECTS_FOLDER_PATH")
#' setup_projects(path = tempdir(), .Renviron_path = fs::path_temp(".Renviron"))
#' ############################################################################
#'
#' # Creating affiliations
#' new_affiliation(department_name = "Math Dept.",
#'                 institution_name = "Springfield College",
#'                 address = "123 College St, Springfield, AB")
#' new_affiliation(department_name = "Art Department",
#'                 institution_name = "Springfield College",
#'                 address = "321 University Boulevard, Springfield, AB",
#'                 id = 42)
#'
#' # Editing an affiliation
#' edit_affiliation("Math Dept", department_name = "Mathematics Department")
#'
#' # Creating authors
#' new_author(given_names = "Rosetta", last_name = "Stone",
#'            affiliations = c(42, "Math"), degree = "PhD",
#'            email = "slab@rock.net", phone = "867-555-5309", id = 8888)
#' new_author(given_names = "Spiro", last_name = "Agnew", degree = "LLB",
#'            affiliations = "Art D", id = 13)
#' new_author(last_name = "Plato", id = 303)
#'
#' # Editing an author, showcasing the removal of a text element (last_name)
#' edit_author(author = 303, given_names = "Plato", last_name = NULL)
#'
#' # Editing an author, showcasing the addition and removal of affiliations
#' edit_author("Spiro", affiliations = ~ -"Art D" + Math)
#'
#' # Creating a project
#' new_project(title = "Understanding the Construction of the United States",
#'             short_title = "USA", authors = c(13, "Stone"),
#'             stage = 4, deadline = "2055-02-28", deadline_type = "submission",
#'             path = "famous_studied/philosophers/rocks",
#'             corresp_auth = "Stone", current_owner = "agnew",
#'             make_directories = TRUE, use_bib = TRUE,
#'             status = "waiting on IRB")
#'
#' # Editing a project, showcasing the addition and removal of authors
#' edit_project("Understanding", short_title = "usa1",
#'              authors = ~ + "303" - 13 - Stone)
#'
#' # Wrapped in if(interactive()) because it requires interactive console input
#' # and fails automated package checking and testing.
#' if(interactive()) {
#'   delete_project("usa1")
#'   delete_author(303)
#'   delete_affiliation("Math")
#' }
#'
#' #############################################################################
#' # CLEANUP
#' Sys.setenv(PROJECTS_FOLDER_PATH = old_path)
#' fs::file_delete(c(fs::path_temp("projects"), fs::path_temp(".Renviron")))
#' @name new_edit_delete
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
new_project <- function(title            = NA,
                        short_title      = NA,
                        authors          = NULL,
                        current_owner    = NA,
                        status           = "just created",
                        deadline_type    = NA,
                        deadline         = NA,
                        stage            = c("1: design", "2: data collection",
                                             "3: analysis", "4: manuscript",
                                             "5: under review", "6: accepted"),
                        path             = projects_folder(),
                        make_directories = FALSE,
                        corresp_auth     = NA,
                        creator          = Sys.info()["user"],
                        id               = NA,
                        protocol         = c("01_protocol.Rmd",
                                             "STROBE_protocol.Rmd",
                                             "CONSORT_protocol.Rmd"),
                        datawork         = "02_datawork.Rmd",
                        analysis         = "03_analysis.Rmd",
                        report           = "04_report.Rmd",
                        css              = "style.css",
                        Rproj            = "pXXXX.Rproj",
                        use_bib          = FALSE) {

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

  stage <- validate_stage(stage, choices = eval(formals()$stage))

  ########################
  # Validation of authors, creator, corresp_auth, current_owner

  # Won't let new_project() continue if there are no authors and the user is
  # trying to add authors
  if(nrow(authors_tibble) == 0 &&
     !all(creator == Sys.info()["user"], is.na(corresp_auth),
          is.na(current_owner), is.null(authors))) {
    stop("Can't set authors, creator, corresp_auth, or current owner until an",
         " author is created. Run new_author()")
  }

  # Validate corresp_auth
  if(!is.na(corresp_auth)) {
    corresp_auth <- validate_entry(corresp_auth,
                                   what       = "author",
                                   rds_tibble = authors_tibble,
                                   max.length = 1)
  }

  # creator is validated against the authors tibble only if it's different from
  # Sys.info()["user"]. If the user cleverly tries to set creator as NA, it will
  # be set to Sys.info()["user"].
  if(is.na(creator)) {
    creator <- Sys.info()["user"]
  }
  else if(creator != Sys.info()["user"]) {
    creator <- validate_entry(creator,
                              what       = "author",
                              rds_tibble = authors_tibble,
                              max.length = 1)
  }

  # Validate current_owner
  if(!is.na(current_owner)) {
    current_owner <- validate_entry(current_owner,
                                    what       = "author",
                                    rds_tibble = authors_tibble,
                                    max.length = 1)
  }

  # If authors is left blank and current_owner isn't, authors is populated with
  # current_owner.
  # If current_owner is blank and authors isn't, current_owner is populated with
  # the first author.
  # If neither are blank and current_owner isn't in authors, current_owner is
  # added to authors immediately preceding the last author (unless there was
  # only one author in authors; in that case, it's made to be the second one).
  # If both authors and current_owner are blank, they're left blank.
  if(is.null(authors)) {
    if(!is.na(current_owner)) {
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

  protocol <- validate_protocol(protocol, choices = eval(formals()$protocol))

  files <-
    c("protocol", "datawork", "analysis", "report", "css", "Rproj") %>%
    stats::setNames(
      object = purrr::pmap(
        .l     = list(
          file_name        = list(protocol, datawork, analysis, report, css,
                                  Rproj),
          what             = .,
          default_name     = list(c("01_protocol.Rmd", "STROBE_protocol.Rmd",
                                    "CONSORT_protocol.Rmd"),
                                  "02_datawork.Rmd",
                                  "03_analysis.Rmd",
                                  "04_report.Rmd",
                                  "style.css",
                                  "pXXXX.Rproj"),
          default_template = list(list(STROBE_template, STROBE_template,
                                       CONSORT_template),
                                  list(datawork_template),
                                  list(analysis_template),
                                  list(report_template),
                                  list(css_template),
                                  list(Rproj_template))),
        .f     = validate_template,
        p_path = p_path),
      nm     = .)

  files$protocol <-
    build_protocol_report(vector = files$protocol,
                          what   = "protocol",
                          project_id          = id,
                          title               = title,
                          corresp_auth        = corresp_auth,
                          authors_tibble      = authors_tibble,
                          affiliations_tibble = affiliations_tibble,
                          project_authors     = authors,
                          aa_assoc_tibble     = aa_assoc_tibble,
                          use_bib             = use_bib,
                          pXXXX_name          = pXXXX_name)

  files$report <-
    build_protocol_report(vector = files$report,
                          what   = "report",
                          project_id          = id,
                          title               = title,
                          corresp_auth        = corresp_auth,
                          authors_tibble      = authors_tibble,
                          affiliations_tibble = affiliations_tibble,
                          project_authors     = authors,
                          aa_assoc_tibble     = aa_assoc_tibble,
                          use_bib             = use_bib,
                          pXXXX_name          = pXXXX_name)

  files$datawork <- build_datawork_analysis(vector     = files$datawork,
                                            what       = "datawork",
                                            p_path     = p_path,
                                            pXXXX_name = pXXXX_name)

  files$analysis <- build_datawork_analysis(vector     = files$analysis,
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
                                  status        = as.character(status),
                                  deadline_type = as.character(deadline_type),
                                  deadline      = as.Date(deadline),
                                  stage         = stage,
                                  path          = pXXXX_path,
                                  corresp_auth  = as.integer(corresp_auth),
                                  creator       = as.character(creator))

  # Add row(s) to project-author association table
  if(!is.null(authors)) {
    pa_assoc_tibble <-
      change_assoc(assoc_path   = pa_assoc_path,
                   assoc_tibble = pa_assoc_tibble,
                   new          = TRUE,
                   id1          = id,
                   id2          = authors)
  }

  # Write the files
  write_project_files(pXXXX_path      = pXXXX_path,
                      files           = files,
                      use_bib         = use_bib,
                      pXXXX_name      = pXXXX_name)


  message("\nProject ", id, " has been created at\n", pXXXX_path)
  print(dplyr::select(new_project_row,
                      -c("current_owner", "creator", "corresp_auth")))

  message("\nNew project's authors:")
  if(is.null(authors)) {
    print("None.")
  }
  else {
    print(pa_assoc_tibble %>%
            dplyr::filter(.data$id1 == id) %>%
            dplyr::left_join(authors_tibble,
                             by = c("id2" = "id")) %>%
            dplyr::select(-.data$id1) %>%
            dplyr::rename(author_id = .data$id2))
  }

  message("\nCurrent owner:")
  if(is.na(current_owner)) {
    print("None.")
  }
  else {
    print(dplyr::filter(authors_tibble, .data$id == current_owner))
  }

  message("\nCorresponding author:")
  if(is.na(corresp_auth)) {
    print("None.")
  }
  else {
    print(dplyr::filter(authors_tibble, .data$id == corresp_auth))
  }

  if(creator == Sys.info()["user"]) {
    message("\nCreator: ", creator)
  }
  else {
    message("\nCreator:")
    print(dplyr::filter(authors_tibble, .data$id == creator))
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
                       phone       = NA,    id           = NA) {

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

  new_author_row <- change_table(action      = "new",
                                 rds_path    = authors_path,
                                 rds_tibble  = authors_tibble,
                                 id          = id,
                                 given_names = as.character(given_names),
                                 last_name   = as.character(last_name),
                                 title       = as.character(title),
                                 degree      = as.character(degree),
                                 email       = tolower(as.character(email)),
                                 phone       = as.character(phone))

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
                     department_name  = as.character(department_name),
                     institution_name = as.character(institution_name),
                     address          = as.character(address)))
}
################################################################################

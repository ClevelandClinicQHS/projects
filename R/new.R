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
#' @param stage A number or string that will partially match exactly one of
#'   \code{c("1: design", "2: data collection", "3: analysis", "4: manuscript",
#'   "5: under review", "6: accepted", "0: ideas")}, communicating the stage the
#'   project is in. This will be coerced to be a character vector of class
#'   \code{projects_stage}. Defaults to \code{"1: design"}.
#'
#'   If set to one of \code{c("3: analysis", "4: manuscript", "5: under review",
#'   "6: accepted")}, \code{protocol} and \code{datawork} are ignored and the
#'   \emph{01_protocol.Rmd} and \emph{02_datawork.Rmd} files will not be
#'   written.
#' @param protocol,datawork,analysis,report,css,Rproj A character string
#'   matching the name of a corresponding template file in the \emph{.templates}
#'   subdirectory of the main \code{\link{projects_folder}}. Default templates
#'   are placed there when \code{\link{setup_projects}()} is run, and the user
#'   can edit these if desired.
#'
#'   Multiple default \code{protocol} templates are available.
#'
#'   \emph{01_protocol.Rmd}, which by default is the same as
#'   \emph{STROBE_protocol.Rmd}, will be selected if \code{protocol} is
#'   unspecified. Users can edit these default templates.
#'
#'   If using an edited or custom template, make sure to match the case and file
#'   extension exactly.
#'
#'   If the \code{stage} argument is set to one of \code{c("3: analysis", "4:
#'   manuscript", "5: under review", "6: accepted")}, \code{protocol} and
#'   \code{datawork} are ignored and the \emph{01_protocol.Rmd} and
#'   \emph{02_datawork.Rmd} files will not be written.
#' @param use_bib Logical. If \code{TRUE}, a blank \emph{.bib} file will be
#'   written into the \strong{progs} subdirectory of the newly created project
#'   folder. Its name will be of the form \emph{pXXXX.bib}, and the YAML header
#'   of \emph{progs/01_protocol.Rmd} and \emph{progs/04_report.Rmd} will include
#'   the line \code{bibliography: pXXXX.bib}.
#' @param stitle_as_folder Logical, indicating whether or not to use the
#'   \code{short_title} as the name of the new project's folder. Defaults to
#'   \code{FALSE}.
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
                        current_owner    = NA,
                        stage            = c("1: design", "2: data collection",
                                             "3: analysis", "4: manuscript",
                                             "5: under review", "6: accepted",
                                             "0: idea"),
                        status           = "just created",
                        short_title      = NA,
                        authors          = NULL,
                        deadline_type    = NA,
                        deadline         = NA,
                        path             = projects_folder(),
                        make_directories = FALSE,
                        corresp_auth     = NA,
                        creator          = NA,
                        id               = NA,
                        protocol         = c("01_protocol.Rmd",
                                             "STROBE_protocol.Rmd",
                                             "CONSORT_protocol.Rmd"),
                        datawork         = "02_datawork.Rmd",
                        analysis         = "03_analysis.Rmd",
                        report           = "04_report.Rmd",
                        css              = "style.css",
                        Rproj            = "pXXXX.Rproj",
                        use_bib          = FALSE,
                        stitle_as_folder = FALSE) {

  p_path             <- p_path()

  projects_path      <- make_rds_path("projects", p_path)
  projects_table     <- get_rds(projects_path)

  authors_table      <- authors_internal(p_path)

  pa_assoc_path      <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_table     <- get_rds(pa_assoc_path)

  affiliations_table <- affiliations_internal(p_path)

  aa_assoc_table     <- aa_assoc_internal(p_path)

  # Validation-----------------------------------------------------------------
  id <- validate_new(id = id, what = "project", rds_table = projects_table)

  title         <- validate_single_string(title)
  short_title   <- validate_single_string(short_title)
  status        <- validate_single_string(status)
  deadline_type <- validate_single_string(deadline_type)

  deadline <- validate_deadline(deadline)

  stage         <- validate_stage(stage)

  all_authors   <-
    validate_authors(
      general_authors = authors,
      current_owner   = current_owner,
      corresp_auth    = corresp_auth,
      creator         = creator,
      authors_table   = authors_table
    )

  if (!is.na(all_authors$corresp_auth)) {
    corresp_auth_row <-
      authors_table[match(all_authors$corresp_auth, authors_table$id), ]
  } else {
    corresp_auth_row <- NULL
  }

  path          <-
    validate_directory(
      path             = path,
      p_path           = p_path,
      make_directories = make_directories
    )

  # File preparation-----------------------------------------------------------

  pXXXX_name    <-
    make_project_name(
      x           = ifelse(stitle_as_folder, short_title, id),
      short_title = stitle_as_folder
    )

  pXXXX_path <- make_project_path(pXXXX_name, path)

  if (fs::dir_exists(pXXXX_path)) {
    stop(
      "The directory\n", pXXXX_path, "\nalready exists.",
      "\nMove or delete it, or pick a different project id/short_title."
    )
  }

  files <-
    build_rmds(
      stage              = stage,
      protocol           = protocol,
      datawork           = datawork,
      analysis           = analysis,
      report             = report,
      css                = css,
      Rproj              = Rproj,
      p_path             = p_path,
      id                 = id,
      title              = title,
      authors            = all_authors$general_authors,
      corresp_auth_row   = corresp_auth_row,
      authors_table      = authors_table,
      affiliations_table = affiliations_table,
      aa_assoc_table     = aa_assoc_table,
      use_bib            = use_bib,
      pXXXX_name         = pXXXX_name
    )

  # Add new row to project list
  new_project_row <-
    change_table(
      action        = "new",
      rds_path      = projects_path,
      rds_table     = projects_table,
      id            = id,
      title         = title,
      short_title   = short_title,
      current_owner = all_authors$current_owner,
      status        = status,
      deadline_type = deadline_type,
      deadline      = deadline,
      stage         = stage,
      path          = pXXXX_path,
      corresp_auth  = all_authors$corresp_auth,
      creator       = all_authors$creator
    )

  # Add row(s) to project-author association table
  if (!is.null(all_authors$general_authors)) {
    pa_assoc_table <-
      change_assoc(
        assoc_path   = pa_assoc_path,
        assoc_table = pa_assoc_table,
        new          = TRUE,
        id1          = id,
        id2          = all_authors$general_authors
      )
  }

  # Write the files
  write_project_files(
    pXXXX_path = pXXXX_path,
    files      = files,
    use_bib    = use_bib,
    pXXXX_name = pXXXX_name
  )


  # Print results--------------------------------------------------------------

  message("\nProject ", id, " has been created at\n", pXXXX_path)
  print(
    dplyr::select(
      new_project_row,
      "id",
      "title",
      "stage",
      "status",
      "deadline_type",
      "deadline"
    )
  )

  message("\nNew project's authors:")
  if (length(all_authors$general_authors) == 0) {
    cat("None.")
  } else {
    print(
      pa_assoc_table %>%
        dplyr::filter(.data$id1 == !!id) %>%
        dplyr::left_join(authors_table, by = c("id2" = "id")) %>%
        dplyr::select(-"id1") %>%
        dplyr::rename("author_id" = "id2")
    )
  }

  print(
    dplyr::select(
      new_project_row, "current_owner", "corresp_auth", "creator"
    )
  )

  invisible(new_project_row)
}
################################################################################





build_rmds <- function(stage,
                       protocol,
                       datawork,
                       analysis,
                       report,
                       css,
                       Rproj,
                       p_path,
                       id,
                       title,
                       authors,
                       corresp_auth_row,
                       authors_table,
                       affiliations_table,
                       aa_assoc_table,
                       use_bib,
                       pXXXX_name) {

  protocol_choices <- eval(formals(new_project)$protocol)

  if (any(c("1: design", "0: idea") == as.character(stage))) {
    protocol <- validate_protocol(protocol, choices = protocol_choices)
  }

  files <-
    list(
      file_name = list(protocol, datawork, analysis, report, css, Rproj),
      what =  c("protocol", "datawork", "analysis", "report", "css", "Rproj"),
      default_name =
        list(c("01_protocol.Rmd",
               "STROBE_protocol.Rmd",
               "CONSORT_protocol.Rmd"),
             "02_datawork.Rmd",
             "03_analysis.Rmd",
             "04_report.Rmd",
             "style.css",
             "pXXXX.Rproj"
        ),
      default_template =
        list(
          list(STROBE_template, STROBE_template, CONSORT_template),
          list(datawork_template),
          list(analysis_template),
          list(report_template),
          list(css_template),
          list(Rproj_template)
        )
    )

  if (
    any(
      c(
        "2: data collection",
        "3: analysis",
        "4: manuscript",
        "5: under review",
        "6: accepted"
      ) == as.character(stage)
    )
  ) {

    if (!identical(protocol, protocol_choices)) {
      stop(
        "User input to protocol argument detected, but protocol is not\n",
        "used when stage is set to \"2: data collection\" or higher"
      )
    }

    files <- lapply(files, utils::tail, n = -1)

    if (stage != "2: data collection") {

      if (!identical(datawork, eval(formals(new_project)$datawork))) {
        stop(
          "User input to datawork argument detected, but datawork is not\n",
          "used when stage is set to \"3: analysis\" or higher"
        )
      }

      files <- lapply(files, utils::tail, n = -1)
    }
  }

  files <-
    stats::setNames(
      object = purrr::pmap(files, validate_template, p_path = p_path),
      nm     = files$what
    )

  if (!is.null(files$protocol)) {
    files$protocol <-
      build_protocol_report(
        vector             = files$protocol,
        what               = "protocol",
        project_id         = id,
        title              = title,
        corresp_auth_row   = corresp_auth_row,
        authors_table      = authors_table,
        affiliations_table = affiliations_table,
        project_authors    = authors,
        aa_assoc_table     = aa_assoc_table,
        use_bib            = use_bib,
        pXXXX_name         = pXXXX_name
      )
  }

  files$report <-
    build_protocol_report(
      vector              = files$report,
      what                = "report",
      project_id          = id,
      title               = title,
      corresp_auth_row    = corresp_auth_row,
      authors_table       = authors_table,
      affiliations_table  = affiliations_table,
      project_authors     = authors,
      aa_assoc_table      = aa_assoc_table,
      use_bib             = use_bib,
      pXXXX_name          = pXXXX_name
    )

  if (!is.null(files$datawork)) {
    files$datawork <-
      build_datawork_analysis(
        vector     = files$datawork,
        what       = "datawork",
        p_path     = p_path,
        pXXXX_name = pXXXX_name
      )
  }

  files$analysis <-
    build_datawork_analysis(
      vector     = files$analysis,
      what       = "analysis",
      p_path     = p_path,
      pXXXX_name = pXXXX_name
    )

  files
}



#' @export
new_idea <- function(title = NA, status = "just an idea", ...) {
  utils::capture.output(
    idea <-
      suppressMessages(
        new_project(title = title, stage = "0: idea", status = status, ...)
      )
  )
  idea[c("id", "title", "status")]
}



################################################################################
#' @rdname new_edit_delete
#' @importFrom rlang .data
#' @export
new_author <- function(given_names  = NA,
                       last_name    = NA,
                       title        = NA,
                       affiliations = NULL,
                       degree       = NA,
                       email        = NA,
                       phone        = NA,
                       id           = NA) {

  p_path         <- p_path()

  authors_path   <- make_rds_path("authors", p_path)
  authors_table  <- get_rds(authors_path)

  id <- validate_new(id = id, what = "author", rds_table = authors_table)

  given_names <- validate_single_string(given_names)
  last_name   <- validate_single_string(last_name)
  title       <- validate_single_string(title)
  degree      <- validate_single_string(degree)
  email       <- validate_single_string(email) %>% tolower()
  phone       <- validate_single_string(phone)

  if (!is.null(affiliations)) {

    aa_assoc_path      <- make_rds_path("author_affiliation_assoc", p_path)
    aa_assoc_table     <- get_rds(aa_assoc_path)
    affiliations_table <- affiliations_internal(p_path)

    if (nrow(affiliations_table) == 0) {
      stop(
        "Can't set affiliations until an affiliation is created. ",
        "Run new_affiliation()"
      )
    }

    affiliations <-
      validate_unique_entry_list(
        affiliations,
        table = affiliations_table,
        what  = "affiliation"
      )$id
  }

  new_author_row <-
    change_table(
      action      = "new",
      rds_path    = authors_path,
      rds_table   = authors_table,
      id          = id,
      given_names = given_names,
      last_name   = last_name,
      title       = title,
      degree      = degree,
      email       = email,
      phone       = phone
    )

  if (!is.null(affiliations)) {
    new_author_affiliations <-
      change_assoc(
        assoc_path  = aa_assoc_path,
        assoc_table = aa_assoc_table,
        new         = TRUE,
        id1         = id,
        id2         = affiliations
      ) %>%
      dplyr::filter(.data$id1 == id)
  }

  message("New author:")
  print(new_author_row)

  if (!is.null(affiliations)) {
    message("\nNew author's affiliations:")
    print(
      new_author_affiliations %>%
        dplyr::filter(.data$id1 == id) %>%
        dplyr::left_join(affiliations_table, by = c("id2" = "id")) %>%
        dplyr::select(-"id1") %>%
        dplyr::rename("affiliation_id" = "id2")
    )
  } else {
    cat("None.")
  }

  invisible(new_author_row)
}
################################################################################


################################################################################
#' @rdname new_edit_delete
#' @export
new_affiliation <- function(department_name  = NA,
                            institution_name = NA,
                            address          = NA,
                            id               = NA) {

  affiliations_path   <- make_rds_path("affiliations")
  affiliations_table  <- get_rds(affiliations_path)

  id <-
    validate_new(id = id, what = "affiliations", rds_table = affiliations_table)

  department_name  <- validate_single_string(department_name)
  institution_name <- validate_single_string(institution_name)
  address          <- validate_single_string(address)

  message("New affiliation:")

  change_table(
    action           = "new",
    rds_path         = affiliations_path,
    rds_table        = affiliations_table,
    id               = id,
    department_name  = department_name,
    institution_name = institution_name,
    address          = address
  )
}
################################################################################

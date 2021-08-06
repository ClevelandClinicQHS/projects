#' Create, edit or delete projects, authors and affiliations
#'
#' These functions create, edit, or delete rows in the \code{\link{projects}()},
#' \code{\link{tasks}()}, \code{\link{authors}()}, and
#' \code{\link{affiliations}()} tables, which are stored in the \emph{.metadata}
#' subdirectory of the main \link[=projects_folder]{projects folder}.
#'
#' \code{new_project()} copies the folder in the \emph{.templates} folder named
#' by the \code{template_name} argument into the
#' \link[=projects_folder]{projects folder}, giving it the name specified by the
#' \code{folder_name} argument. It then creates a line in the
#' \code{\link{projects}()} table for the newly created project, filling many of
#' its fields with the contents of corresponding arguments of this function. See
#' \code{\link{setup_projects}()} for more information on the \emph{.templates}
#' folder.
#'
#' \code{delete_project()} deletes project folders and removes their line from
#' the \code{\link{projects}()} table.
#'
#' The \code{edit_*()} functions and the other \code{new_*()} and
#' \code{delete_*()} functions only create or edit rows in the \emph{.metadata}
#' tables.
#'
#' \code{new_idea()} is a convenience function for quickly creating projects in
#' the \code{"0: idea"} stage.
#'
#' \code{finish()} is a shortcut for \code{edit_task(my_project, my_TID, done =
#' 1)}
#'
#' @param id An integer that will become the item's permanent identification
#'   number. Must be in the range 1-9999 or left blank. If left blank, the
#'   lowest available integer in the aforementioned range will be selected.
#' @param title For \code{new_project()}, \code{new_idea()}, and
#'   \code{edit_project()}, the title of the project.
#'
#'   For the \code{new_author()} and \code{edit_author()}, the job title of the
#'   author.
#' @param short_title A nickname for the project. Can be used in other
#'   \code{projects} package functions whenever needing to specify a project.
#' @param task The name / very short description of the task.
#' @param TID The "\strong{t}ask \strong{ID}." \code{TID}s are always
#'   standardized so that for each project they are positive integers starting
#'   with 1.
#'
#'   For \code{new_task()}, this is used to specify the \code{TID} of the new
#'   task. The user can enter any scalar, non-missing numeric value, and all
#'   tasks' \code{TID}s will be standardized. Therefore, the default of
#'   \code{Inf} for a new task always places the new task at the end of the
#'   list. If the new task's \code{TID} is the same as an existing task's
#'   \code{TID}, the new task will take priority.
#'
#'   For \code{edit_task()} and \code{delete_task()}, this argument, along with
#'   the \code{project} argument, identifies the task to be edited or deleted.
#' @param new_TID Used to change the task ID of the task specified by
#'   \code{project} and \code{TID}. The user can enter a scalar, non-missing
#'   numeric value, and the \code{TID}s of all the project's tasks' will be
#'   re-standardized (see the description of the \code{TID} argument above). The
#'   task's TID will be unchanged if \code{new_TID} is left as the default
#'   (\code{NULL}).
#' @param given_names,last_name,department_name,institution_name Each a single
#'   character string. Can be used whenever needing to specify a specific
#'   author/affiliation.
#' @param degree A character string (preferably an abbreviation) denoting the
#'   author's academic degree(s). Will be written next to author names when
#'   \code{\link{header}()} is run.
#' @param email,phone A character string denoting the email/phone of the author.
#'   Email will be coerced to lowercase. When a project is given a
#'   \code{corresp_auth}, \code{email} will be included in "Corresponding
#'   author:" section written by \code{\link{header}()}.
#' @param address A character string indicating the address of the affiliation.
#' @param project,author,affiliation The \code{id} or unambiguous name(s) of a
#'   project/author/affiliation to \code{new_task()} or \code{edit_*()} or
#'   \code{delete_*()}.
#' @param authors,affiliations For \code{new_project()}/\code{new_author()}, a
#'   vector of \code{id}s or unambiguous \code{given_names}/\code{last_name} or
#'   \code{department_name}/\code{institution_name} of authors/affiliations.
#'   Order will be preserved.
#'
#'   For \code{edit_project()}/\code{edit_author()}, a formula specifying
#'   authors/affiliations to add or remove from the project/author. Formulas
#'   must have no left-hand side (i.e., begin with \code{\link{~}}) and use
#'   \code{+} to add and \code{-} to remove (see \link{formula}).
#'
#'   Authors and affiliations may be specified by \code{id} or name. Each
#'   element must match an existing row in the
#'   \code{\link{authors}()}/\code{\link{affiliations}()} table.
#' @param current_owner,corresp_auth,creator,lead An \code{id} or unambiguous
#'   \code{last_name}/\code{given_names} of one of the authors in the
#'   \code{\link{authors}()} table, which will be coerced into a
#'   \link{projects_author-class} object.
#'
#'   If \code{corresp_auth} is specified, this author's contact information will
#'   be especially included in the output of \code{\link{header}()}.
#'
#'   If \code{creator} is left blank, the numeric portion of the resulting
#'   \link{projects_author-class} object will be \code{0:}, followed by the
#'   value of \code{\link{Sys.info}()["user"]} (e.g., \code{0: user_j_smith}).
#' @param stage A number or string that will partially match exactly one of
#'   \code{c("1: design", "2: data collection", "3: analysis", "4: manuscript",
#'   "5: under review", "6: accepted", "0: ideas")}, communicating the stage the
#'   project is in. This will be coerced to be a character vector of class
#'   \code{projects_stage}.
#'
#'   For \code{new_project()}, defaults to \code{"1: design"}.
#'
#'   See \link{projects_stage-class}.
#' @param impact A scalar numeric value, intended to communicate the estimated
#'   impact the project will have.
#' @param status A free text field, intended to communicate the most current
#'   condition the project or task is in.
#'
#'   For \code{new_project()}, default is \code{"just created"}. For
#'   \code{new_idea()}, default is \code{"just an idea"}. For \code{new_task()},
#'   the default is \code{NA}.
#' @param deadline_type A free text field, intended to communicate the meaning
#'   of the next field, \code{deadline}.
#' @param deadline A \code{\link{POSIXct}} object or something coercible to one
#'   (via \code{lubridate::\link[lubridate]{as_datetime}()}).
#' @param effort,timing Any scalar numeric value, intended to communicate the
#'   level of effort the task will require and the nature of the timing of the
#'   task, respectively.
#' @param done Used to indicate whether or not the task has been completed. Must
#'   be \code{0}, \code{1}, or \code{NA}.
#' @param folder_name A character string that can serve as a valid directory
#'   name. By default, it is "p" followed by the project \code{id} number
#'   left-filled with "0" until the number is four digits long.
#' @param parent_directory A character string that can be read as a file path.
#'   Can be either:
#'
#'   \enumerate{
#'
#'   \item the \emph{absolute} path of the \link[=projects_folder]{projects
#'   folder} (i.e., the value of \code{\link{projects_folder}()}, which is the
#'   default)
#'
#'   \item an \emph{absolute} path pointing to a subfolder within the
#'   \link[=projects_folder]{projects folder}
#'
#'   \item a \emph{relative} path (leading \code{"."} optional) that will be
#'   appended onto the end of the value of \code{\link{projects_folder}()}.
#'
#'   }
#'
#'   In any case, the result is that the new project folder will be a
#'   subdirectory of the main \link[=projects_folder]{projects folder}. See also
#'   \code{\link{setup_projects}()}.
#' @param make_directories Logical, indicating whether or not
#'   \code{new_project()} should create subdirectories specified in the
#'   \code{path} argument that do not already exist. Ignored if \code{path} is
#'   left as the default or if all directories in \code{path} already exist.
#' @param template_folder A character string naming a folder in the
#'   \emph{.templates} folder that will be copied into the
#'   \link[=projects_folder]{projects folder} as the new project folder, renamed
#'   according to the value of the \code{folder_name} argument. See also
#'   \strong{Details} below.
#' @param archived Logical indicating whether or not the function should
#'   consider archived projects when determining which project the user is
#'   referring to in the \code{project} argument. \code{FALSE} by default.
#'
#'   See the \strong{Details} section of \code{\link{archive_project}()} for
#'   more information on the "archived" status of a project.
#' @param ... Additional arguments to be passed to \code{new_project()}
#'
#' @return \code{new_affiliation()} and \code{edit_affiliation()} simply return
#'   the new or edited row of the \code{\link{affiliations}()} \code{tibble}.
#'
#'   \code{new_project()}, \code{new_author()}, \code{edit_project()},
#'   \code{edit_author()}, and the \code{delete_*()} functions
#'   \link[=invisible]{invisibly} return the row of the corresponding metadata
#'   \code{tibble} that was added/edited/deleted, although the contents of this
#'   row are printed as a side-effect along with the other relevant information
#'   where applicable (e.g., project authors, author affiliations, project file
#'   paths).
#'
#'   \code{new_idea()} returns the \code{id}, \code{title}, and \code{status}
#'   columns of the newly created row of the \code{\link{projects}()}
#'   \code{tibble}.
#'
#' @examples
#' #############################################################################
#' # SETUP
#' old_home <- Sys.getenv("HOME")
#' old_ppath <- Sys.getenv("PROJECTS_FOLDER_PATH")
#' temp_dir <- tempfile("dir")
#' dir.create(temp_dir)
#' Sys.unsetenv("PROJECTS_FOLDER_PATH")
#' Sys.setenv(HOME = temp_dir)
#' setup_projects(path = temp_dir)
#' #############################################################################
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
#' new_author(
#'   given_names = "Rosetta",
#'   last_name = "Stone",
#'   affiliations = c(42, "Math"),
#'   degree = "PhD",
#'   email = "slab@rock.net",
#'   phone = "867-555-5309",
#'   id = 8888
#' )
#' new_author(
#'   given_names = "Spiro",
#'   last_name = "Agnew",
#'   degree = "LLB",
#'   affiliations = "Art D", id = 13
#' )
#' new_author(last_name = "Plato", id = 303)
#'
#' # Editing an author, showcasing the removal of a text element (last_name)
#' edit_author(author = 303, given_names = "Plato", last_name = NA)
#'
#' # Editing an author, showcasing the addition and removal of affiliations
#' edit_author("Spiro", affiliations = ~ -"Art D" + Math)
#'
#' # Creating a project
#' new_project(
#'   title = "Understanding the Construction of the United States",
#'   short_title = "USA",
#'   authors = c(13, "Stone"),
#'   stage = 4,
#'   deadline = "2055-02-28",
#'   deadline_type = "submission",
#'   parent_directory = "famous_studies/philosophers/rocks",
#'   corresp_auth = "Stone",
#'   current_owner = "agnew",
#'   make_directories = TRUE,
#'   status = "waiting on IRB"
#' )
#'
#' # Editing a project, showcasing the addition and removal of authors
#' edit_project(
#'   "Understanding",
#'   short_title = "usa1",
#'   authors = ~ + "303" - 13 - Stone
#' )
#'
#'
#' # Creating a new idea
#' new_idea(title = "Boiling the Ocean")
#'
#' # Creating some tasks
#' new_task(project = 2,
#'          task = "put the horse before the cart",
#'          lead = "spiro",
#'          timing = NaN)
#'
#' new_task(project = 1,
#'          task = "learn something",
#'          effort = pi,
#'          lead = "Stone",
#'          status = "foobar",
#'          timing = Inf)
#'
#' new_task(project = 1,
#'          task = "take a break",
#'          TID = 600.66,
#'          effort = -100,
#'          status = "throw it all away",
#'          lead = "303")
#'
#' new_task(project = 1,
#'          task = "tie your shoes",
#'          TID = 2,
#'          lead = 303)
#'
#' new_task(project = 1,
#'          task = "put out the fire",
#'          TID = .5,
#'          lead = "stone")
#'
#' # Editing a task
#' edit_task(project = "understanding",
#'           TID = 3,
#'           new_TID = 2,
#'           lead = "agnew",
#'           done = 1,
#'           status = "make dinner")
#'
#' # finish is a shortcut for setting done = 1 on the specified task.
#' finish("construction", 4)
#'
#' # Wrapped in if (interactive()) because it requires interactive console input
#' # and fails automated package checking and testing.
#' if (interactive()) {
#'   delete_project("usa1")
#'   delete_author(303)
#'   delete_task(2, 1)
#'   delete_affiliation("Math")
#' }
#'
#' #############################################################################
#' # CLEANUP
#' Sys.setenv(HOME = old_home, PROJECTS_FOLDER_PATH = old_ppath)
#' @name new_edit_delete
#' @export
new_project <- function(title            = NA,
                        current_owner    = NA,
                        stage            = c("1: design", "2: data collection",
                                             "3: analysis", "4: manuscript",
                                             "5: under review", "6: accepted",
                                             "0: idea"),
                        impact           = NA,
                        status           = "just created",
                        short_title      = NA,
                        authors          = NULL,
                        corresp_auth     = NA,
                        creator          = NA,
                        deadline_type    = NA,
                        deadline         = NA,
                        id               = NA,
                        folder_name      =
                          paste0("p", stringr::str_pad(id, 4, pad = "0")),
                        parent_directory = projects_folder(),
                        make_directories = FALSE,
                        template_folder  = "default_folder"
                        ) {

  p_path             <- get_p_path()

  projects_path      <- make_rds_path("projects", p_path)
  projects_table     <- get_rds(projects_path)

  authors_table      <- authors_internal(p_path)

  pa_assoc_path      <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_table     <- get_rds(pa_assoc_path)

  # Validation-----------------------------------------------------------------
  id <- validate_new(id = id, what = "project", rds_table = projects_table)

  title         <- validate_single_string(title)
  short_title   <- validate_single_string(short_title)
  status        <- validate_single_string(status)
  deadline_type <- validate_single_string(deadline_type)
  folder_name   <-
    validate_single_string(folder_name, na.ok = FALSE, zero.chars.ok = FALSE)
  deadline <- validate_deadline(deadline)
  stage <-
    if (missing(stage)) projects_stage(1L) else validate_stage(stage, n = 1)
  impact <- validate_single_number(impact)

  all_authors   <-
    validate_authors(
      general_authors = authors,
      current_owner   = current_owner,
      corresp_auth    = corresp_auth,
      creator         = creator,
      authors_table   = authors_table
    )

  parent_directory <-
    validate_directory(
      path             = parent_directory,
      p_path           = p_path,
      make_directories = make_directories
    )

  # File preparation-----------------------------------------------------------

  folder_path <- make_project_path(folder_name, parent_directory)

  if (fs::dir_exists(folder_path)) {
    stop(
      "The directory\n", folder_path, "\nalready exists.",
      "\nMove or delete it, or pick a different folder_name."
    )
  }

  new_project_row <-
    dplyr::tibble(
      id = id,
      title         = title,
      short_title   = short_title,
      current_owner = all_authors$current_owner,
      status        = status,
      deadline_type = deadline_type,
      deadline      = deadline,
      stage         = stage,
      impact        = impact,
      path          = folder_path,
      corresp_auth  = all_authors$corresp_auth,
      creator       = all_authors$creator
    )

  add_metadata(
    table = projects_table,
    new_row = new_project_row,
    table_path = projects_path,
    .ptype = projects_ptype
  )

  # Add row(s) to project-author association table
  if (!is.null(all_authors$general_authors)) {

    new_pa_assoc <- dplyr::tibble(id1 = id, id2 = all_authors$general_authors)

    add_assoc(
      assoc_table = pa_assoc_table,
      new_rows    = new_pa_assoc,
      assoc_path  = pa_assoc_path
    )
  }

  new_project_folder(
    folder_path = folder_path,
    folder_name = folder_name,
    template_folder = template_folder,
    p_path = p_path
  )


  # Print results--------------------------------------------------------------

  message("\nProject ", id, " has been created at\n", folder_path)
  print(
    new_project_row[
      c("id", "title", "stage", "status", "deadline_type", "deadline")
    ]
  )

  message("\nNew project's authors:")
  if (length(all_authors$general_authors) == 0L) {
    cat("None.\n")
  } else {
    print(
      new_pa_assoc %>%
        dplyr::left_join(authors_table, by = c("id2" = "id")) %>%
        dplyr::select(-"id1") %>%
        dplyr::rename("author_id" = "id2")
    )
  }

  print(new_project_row[c("current_owner", "corresp_auth", "creator")])

  invisible(new_project_row)
}


#' @rdname new_edit_delete
#' @export
new_idea <- function(title, status = "just an idea", ...) {
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
new_task <- function(project,
                     task = NA,
                     TID = Inf,
                     lead = NA,
                     effort = NA,
                     timing = NA,
                     status = NA,
                     done = 0L,
                     archived = FALSE) {
  p_path          <- get_p_path()

  projects_table  <- projects_internal(p_path = p_path, archived = archived)

  project <-
    validate_unique_entry(
      x     = project,
      table = projects_table,
      what  = "project"
    )

  tasks_path <- make_rds_path("tasks", p_path)
  tasks_table <- get_rds(tasks_path)

  authors_table <- authors_internal(p_path)

  task <- validate_single_string(task)
  effort <- validate_single_number(effort)
  timing <- validate_single_number(timing)
  status <- validate_single_string(status)
  done <- validate_single_integer(done, na.ok = TRUE, min = 0L, max = 1L)
  TID <- validate_single_number(TID, na.ok = FALSE)
  # We validate the lead against the authors table but not against the projects
  # author list
  lead <- validate_projects_author(lead, authors_table)

  new_task_row <-
    dplyr::tibble(
      PID = project$id,
      TID = TID,
      done = done,
      task = task,
      effort = effort,
      timing = timing,
      lead = lead,
      status = status
    )

  project_tasks <-
    add_metadata(
      table = tasks_table,
      new_row = new_task_row,
      table_path = tasks_path,
      .ptype = tasks_ptype,
      task = TRUE
    ) %>%
    dplyr::filter(.data$PID == !!project$id) %>%
    dplyr::arrange(.data$TID)

  # Print results--------------------------------------------------------------
  message("\nNew task was added to project ", project$id, ":")
  project_tasks
}



################################################################################
#' @rdname new_edit_delete
#' @export
new_author <- function(given_names  = NA,
                       last_name    = NA,
                       title        = NA,
                       affiliations = NULL,
                       degree       = NA,
                       email        = NA,
                       phone        = NA,
                       id           = NA) {
  p_path         <- get_p_path()

  authors_path   <- make_rds_path("authors", p_path)
  authors_table  <- get_rds(authors_path)

  id <- validate_new(id = id, what = "author", rds_table = authors_table)

  given_names <- validate_single_string(given_names)
  last_name   <- validate_single_string(last_name)
  title       <- validate_single_string(title)
  degree      <- validate_single_string(degree)
  email       <- validate_single_string(email, tolower = TRUE)
  phone       <- validate_single_string(phone)

  if (!is.null(affiliations)) {

    aa_assoc_path      <- make_rds_path("author_affiliation_assoc", p_path)
    aa_assoc_table     <- get_rds(aa_assoc_path)
    affiliations_table <- affiliations_internal(p_path)

    if (nrow(affiliations_table) == 0L) {
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
    dplyr::tibble(
      id          = id,
      given_names = given_names,
      last_name   = last_name,
      title       = title,
      degree      = degree,
      email       = email,
      phone       = phone
    ) %>%
    add_metadata(
      table = authors_table,
      new_row = .,
      table_path = authors_path,
      .ptype = authors_ptype
    ) %>%
    dplyr::slice_tail()

  if (!is.null(affiliations)) {

    new_aa_assoc <- dplyr::tibble(id1 = id, id2 = affiliations)

    add_assoc(
      assoc_table = aa_assoc_table,
      new_rows    = new_aa_assoc,
      assoc_path  = aa_assoc_path
    )
  }

  message("New author:")
  print(new_author_row)

  message("\nNew author's affiliations:")
  if (!is.null(affiliations)) {
    print(
      new_aa_assoc %>%
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

  new_affiliation_row <-
    dplyr::tibble(
      id               = id,
      department_name  = department_name,
      institution_name = institution_name,
      address          = address
    ) %>%
    add_metadata(
      table = affiliations_table,
      new_row = .,
      table_path = affiliations_path,
      .ptype = affiliations_ptype
    ) %>%
    dplyr::slice_tail()

  new_affiliation_row
}
################################################################################




new_project_folder <- function(folder_path,
                               folder_name,
                               template_folder,
                               p_path) {
  fs::dir_copy(
    path     = fs::path(p_path, ".templates", template_folder),
    new_path = folder_path
  )

  fs::file_move(
    path     = fs::dir_ls(folder_path, glob = "*.Rproj"),
    new_path = fs::path(folder_path, folder_name, ext = "Rproj")
  )
}

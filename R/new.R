#' Create, edit or delete projects, authors and affiliations
#'
#' These functions create, edit, or delete rows in the \code{\link{projects}()},
#' \code{\link{authors}()}, and \code{\link{affiliations}()} tables, which are
#' stored in the \emph{.metadata} subdirectory of the main
#' \link[=projects_folder]{projects folder}.
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
#'   project/author/affiliation to \code{edit_*()} or to \code{delete_*()}.
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
#' @param current_owner,corresp_auth,creator An \code{id} or unambiguous
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
#' @param status A free text field, intended to communicate the most current
#'   condition the project is in.
#'
#'   For \code{new_project()}, default is \code{"just created"}. For
#'   \code{new_idea()}, default is \code{"just an idea"}.
#' @param deadline_type A free text field, intended to communicate the meaning
#'   of the next field, \code{deadline}.
#' @param deadline A \code{\link{POSIXct}} object or something coercible to one
#'   (via \code{lubridate::\link[lubridate]{as_datetime}()}).
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
#'   parent_directory = "famous_studied/philosophers/rocks",
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
#' new_idea(title = "Boiling the Ocean")
#'
#' # Wrapped in if (interactive()) because it requires interactive console input
#' # and fails automated package checking and testing.
#' if (interactive()) {
#'   delete_project("usa1")
#'   delete_author(303)
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

  deadline      <- validate_deadline(deadline)
  stage         <- validate_stage(stage)

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
    tibble::tibble(
      id = id,
      title         = title,
      short_title   = short_title,
      current_owner = all_authors$current_owner,
      status        = status,
      deadline_type = deadline_type,
      deadline      = deadline,
      stage         = stage,
      path          = folder_path,
      corresp_auth  = all_authors$corresp_auth,
      creator       = all_authors$creator
    )

  add_metadata(
    table = projects_table,
    new_row = new_project_row,
    table_path = projects_path
  )

  # Add row(s) to project-author association table
  if (!is.null(all_authors$general_authors)) {

    new_pa_assoc <- tibble::tibble(id1 = id, id2 = all_authors$general_authors)

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
    cat("None.")
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
    add_metadata(
      table = authors_table,
      new_row =
        tibble::tibble(
          id          = id,
          given_names = given_names,
          last_name   = last_name,
          title       = title,
          degree      = degree,
          email       = email,
          phone       = phone
        ),
      table_path = authors_path
    )

  if (!is.null(affiliations)) {

    new_aa_assoc <- tibble::tibble(id1 = id, id2 = affiliations)

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
    add_metadata(
      table = affiliations_table,
      new_row =
        tibble::tibble(
          id               = id,
          department_name  = department_name,
          institution_name = institution_name,
          address          = address
        ),
      table_path = affiliations_path
    )

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

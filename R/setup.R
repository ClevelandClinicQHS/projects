
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", ":="))

#' Set up the projects folder
#'
#' Creates or restores the projects folder at the user-specified path.
#'
#' The \code{\link[=projects-package]{projects}} package remembers where the
#' \link[=projects_folder]{projects folder} is located by storing its file path
#' in a \link{.Renviron} file (the home .Renviron file by default). The entry is
#' named \code{PROJECTS_FOLDER_PATH}.
#'
#' Note that changing the \code{.Renviron_path} argument may create an .Renviron
#' file that R will not notice or use. See \link{Startup} for more details.
#'
#' @section Default contents: The \link[=projects_folder]{projects folder}
#'   automatically contains the subdirectories \emph{.metadata} and
#'   \emph{.template}, which are hidden by default on some operating systems.
#'
#'   The \emph{.metadata} folder and its contents should \strong{never} be
#'   manually moved or modified.
#'
#'   The \emph{.templates} will contain several templates that
#'   \code{\link{new_project}()} reads when creating a new project. Advanced
#'   users may edit these templates or add their own. See
#'   \code{\link{new_project}()} for details.
#'
#' @section Behavior when projects folder already exists: If \code{overwrite =
#'   TRUE}, the function will run no matter what. Use with caution.
#'
#'   If the user has a pre-existing \link[=projects_folder]{projects folder} and
#'   runs this command with the pre-existing projects folder's path, nothing
#'   will be deleted.
#'
#'   \strong{Therefore}, if the user "broke" the projects folder (e.g., by
#'   deleting metadata; by changing the "PROJECTS_FOLDER_PATH" line in the
#'   \emph{.Renviron} file), the user can "fix" the projects folder to some
#'   degree by running this function with the folder's actual file path (e.g.,
#'   restore all default templates; restore missing metadata files).
#'
#' @param path The full file path where the user would like a directory called
#'   "projects" to be created, wherein all projects and their data will dwell.
#' @param overwrite Logical indicating whether or not to abandon any previously
#'   stored projects folders stored in the system.
#' @param make_directories Logical indicating whether or not the function should
#'   write any directories specified in the \code{path} argument that don't
#'   already exist.'
#' @param .Renviron_path The full file path of the .Renviron file where the user
#'   would like to store the \code{\link{projects_folder}()} path. Default is
#'   the home .Renviron file. If the file doesn't exist it will be created.
#'
#' @examples
#' # This sequence is used in all other examples in this package.
#'
#' # Back up old projects_folder()
#' old_path <- Sys.getenv("PROJECTS_FOLDER_PATH")
#'
#' # This sets up an example projects_folder() in a temporary directory.
#' # It will not edit any of the user's .Renviron files.
#' setup_projects(path = tempdir(), .Renviron_path = fs::path_temp(".Renviron"))
#'
#' # Cleanup
#' Sys.setenv(PROJECTS_FOLDER_PATH = old_path)
#' fs::file_delete(c(fs::path_temp("projects"), fs::path_temp(".Renviron")))
#' @return The project folder's path, invisibly. It will be \code{""} if it
#'   doesn't exist.
#'
#' @seealso \code{\link{new_project}()} for information on templates
#'
#'   \link{Startup} for more information on how \emph{.Renviron} files work.
#'
#' @export
setup_projects <- function(path,
                           overwrite        = FALSE,
                           make_directories = FALSE,
                           .Renviron_path   = fs::path_home_r(".Renviron")) {
  path     <- path %>%
    validate_directory(
      p_path           = NULL,
      make_directories = make_directories
    ) %>%
    fs::path("projects")

  old_path <- p_path_from_explicit_renviron(.Renviron_path)

  # If overwite = TRUE, function will run no matter what, overwriting any
  # pre-existing value of PROJECTS_FOLDER_PATH in the home .Renviron file.
  #
  # If overwrite = FALSE, function will still run UNLESS a
  # PROJECTS_FOLDER_PATH value already exists and does not match up with the
  # user-specified path.

  if (old_path != "" && old_path != path && !overwrite) {
    message(
      "\nThe .Renviron file at\n",
      .Renviron_path,
      "\nindicates that a 'projects' folder already exists at\n",
      old_path,
      '\n\nRerun with that path OR set overwrite = TRUE'
    )
    return(invisible(old_path))
  }

  set_Renviron(path, old_path, .Renviron_path)

  create_projects_folder(path)

  setup_messages(path, old_path)

  invisible(path)
}



p_path_from_explicit_renviron <- function(path) {
  if (fs::file_exists(path)) {
    readRenviron(path)
    Sys.getenv("PROJECTS_FOLDER_PATH")
  } else {
    ""
  }
}



set_Renviron <- function(path, old_path, .Renviron_path) {

  if (!any(c("", path) == old_path)) {
    user_prompt(
      msg   = paste0("\nAre you sure you want to abandon the old projects ",
                     "folder at\n", old_path, "\n\nand create a new one at\n",
                     path, "\n?\n\nThis will change the .Renviron file at\n",
                     .Renviron_path, "\nso that its PROJECTS_FOLDER_PATH ",
                     "line will be:\nPROJECTS_FOLDER_PATH='", path, "'",
                     "\n\nContinue? (y/n)"),
      n_msg = paste0("\nProjects folder remains at\n", old_path))
  }

  Renviron_entries <- paste0("PROJECTS_FOLDER_PATH='", path, "'")

  # If a home .Renviron file already exists, it is overwritten with its original
  # contents, minus any old values of PROJECTS_FOLDER_PATH, plus the new value
  # of PROJECTS_FOLDER_PATH (i.e., the user-specified path, which could
  # actually be the same as the old value).
  if (fs::file_exists(.Renviron_path)) {

    Renviron_entries <-
      c(grep(pattern = "^PROJECTS_FOLDER_PATH",
             x       = readr::read_lines(.Renviron_path),
             value   = TRUE,
             invert  = TRUE),
        # Existing home .Renviron file minus any old entries of
        # PROJECTS_FOLDER_PATH

        Renviron_entries)
  }

  readr::write_lines(Renviron_entries, path = .Renviron_path)
  readRenviron(.Renviron_path)
}


create_projects_folder <- function(path) {
  fs::dir_create(fs::path(path, c(".metadata", ".templates")))
  restore_templates(path)
  restore_metadata(path)
}


restore_templates <- function(path) {
  purrr::walk2(
    .x = c("01_protocol.Rmd", "STROBE_protocol.Rmd", "CONSORT_protocol.Rmd",
           "02_datawork.Rmd", "03_analysis.Rmd", "04_report.Rmd",
           "style.css", "pXXXX.Rproj"),
    .y = list(STROBE_template, STROBE_template, CONSORT_template,
              datawork_template, analysis_template, report_template,
              css_template, Rproj_template),
    .f = function(template_name, template_vector) {
      if (!fs::file_exists(fs::path(path, ".templates", template_name))) {
        readr::write_lines(template_vector,
                           fs::path(path, ".templates", template_name))
      }
    })
}

#' @importFrom tibble tibble
restore_metadata <- function(path) {
  purrr::walk2(
    .x = c("projects", "authors", "affiliations",
           "project_author_assoc", "author_affiliation_assoc"),
    .y = list(
      # projects
      tibble(
        id            = integer(),
        title         = character(),
        short_title   = character(),
        current_owner = new_projects_author(),
        status        = character(),
        deadline_type = character(),
        deadline      = as.POSIXct(character()),
        stage         = new_projects_stage(),
        path          = character(),
        corresp_auth  = new_projects_author(),
        creator       = new_projects_author()
      ),
      # authors
      tibble(
        id          = integer(),
        last_name   = character(),
        given_names = character(),
        title       = character(),
        degree      = character(),
        email       = character(),
        phone       = character()
      ),
      # affiliations
      tibble(
        id               = integer(),
        department_name  = character(),
        institution_name = character(),
        address          = character()),
      # project_author_assoc
      tibble(id1 = integer(), id2 = integer()),
      # author_affiliation_assoc
      tibble(id1 = integer(), id2 = integer())),
    .f =
      function(rds_name, tibble) {
        rds_path <- make_rds_path(rds_name, path)
        if (fs::file_exists(rds_path)) {
          tibble <- rbind(readRDS(rds_path), tibble)
        }
        write_metadata(table = tibble, table_path = rds_path)
      }
  )
}


setup_messages <- function(path, old_path) {
  if (old_path == "") {
    message('"projects" folder created at\n', path,
            '\n\nAdd affiliations with new_affiliation(), then add authors ',
            'with new_author(),\nthen create projects with new_project()')
  }
  else if (old_path == path) {
    message('"projects" folder restored at\n', path)
  }
  else {
    message('"projects" folder is now at\n', path,
            '\n\nThe "projects" folder at\n', old_path, '\nhas been abandoned.')
  }
}


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
#'   \emph{.templates}, which are hidden by default on some operating systems.
#'
#'   The \emph{.metadata} folder and its contents should \strong{never} be
#'   manually moved or modified.
#'
#'   The \emph{.templates} folder is where template project files and folders
#'   should be stored. When this function is successfully run, the default
#'   projects folder template is created (as "default_folder") alongside a few
#'   other template files. When a new project is created,
#'   \code{\link{new_project}()} looks here for the folder named by its
#'   \code{template_folder} argument (\code{"default_folder"} by default), and
#'   this folder is copied into the \link[=projects_folder]{projects folder}
#'   (with name specified by the \code{folder_name} argument) as the new project
#'   folder. Users are able and encouraged to customize the
#'   \code{default_folder} to suit their research needs, and may even create
#'   multiple project folder templates for different situations.
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
#' @param path The file path of the \strong{directory inside of which} the user
#'   would like the projects folder to be created. Do not include the name of
#'   the projects folder itself (i.e., the value of the argument
#'   \code{folder_name} below).
#' @param folder_name The name of the projects folder that will be created in
#'   the directory specified by the argument \code{path} above. Defaults to
#'   \code{"projects"}.
#' @param overwrite Logical indicating whether or not to abandon any previously
#'   stored projects folders stored in the system.
#' @param make_directories Logical indicating whether or not the function should
#'   write any directories specified in the \code{path} argument that don't
#'   already exist.
#' @param .Renviron_path The full file path of the .Renviron file where the user
#'   would like to store the \code{\link{projects_folder}()} path. Default is
#'   the home .Renviron file. If the file doesn't exist it will be created.
#'
#' @examples
#' #############################################################################
#' # Setup
#' # Any existing "projects" folder is left totally untouched,
#' # and the user's home directory and .Renviron file are also left untouched.
#' old_home  <- Sys.getenv("HOME")
#' old_ppath <- Sys.getenv("PROJECTS_FOLDER_PATH")
#' temp_dir <- tempfile("dir")
#' dir.create(temp_dir)
#' Sys.setenv(HOME = temp_dir)
#' Sys.unsetenv("PROJECTS_FOLDER_PATH")
#' #############################################################################
#'
#' # Creating the projects folder
#' setup_projects(path = temp_dir)
#'
#' # Viewing the projects folder path:
#' path1 <- projects_folder()
#'
#' # Viewing the contents of the projects folder:
#' list.files(path1, full.names = TRUE, recursive = TRUE,  all.files = TRUE)
#'
#' # Create an arbitrary subfolder in temp_dir:
#' subfolder_path <- file.path(temp_dir, "test")
#' dir.create(subfolder_path)
#'
#'
#' # Wrapped in if (interactive()) because it requires user input
#' if (interactive()) {
#'   # The function won't let the user abandon the old projects folder...
#'   setup_projects(path = subfolder_path)
#'
#'   # ...unless overwrite = TRUE
#'   setup_projects(path = file.path(temp_dir, "test"), overwrite = TRUE)
#'
#'   # Even then, only the stored location of the projects folder is overwritten.
#'   # The old projects folder still exists:
#'   list.files(path1, full.names = TRUE, recursive = TRUE,  all.files = TRUE)
#'
#'   # Giving the "projects" folder a different name:
#'   setup_projects(path = temp_dir, folder_name = "studies", overwrite = TRUE)
#' }
#'
#' #############################################################################
#' # Cleanup
#' # (or, the user can just restart R)
#' Sys.setenv(HOME = old_home, PROJECTS_FOLDER_PATH = old_ppath)
#' #############################################################################
#' @return The project folder's path, invisibly.
#'
#' @seealso \code{\link{new_project}()} for information on templates
#'
#'   \link{Startup} for more information on how \emph{.Renviron} files work.
#'
#' @export
setup_projects <- function(path,
                           folder_name      = "projects",
                           overwrite        = FALSE,
                           make_directories = FALSE,
                           .Renviron_path   =
                             file.path(Sys.getenv("HOME"), ".Renviron")) {
  folder_name <-
    validate_single_string(folder_name, na.ok = FALSE, zero.chars.ok = FALSE)

  if (folder_name != fs::path_file(folder_name)) {
    stop("folder_name must be a single string, not a file path")
  }

  path     <- path %>%
    validate_directory(
      p_path           = NULL,
      make_directories = make_directories
    ) %>%
    fs::path(folder_name)

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



p_path_from_explicit_renviron <- function(.Renviron_path) {
  if (fs::file_exists(.Renviron_path)) {
    readRenviron(.Renviron_path)
    Sys.getenv("PROJECTS_FOLDER_PATH")
  } else {
    ""
  }
}



set_Renviron <- function(projects_folder_path, old_path, .Renviron_path) {

  if (old_path != projects_folder_path && old_path != "") {
    user_prompt(
      msg   =
        paste0(
          "\nAre you sure you want to abandon the old projects ",
          "folder at\n", old_path, "\n\nand create a new one at\n",
          projects_folder_path,
          "\n?\n\nThis will change the .Renviron file at\n",
          .Renviron_path,
          "\nso that its PROJECTS_FOLDER_PATH ",
          "line will be:\nPROJECTS_FOLDER_PATH='", projects_folder_path, "'",
          "\n\nContinue? (y/n)"
        ),
      n_msg = paste0("\nProjects folder remains at\n", old_path)
    )
  }

  # If a home .Renviron file already exists, it is overwritten with its original
  # contents, minus any old values of PROJECTS_FOLDER_PATH, plus the new value
  # of PROJECTS_FOLDER_PATH (i.e., the user-specified path, which could
  # actually be the same as the old value).
  Renviron_entries <-
    if (fs::file_exists(.Renviron_path)) {

      c(
        # Existing home .Renviron file minus any old entries of
        # PROJECTS_FOLDER_PATH
        grep(
          pattern = "^PROJECTS_FOLDER_PATH",
          x       = readr::read_lines(.Renviron_path),
          value   = TRUE,
          invert  = TRUE
        ),

        paste0("PROJECTS_FOLDER_PATH='", projects_folder_path, "'")
      )
    } else {
      paste0("PROJECTS_FOLDER_PATH='", projects_folder_path, "'")
    }

  readr::write_lines(Renviron_entries, path = .Renviron_path)
  readRenviron(.Renviron_path)
}



create_projects_folder <- function(projects_folder_path) {
  fs::dir_create(fs::path(projects_folder_path, ".metadata"))
  fs::dir_create(
    fs::path(
      projects_folder_path,
      ".templates/default_folder",
      c("data", "data_raw", "progs", "figures", "manuscript")
    )
  )
  restore_templates(projects_folder_path)
  restore_metadata(projects_folder_path)
}


restore_templates <- function(projects_folder_path) {
  purrr::pwalk(
    .l =
      list(
        template_name =
          c(
            "CONSORT_protocol.Rmd",
            "STROBE_protocol.Rmd",
            "pXXXX.Rproj",
            "01_protocol.Rmd",
            "02_datawork.Rmd",
            "03_analysis.Rmd",
            "04_report.Rmd",
            "style.css",
            "styles.docx",
            "citations.bib"
          ),
        template_source =
          c(
            "CONSORT_protocol.Rmd",
            "STROBE_protocol.Rmd",
            "pXXXX.Rproj",
            "STROBE_protocol.Rmd",
            "02_datawork.Rmd",
            "03_analysis.Rmd",
            "04_report.Rmd",
            "style.css",
            "styles.docx",
            "citations.bib"
          ),
        subfolder =
          c(
            "",
            "",
            "default_folder",
            "default_folder/progs",
            "default_folder/progs",
            "default_folder/progs",
            "default_folder/progs",
            "default_folder/progs",
            "default_folder/progs",
            "default_folder/progs"
          )
      ),
    .f =
      function(template_name, template_source, subfolder) {

        template_path <-
          fs::path(projects_folder_path, ".templates", subfolder, template_name)

        if (!fs::file_exists(template_path)) {

          template_source <-
            system.file(
              "templates",
              template_source,
              package = "projects",
              mustWork = TRUE
            )

          fs::file_copy(template_source, template_path)
        }
      }
  )
}



restore_metadata <- function(path) {
  purrr::walk2(
    .x = c("projects", "authors", "affiliations",
           "project_author_assoc", "author_affiliation_assoc"),
    .y = list(
      # projects
      tibble::tibble(
        id            = integer(),
        title         = character(),
        short_title   = character(),
        current_owner = new_projects_author(),
        status        = character(),
        deadline_type = character(),
        deadline      = lubridate::as_datetime(character()),
        stage         = new_projects_stage(),
        path          = character(),
        corresp_auth  = new_projects_author(),
        creator       = new_projects_author()
      ),
      # authors
      tibble::tibble(
        id          = integer(),
        last_name   = character(),
        given_names = character(),
        title       = character(),
        degree      = character(),
        email       = character(),
        phone       = character()
      ),
      # affiliations
      tibble::tibble(
        id               = integer(),
        department_name  = character(),
        institution_name = character(),
        address          = character()),
      # project_author_assoc
      tibble::tibble(id1 = integer(), id2 = integer()),
      # author_affiliation_assoc
      tibble::tibble(id1 = integer(), id2 = integer())),
    .f =
      function(rds_name, tibble) {
        rds_path <- make_rds_path(rds_name, path)
        if (fs::file_exists(rds_path)) {
          tibble <- rbind(readRDS(rds_path), tibble)
        }
        readr::write_rds(x = tibble, path = rds_path)
      }
  )
}


setup_messages <- function(projects_folder_path, old_path) {
  if (old_path == "") {
    message(
      'projects folder created at\n', projects_folder_path,
      '\n\nAdd affiliations with new_affiliation(),',
      '\nthen add authors with new_author(),',
      '\nthen create projects with new_project()'
    )
  }
  else if (old_path == projects_folder_path) {
    message('projects folder restored at\n', projects_folder_path)
  }
  else {
    message(
      'projects folder is now at\n', projects_folder_path,
      '\n\nThe projects folder at\n', old_path, '\nhas been abandoned.'
    )
  }
}

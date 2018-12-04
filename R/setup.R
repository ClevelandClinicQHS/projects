#' Set up the projects folder
#'
#' Creates or restores the projects folder at the user-specified path.
#'
#' The \code{projects} package remembers where the
#' \code{\link{projects_folder}()} is located by storing its file path in the
#' \strong{home} \link[base]{.Renviron} file. The entry is named
#' \code{PROJECTS_FOLDER_PATH}. See \link[base]{Startup} for more details.
#'
#' @section Default contents: The \code{\link{projects_folder}} automatically
#'   contains the subdirectories \emph{.metadata} and \emph{.template}, which
#'   are hidden by default on some operating systems.
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
#'   If the user has a pre-existing \link{projects_folder} and runs this
#'   command with the pre-existing \link{projects_folder}'s path, nothing
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
#'   already exist.
#'
#' @examples
#' setup_projects(tempdir())
#'
#' @seealso \code{\link{new_project}()} for information on templates
#'
#'   \link[base]{Startup} for more information on how \emph{.Renviron} files
#'   work
#'
#' @importFrom tibble tibble
#' @export
setup_projects <- function(path, overwrite = FALSE, make_directories = FALSE) {

  path <- validate_directory(path             = path,
                             p_path           = NULL,
                             make_directories = make_directories)

  path <- fs::path(path, "projects")

  old_path           <- Sys.getenv("PROJECTS_FOLDER_PATH")
  home_Renviron_path <- fs::path(Sys.getenv("HOME"), ".Renviron")

  # If overwite = TRUE, function will run no matter what, overwriting any
  # pre-existing value of PROJECTS_FOLDER_PATH in the home .Renviron file.
  #
  # If overwrite = FALSE, function will still run UNLESS a
  # PROJECTS_FOLDER_PATH value already exists and does not match up with the
  # user-specified path.
  if(!overwrite && old_path != "" && old_path != path) {
    stop('An .Renviron file (probably at ', home_Renviron_path,
         ') indicates that a "projects" folder already exists at ',
         old_path, '. Rerun with that path OR set overwrite = TRUE')
  }

  if(!(old_path %in% c("", path))) {
    user_prompt(
      msg   = paste0("\nAre you sure you want to abandon the old projects ",
                     "folder at\n", old_path, "\n\nand create a new one at\n",
                     path, "\n\n? (y/n)"),
      n_msg = paste0("\nProjects folder remains at\n", old_path))
  }

  Sys.setenv(PROJECTS_FOLDER_PATH = path)

  readRenviron(home_Renviron_path)

  fs::dir_create(fs::path(path, c(".metadata", ".templates")))

  purrr::walk2(
    .x = c("01_protocol.Rmd", "STROBE_protocol.Rmd", "CONSORT_protocol.Rmd",
           "02_datawork.Rmd", "03_analysis.Rmd", "04_report.Rmd",
           "style.css", "pXXXX.Rproj"),
    .y = list(STROBE_template, STROBE_template, CONSORT_template,
              datawork_template, analysis_template, report_template,
              css_template, Rproj_template),
    .f = function(template_name, template_vector) {
      if(!fs::file_exists(fs::path(path, ".templates", template_name))) {
        readr::write_lines(template_vector,
                           fs::path(path, ".templates", template_name))
      }
    })

  purrr::walk2(
    .x = c("projects", "authors", "affiliations",
           "project_author_assoc", "author_affiliation_assoc"),
    .y = list(
           # projects
           tibble(
             id            = integer(),            title         = character(),
             short_title   = character(),          current_owner = integer(),
             status        = character(),          deadline_type = character(),
             deadline      = as.Date(character()),

             stage =
               factor(
                 levels = c("1: design", "2: data collection", "3: analysis",
                            "4: manuscript", "5: under review", "6: accepted")),

             path          = character(),          corresp_auth  = integer(),
             creator       = character()),
           # authors
           tibble(id          = integer(),   last_name = character(),
                  given_names = character(), title     = character(),
                  degree      = character(), email     = character(),
                  phone       = character()),
           # affiliations
           tibble(id               = integer(),   department_name= character(),
                  institution_name = character(), address        = character()),
           # project_author_assoc
           tibble(id1 = integer(), id2 = integer()),
           # author_affiliation_assoc
           tibble(id1 = integer(), id2 = integer())),
    .f =
      function(rds_name, tibble) {
        rds_path <- make_rds_path(rds_name, path)
        if(fs::file_exists(rds_path)) {
          tibble <- dplyr::bind_rows(readRDS(rds_path), tibble)
        }
        saveRDS(object = tibble, file = rds_path)
      }
  )

  if(old_path == "") {
    message('"projects" folder created at\n', path,
            '\n\nAdd affiliations with new_affiliation(), then add authors ',
            'with new_author(),\nthen create projects with new_project()')
  }
  else if(old_path == path) {
    message('"projects" folder restored at\n', path)
  }
  else {
    message('"projects" folder is now at\n', path,
            '\n\nThe "projects" folder at\n', old_path, '\nhas been abandoned.')
  }
}

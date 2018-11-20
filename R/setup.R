#' Set up the projects folder
#'
#' Creates or restores the projects folder at the user-specified path.
#'
#' If \code{overwrite = TRUE}, the function will run no matter what. Use with
#' caution.
#'
#' If the user has a pre-existing projects folder and runs this command with the
#' pre-existing projects folder's path, nothing will be deleted.
#'
#' Therefore, if the user "broke" the projects folder (e.g., by deleting
#' metadata; by changing the "PROJECTS_FOLDER_PATH" line in the .Renviron file),
#' the user can "fix" the projects folder by running this command with the
#' folder's actual file path.
#'
#' @param path The full file path where the user would like a directory called
#'   "projects" to be created, wherein all \code{projects} and their data will
#'   dwell.
#' @param overwrite Logical indicating whether or not to abandon any previously
#'   stored projects folders stored in the system.
#' @param make_directories Logical indicating whether or not the function should
#'   write any directories specified in the \code{path} argument that don't
#'   already exist.
#'
#' @aliases setup_projects()
#' @importFrom tibble tibble
#' @export
setup_projects <- function(path, overwrite = FALSE, make_directories = FALSE) {

  path <- validate_directory(path             = path,
                             p_path           = NULL,
                             make_directories = make_directories)

  path <- fs::path(path, "projects")

  old_path           <- Sys.getenv("PROJECTS_FOLDER_PATH")
  home_Renviron_path <- fs::path(Sys.getenv("HOME"), ".Renviron")

  # If overwite == TRUE, function will run no matter what, overwriting any
  # pre-existing value of PROJECTS_FOLDER_PATH in the home .Renviron file.
  #
  # If overwrite == FALSE, function will still run UNLESS a
  # PROJECTS_FOLDER_PATH value already exists and does not match up with the
  # user-specified path.
  if(!overwrite && old_path != "" && old_path != path) {
    stop('An .Renviron file (probably at ', home_Renviron_path,
         ') indicates that a "projects" folder already exists at ',
         old_path, '. Rerun with that path OR set overwrite = TRUE')
  }

  home_Renviron_file <- paste0("PROJECTS_FOLDER_PATH='", path, "'")

  # If a home .Renviron file already exists, it is overwritten with its original
  # contents, minus any old values of PROJECTS_FOLDER_PATH, plus the new value
  # of PROJECTS_FOLDER_PATH (i.e., the user-specified path, which could
  # actually be the same as the old value).
  if(fs::file_exists(home_Renviron_path)) {
    old_home_Renviron  <- readr::read_lines(home_Renviron_path)
    home_Renviron_file <-
      append(
        old_home_Renviron[!grepl("PROJECTS_FOLDER_PATH", old_home_Renviron)],
        # Contents of the old .Renviron file, excluding any pre-existing
        # PROJECTS_FOLDER_PATH value

        home_Renviron_file)
  }

  user_prompt(
    msg =
      dplyr::case_when(
        old_path == ""   ~ paste0("\nAre you sure you want the main projects ",
                                  "folder to have the file path\n", path,
                                  "\n\n? (y/n)"),
        old_path == path ~ paste0("\nAre you sure you want to restore the ",
                                  "main projects folder at the file path\n",
                                  path, "\n\n? (no projects, authors, ",
                                  "affiliations, or templates will be deleted)",
                                  "\n\n(y/n)"),
        TRUE             ~ paste0("\nAre you sure you want to abandon the old ",
                                  "projects folder at\n", old_path, "\n\nand ",
                                  "create a new one at\n", path,
                                  "\n\n? (y/n)")),
    n_msg = paste0("Projects folder not created. No changes made."))

  readr::write_lines(home_Renviron_file, path = home_Renviron_path)

  readRenviron(home_Renviron_path)

  fs::dir_create(fs::path(path, c(".metadata", ".templates")))

  if(!fs::file_exists(fs::path(path, ".templates", "CONSORT_template.Rmd"))) {
    readr::write_lines(CONSORT_template,
                       fs::path(path, ".templates", "CONSORT_template.Rmd"))
  }

  if(!fs::file_exists(fs::path(path, ".templates", "STROBE_template.Rmd"))) {
    readr::write_lines(STROBE_template,
                       fs::path(path, ".templates", "STROBE_template.Rmd"))
  }

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

  message('"projects" folder created at\n', path,
          '\n\nAdd affiliations with new_affiliation(), then add authors with ',
          'new_author(),\nthen create projects with new_project()')
}

#' file management
#'
#' Tools for Organizing and Managing Project Files
#'
#' Projects can be moved (\code{move_project()}), copied
#' (\code{copy_project()}), deleted (\code{\link{delete_project}()}) or archived
#' (\code{archive_project}).
#'
#' The difference between \code{delete_project()} and \code{archive_project()}
#' is that the latter will just move the project to a directory called
#' \emph{archive}, located in the same parent directory as the project. This
#' directory gets created if it doesn't yet exist. Some functions that perform
#' actions on projects will exclude archived projects by default in order to
#' make it easier for the user to enter a nonambiguous string that will match an
#' active (i.e., non-archived) project.
#'
#' Projects can also be organized into groups. By default, all projects are
#' created within the main \code{\link{projects_folder}}. To create a project
#' group, which essentially is a subfolder of projects that sits within the main
#' \code{\link{projects_folder}} (or recursively within another project group's
#' folder), use \code{new_project_group()}.
#'
#' \code{open_project()} is a wrapper around
#' \code{\link[rstudioapi]{openProject}}, but the user only needs to know the
#' project's \code{id}, \code{title}, or \code{short_title} instead of the file
#' path of the project's \emph{.Rproj} file.
#'
#' @param path A valid path string. See the \code{path} argument in
#'   \code{\link{new_project}()} for details, the one difference being that
#'   there is no default (i.e., the user cannot leave \code{path} blank in these
#'   functions).
#' @param project Project \code{id} or unambiguous substring of the project name
#'   from the \code{\link{projects}()} tibble.
#' @param make_directories Logical. If the path represented by the \code{path}
#'   parameter does not exist, should the needed directories be created?
#' @param project_to_copy Project \code{id} or unambiguous substring of the
#'   project name corresponding to the project that is to be copied.
#' @param new_id Optional integer, ranging from 1 to 9999, used as the
#'   newly-created project ID. Must not already exist in
#'   \code{\link{projects}()$id}. If left blank, the lowest available \code{id}
#'   will be automatically used.
#' @param new_session Same as the \code{newSession} argument in
#'   \code{rstudio::\link[rstudioapi]{openProject}()}.
#' @param archived Logical indicating whether or not the function should
#'   consider archived projects when determining which project the user is
#'   referring to in the \code{project}/\code{project_to_copy} argument.
#'   \code{FALSE} by default.
#'
#'   See the \strong{Details} section of \code{\link{archive_project}()} for
#'   more information on the "archived" status of a project.
#'
#' @name file_management
#' @seealso \code{\link{new_project}()} and \code{\link{delete_project}()} for
#'   other functions that write and delete files
#'
#' @examples
#' # SETUP
#' old_path <- Sys.getenv("PROJECTS_FOLDER_PATH")
#' setup_projects(path = tempdir(), .Renviron_path = fs::path_temp(".Renviron"))
#' #############################################################################
#'
#' # setting up a simple project directory tree
#' new_project_group("kidney/clinical")
#' new_project_group("kidney/genomics")
#' new_project_group("prostate/clinical")
#' new_project_group("prostate/genomics")
#'
#' # Wrapped in if(interactive()) because it requires interactive console input
#' # and fails automated package checking and testing.
#' if(interactive()){
#'   new_project(title = "Sample Authorless Project", path = "kidney")
#'
#'   # Moving the project folder, then moving it again.
#'   move_project(project = 1, "kidney/genomics")
#'   move_project(project = "Sample Authorless Project", "prostate/clinical")
#'
#'   # Copying the project
#'   copy_project(project_to_copy = 1, "kidney/clinical")
#'
#'   # Archiving the copy of the project
#'   archive_project(2)
#'
#'   # Opens the project in same session
#'   open_project("Sample")
#'
#'   # Opens the project in a new session
#'   open_project(1, new_session = TRUE)
#' }
#' #############################################################################
#' # CLEANUP
#' Sys.setenv(PROJECTS_FOLDER_PATH = old_path)
#' fs::file_delete(c(fs::path_temp("projects"), fs::path_temp(".Renviron")))
#' @importFrom rlang .data
#' @export
new_project_group <- function(path) {

  p_path <- p_path_internal()

  path   <- validate_directory(path, p_path, make_directories = TRUE)

  if(fs::path_ext(path) != "") {
    stop("\nMust be a directory and not a file (i.e., must not have a file ",
         "extension, which is ", fs::path_ext(path), " in this case).")
  }

  if(fs::dir_exists(path)) {
    stop("\nDirectory already exists.")
  }

  fs::dir_create(path)

  message("\nThe following directory was created:\n", path)
}


#' @rdname file_management
#' @importFrom rlang .data
#' @export
move_project <- function(project, path, make_directories = FALSE,
                         archived = FALSE) {

  p_path          <- p_path_internal()

  projects_path   <- make_rds_path("projects", p_path)
  projects_tibble <- get_rds(projects_path)

  project         <- validate_entry(x          = project,
                                    what       = "project",
                                    rds_tibble = projects_tibble,
                                    max.length = 1,
                                    archived   = archived)

  path            <- validate_directory(path             = path,
                                        p_path           = p_path,
                                        make_directories = make_directories)

  project_row     <- dplyr::filter(projects_tibble, .data$id == project)

  print(project_row)

  if(fs::dir_exists(path)) {
    user_prompt(
      msg   = paste0("Are you sure you want to move this project folder so ",
                     "that its new file path is\n",
                     fs::path(path, fs::path_file(project_row$path)),
                     "\n? (y/n)"),
      n_msg = paste0('Move not completed. To move this project, try again ',
                     'and enter "y".')
    )
  }
  else {
    user_prompt(
      msg   = paste0("\nDirectory not found:\n", path,
                     "\n\nWould you like to create it and move the project ",
                     "folder there, so that its new file path will be\n",
                     fs::path(path, fs::path_file(project_row$path)),
                     "\n\n? (y/n)"),
      n_msg = paste0("\nMove not completed. To move this project, try again ",
                     'and enter "y"'))

    fs::dir_create(path)
  }

  fs::file_move(path = project_row$path, new_path = path)

  project_row$path <-
    fs::path(path, fs::path_file(project_row$path)) %>%
    unclass()

  do.call(what = change_table,
          args = c(list(action        = "edit",
                        rds_path      = projects_path,
                        rds_tibble    = projects_tibble),
                   as.list(project_row)))

  message("\nProject ", project, " moved so that its new path is\n",
          project_row$path)
}


#' @rdname file_management
#' @importFrom rlang .data
#' @export
copy_project <- function(project_to_copy,
                         path,
                         new_id           = NA,
                         make_directories = FALSE,
                         archived         = FALSE) {

  p_path           <- p_path_internal()

  pa_assoc_path    <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_tibble  <- get_rds(pa_assoc_path)

  projects_path    <- make_rds_path("projects", p_path)
  projects_tibble  <- get_rds(projects_path)

  project          <- validate_entry(x          = project_to_copy,
                                     what       = "project",
                                     rds_tibble = projects_tibble,
                                     max.length = 1,
                                     archived   = archived)

  path             <- validate_directory(path             = path,
                                         p_path           = p_path,
                                         make_directories = make_directories)

  project_row      <- dplyr::filter(projects_tibble, .data$id == project)

  old_name         <- make_project_name(project)

  old_path         <- project_row$path

  print(project_row)

  project_row$id   <- validate_new(id         = new_id,
                                   what       = "project",
                                   rds_tibble = projects_tibble)

  pXXXX_name       <- make_project_name(project_row$id)

  project_row$path <- fs::path(path, pXXXX_name) %>% unclass()

  if(fs::dir_exists(path)) {
    user_prompt(
      msg   = paste0("\nAre you sure you want to copy this project into the ",
                     "new directory\n", project_row$path, "\n\n? (y/n)"),
      n_msg = paste0('Copy not completed. To copy this project, try again ',
                     'and enter "y".')
    )
  }
  else {
    user_prompt(
      msg   = paste0("\nDirectory not found:\n", path,
                     "\n\nWould you like to create it and copy the above ",
                     "project there, so that its file path will be\n",
                     project_row$path, "\n\n? (y/n)"),
      n_msg = paste0("\nCopy not completed. To copy this project, try again ",
                     'and enter "y"'))

    fs::dir_create(path)
  }

  fs::dir_copy(path = old_path, new_path = project_row$path)

  do.call(what = change_table,
          args = c(list(action     = "new",
                        rds_path   = projects_path,
                        rds_tibble = projects_tibble),
                   as.list(project_row)))

  old_assoc <- dplyr::filter(pa_assoc_tibble, .data$id1 == project)

  if(nrow(old_assoc) > 0) {
    change_assoc(assoc_path = pa_assoc_path,
                 assoc_tibble = pa_assoc_tibble,
                 new = TRUE,
                 id1 = project_row$id,
                 id2 = old_assoc$id2)
  }

  message("\nProject ", project_row$id, " below is a copy of project ", project,
          " and is located at\n", project_row$path)
  print(project_row)

  new_proj_ls <- fs::dir_ls(project_row$path)
  Rproj_path  <- new_proj_ls[fs::path_ext(new_proj_ls) == "Rproj"]
  if(length(Rproj_path) == 1) {
    new_path <- fs::path(fs::path_dir(Rproj_path), pXXXX_name, ext = "Rproj")
    fs::file_move(path = Rproj_path, new_path = new_path)
    message("\nThe .Rproj file\n", Rproj_path, "\nwas renamed to\n", new_path)
  }
  else if(length(Rproj_path) == 0) {
    message("\nNo .Rproj file found in the new project folder.")
  }
  else {
    message("\nMultiple .Rproj files found in newly created directory.",
            " None have been renamed.")
  }

  message('\nBe sure to change all instances of \"', old_name, '\" to \"',
          pXXXX_name, '\" as desired\n(e.g., .bib files and references to ',
          'them in YAML headers).\n')
}




#' @rdname file_management
#' @importFrom rlang .data
#' @export
archive_project <- function(project) {

  p_path          <- p_path_internal()

  projects_path   <- make_rds_path("projects", p_path)
  projects_tibble <- get_rds(projects_path)

  project         <- validate_entry(x          = project,
                                    what       = "project",
                                    rds_tibble = projects_tibble,
                                    max.length = 1,
                                    archived   = FALSE)

  project_row     <- dplyr::filter(projects_tibble, .data$id == project)

  if(!fs::dir_exists(project_row$path)) {
    print(project_row)
    stop("The above project not found at\n", project_row$path,
         "\nRestore project folder to this location first.")
  }

  archive_folder <- fs::path(fs::path_dir(project_row$path), "archive")
  new_path       <- fs::path(archive_folder, fs::path_file(project_row$path))

  user_prompt(
    msg   = paste0("\nAre you sure you want to archive this project folder",
                   " so that its new file path is\n", new_path, "\n\n? (y/n)"),
    n_msg = paste0('\nArchiving not completed. To archive this project, try ',
                   'again and enter "y".'))

  if(!fs::file_exists(archive_folder)) {
    fs::dir_create(archive_folder)
  }
  fs::file_move(path = project_row$path, new_path = archive_folder)

  projects_tibble$path[projects_tibble$id == project]   <- new_path

  saveRDS(object = projects_tibble, file = projects_path)

  print(dplyr::filter(projects_tibble, .data$id == project))
  message("\nThe above project was archived and has the file path\n", new_path)
}


#' @rdname file_management
#' @export
#' @importFrom rlang .data
open_project <- function(project, new_session = FALSE, archived = FALSE) {

  projects_tibble <- get_rds(make_rds_path("projects"))

  project <- validate_entry(project,
                            what = "project",
                            rds_tibble = projects_tibble,
                            max.length = 1L,
                            archived   = archived)

  path <- dplyr::filter(projects_tibble, .data$id == project)$path

  path <- fs::path(path, make_project_name(project), ext = "Rproj")

  rstudioapi::openProject(path, new_session)
}

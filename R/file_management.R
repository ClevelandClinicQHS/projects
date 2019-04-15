#' file management
#'
#' Tools for Organizing and Managing Project Files
#'
#' Projects can be moved (\code{move_project()}), copied
#' (\code{copy_project()}), or archived (\code{archive_project}()).
#'
#' The difference between \code{delete_project()} and \code{archive_project()}
#' is that the latter will just move the project to a directory called
#' \emph{archive}, located in the same parent directory as the project. This
#' directory gets created if it doesn't yet exist. Most functions that perform
#' actions on projects will exclude archived projects by default in order to
#' make it easier for the user to enter a nonambiguous string that will match an
#' active (i.e., non-archived) project.
#'
#' Projects can also be organized into groups. By default, all projects are
#' created within the main \link[=projects_folder]{projects folder}. To create a
#' project group, which is essentially a subfolder of the main
#' \link[=projects_folder]{projects folder}, use \code{new_project_group()}.
#'
#' The folder name of the project copy created by \code{copy_project()} will be
#' \code{new_short_title} if the user supplies a value that is different from
#' the names of any existing directories at \code{path}. Otherwise, its folder
#' name will be taken from its \code{id} (i.e., "p\emph{XXXX}").
#'
#' \code{open_project()} is a wrapper around
#' \code{rstudioapi::\link[rstudioapi]{openProject}()}, but the user only needs
#' to know the project's \code{id}, \code{title}, or \code{short_title} instead
#' of the file path of the project's \emph{.Rproj} file. If there is no
#' \emph{.Rproj} file in the project's folder, the user has the option to
#' restore a default \emph{.Rproj} folder. If there are multiple \emph{.Rproj}
#' files, an error is thrown.
#'
#' @param path A valid path string.
#'
#'   For \code{copy_project()} only, if left blank, the preexisting project's
#'   directory is used. All other functions here require a valid path.
#'
#'   See the \code{path} argument in \code{\link{new_project}()} for details on
#'   valid paths.
#' @param project Project \code{id} or unambiguous substring of the project name
#'   from the \code{\link{projects}()} table
#' @param make_directories Logical. If the path represented by the \code{path}
#'   parameter does not exist, should the needed directories be created?
#' @param project_to_copy Project \code{id} or unambiguous substring of the
#'   project name corresponding to the project that is to be copied.
#' @param new_id Optional integer, ranging from 1 to 9999, used as the
#'   newly-created project \code{id}. Must not already exist in
#'   \code{\link{projects}()$id}. If left blank, the lowest available \code{id}
#'   will be automatically used.
#' @param new_short_title Optional character string that becomes the
#'   \code{short_title} of the project copy. It also becomes the project copy's
#'   folder name under normal circumstances (see \strong{Details}).
#' @param new_folder_name Character string of new name for project folder.
#'   Always processed with \code{fs::\link[fs]{path_sanitize}()}.
#' @param change_short_title Logical indicating whether or not the project's
#'   \code{short_title} should be changed to the value of
#'   \code{new_folder_name}. Defaults to \code{TRUE}.
#' @param new_session Same as the \code{newSession} argument in
#'   \code{rstudioapi::\link[rstudioapi]{openProject}()}.
#' @param archived Logical indicating whether or not the function should
#'   consider archived projects when determining which project the user is
#'   referring to in the \code{project}/\code{project_to_copy} argument.
#'   \code{FALSE} by default. See \strong{Details}.
#'
#' @name file_management
#' @seealso \code{\link{new_project}()} and \code{\link{delete_project}()} for
#'   other functions that write and delete files.
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
#'   # Renaming the folder of the copy of the project
#'   rename_folder(project = 2, "copy")
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

  p_path <- get_p_path()

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
rename_folder <- function(project,
                          new_folder_name,
                          change_short_title = TRUE,
                          archived           = FALSE) {

  p_path         <- get_p_path()

  projects_path  <- make_rds_path("projects", p_path)
  projects_table <- get_rds(projects_path)

  if (!archived) {
    projects_table <- remove_archived(projects_table)
  }

  project_row    <-
    validate_unique_entry(
      x     = project,
      table = projects_table,
      what  = "project"
    )

  new_folder_name <- fs::path_sanitize(new_folder_name)

  new_path        <- fs::path(fs::path_dir(project_row$path), new_folder_name)

  print(project_row)

  if(fs::dir_exists(new_path)) {
    stop("The directory\n", new_path, "\nalready exists.",
         "\nMove or delete it, or pick a different name.")
  }
  else {
    user_prompt(
      msg   = paste0("Are you sure you want to rename this project folder so ",
                     "that its new file path is\n", new_path, "\n? (y/n)"),
      n_msg = paste0('Renaming not completed. To rename this project, ',
                     'try again and enter "y".')
    )
  }

  fs::file_move(path = project_row$path, new_path = new_path)

  project_row$path <- unclass(new_path)

  if(change_short_title) {
    project_row$short_title <- new_folder_name
  }

  edit_metadata(
    table = projects_table,
    row_id = project_row$id,
    path = project_row$path,
    short_title = project_row$short_title,
    table_path = projects_path
  )

  message(
    "\nProject ", project_row$id,
    " renamed so that its new path is\n", project_row$path
  )

  invisible(project_row)
}



#' @rdname file_management
#' @importFrom rlang .data
#' @export
move_project <- function(project,
                         path,
                         make_directories = FALSE,
                         archived         = FALSE) {

  p_path          <- get_p_path()

  projects_path   <- make_rds_path("projects", p_path)
  projects_table  <- get_rds(projects_path)

  if (!archived) {
    projects_table <- remove_archived(projects_table)
  }

  project_row      <-
    validate_unique_entry(
      x     = project,
      table = projects_table,
      what  = "project"
    )

  path            <-
    validate_directory(
      path             = path,
      p_path           = p_path,
      make_directories = make_directories
    )

  print(project_row)

  if (fs::dir_exists(path)) {
    user_prompt(
      msg   = paste0("Are you sure you want to move this project folder so ",
                     "that its new file path is\n",
                     fs::path(path, fs::path_file(project_row$path)),
                     "\n? (y/n)"),
      n_msg = paste0('Move not completed. To move this project, try again ',
                     'and enter "y".')
    )
  } else {
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

  edit_metadata(
    table = projects_table,
    row_id = project_row$id,
    path = project_row$path,
    table_path = projects_path
  )

  message(
    "\nProject ", project_row$id,
    " moved so that its new path is\n", project_row$path
  )
}



#' @rdname file_management
#' @importFrom rlang .data
#' @export
copy_project <- function(project_to_copy,
                         path,
                         new_id           = NA,
                         new_short_title  = NA,
                         make_directories = FALSE,
                         archived         = FALSE) {

  p_path         <- get_p_path()

  pa_assoc_path  <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_table <- get_rds(pa_assoc_path)

  projects_path  <- make_rds_path("projects", p_path)
  projects_table <- get_rds(projects_path)

  project_row    <-
    validate_unique_entry(
      x     = project_to_copy,
      table = projects_table,
      what  = "project"
    )

  original_project_id <- project_row$id

  old_path            <- project_row$path

  old_folder          <- fs::path_file(old_path)

  if (missing(path)) {
    path         <- fs::path_dir(old_path)
  } else {
    path         <-
      validate_directory(
        path             = path,
        p_path           = p_path,
        make_directories = make_directories
      )
  }

  old_project      <- project_row

  project_row$id   <- validate_new(id         = new_id,
                                   what       = "project",
                                   rds_table = projects_table)

  if (!is.na(new_short_title)) {
    project_row$short_title <- new_short_title
  }

  if (is.na(new_short_title) || any(fs::dir_ls(path) == new_short_title)) {
    folder_name    <- make_project_name(project_row$id)
  } else {
    folder_name    <- make_project_name(new_short_title, short_title = TRUE)
  }

  project_row$path <- fs::path(path, folder_name) %>% unclass()

  print(old_project)

  if (fs::dir_exists(path)) {
    user_prompt(
      msg   = paste0("\nAre you sure you want to copy this project into the ",
                     "new directory\n", project_row$path, "\n\n? (y/n)"),
      n_msg = paste0('Copy not completed. To copy this project, try again ',
                     'and enter "y".')
    )
  } else {
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

  add_metadata(
    table = projects_table,
    new_row = project_row,
    table_path = projects_path
  )

  old_assoc <- dplyr::filter(pa_assoc_table, .data$id1 == original_project_id)

  if (nrow(old_assoc) > 0L) {

    add_assoc(
      assoc_table = pa_assoc_table,
      new_rows    = tibble::tibble(id1 = project_row$id, id2 = old_assoc$id2),
      assoc_path  = pa_assoc_path
    )
  }

  message(
    "\nProject ", project_row$id, " below is a copy of project ",
    original_project_id, " and is located at\n", project_row$path
  )
  print(project_row)

  new_proj_ls <- fs::dir_ls(project_row$path)
  Rproj_path  <- new_proj_ls[fs::path_ext(new_proj_ls) == "Rproj"]
  if (length(Rproj_path) == 1L) {
    new_path  <- fs::path(fs::path_dir(Rproj_path), folder_name, ext = "Rproj")
    fs::file_move(path = Rproj_path, new_path = new_path)
    message("\nThe .Rproj file\n", Rproj_path, "\nwas renamed to\n", new_path)
  } else if (length(Rproj_path) == 0L) {
    message("\nNo .Rproj file found in the new project folder.")
  } else {
    message("\nMultiple .Rproj files found in newly created directory.",
            " None have been renamed.")
  }

  message(
    '\nBe sure to change all instances of \"', old_folder,
    '\" to \"', folder_name,
    '\" as desired\n(e.g., .bib files and references to them in YAML headers).'
  )

  invisible(project_row)
}




#' @rdname file_management
#' @importFrom rlang .data
#' @export
archive_project <- function(project) {

  p_path          <- get_p_path()

  projects_path   <- make_rds_path("projects", p_path)
  projects_table  <- get_rds(projects_path) %>% remove_archived()

  project_row     <-
    validate_unique_entry(
      x     = project,
      table = projects_table,
      what  = "project"
    )

  if (!fs::dir_exists(project_row$path)) {
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

  if (!fs::file_exists(archive_folder)) {
    fs::dir_create(archive_folder)
  }
  fs::file_move(path = project_row$path, new_path = archive_folder)

  projects_table$path[projects_table$id == project_row$id]   <- new_path

  readr::write_rds(x = projects_table, path = projects_path)

  print(dplyr::filter(projects_table, .data$id == project_row$id))
  message("\nThe above project was archived and has the file path\n", new_path)
}



#' @rdname file_management
#' @export
#' @importFrom rlang .data
open_project <- function(project, new_session = FALSE, archived = FALSE) {

  project_row    <-
    validate_unique_entry(
      x     = project,
      table = projects_internal(archived = archived),
      what  = "project"
    )

  Rproj_path <-
    project_row$path %>%
    fs::dir_ls() %>%
    `[`(fs::path_ext(.) == "Rproj")

  if (length(Rproj_path) != 1) {

    if (length(Rproj_path) == 0) {

      user_prompt(
        msg   =
          paste0(
            "\nCannot open project ",
            project_row$id,
            " because there\n",
            "is no .Rproj file in\n",
            project_row$path,
            "\n\nRestore it with a default .Rproj file? (y/n)"
          ),
        n_msg =
          paste0('\nRestore a .Rproj file to the folder\n', project_row$path)
      )

      Rproj_path <-
        fs::path(
          project_row$path,
          make_project_name(project_row$id),
          ext = "Rproj"
        )
      readr::write_lines(Rproj_template, Rproj_path)

      user_prompt(
        msg   =
          paste0(
            ".Rproj file restored at\n",
            Rproj_path,
            "\n\nOpen this project? (y/n)"
          ),
        n_msg =
          paste0("\nProject not opened.")
      )
    } else {
      stop(
        "\nCannot open project ", project_row$id, " because there\n",
        "are multiple .Rproj files in\n", project_row$path,
        "\nNamely: ", paste(fs::path_file(Rproj_path), collapse = ", "),
        "\nMove or delete the extraneous ones."
      )
    }
  }

  rstudioapi::openProject(Rproj_path, new_session)
}

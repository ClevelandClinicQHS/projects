

#' Save R session information
#'
#' Creates a dated text file (.txt) containing the contents of
#' \code{sessioninfo::\link[sessioninfo]{session_info}()}.
#'
#' The date and time when this function was run is included in the resulting
#' .txt file's name and first line. This date and time is obtained from
#' \code{\link{Sys.time}()}.
#'
#' For the file name, hyphens (-) are removed from the date, spaces are replaced
#' with underscores (_), and colons (:) are replaced with a modifier letter
#' colon (U+A789).
#'
#' @param path_dir The full path of the directory where the session information
#'   text file shall be written. If it doesn't exist, it is written with
#'   \code{fs::\link[fs]{dir_create}()}.
#'
#' @return A list of two:
#'
#'   \code{$ time           :} the value of \code{\link{Sys.time}()} that the
#'   function used
#'
#'   \code{$ session_info() :} the value of
#'   \code{sessioninfo::\link[sessioninfo]{session_info}()} that the function
#'   used
#'
#' @export
save_session_info <- function(path_dir = here::here("progs", "session_info")) {

  if (!fs::file_exists(path_dir)) {
    fs::dir_create(path_dir)
  }

  time <- Sys.time()

  time_string <- as.character(time, usetz = TRUE)

  session_info <- sessioninfo::session_info()

  txt_lines <-
    utils::capture.output(
      cat(paste0("Run time: ", time_string, "\n\n")),
      session_info
    )

  readr::write_lines(
    txt_lines,
    fs::path(
      path_dir,
      paste0(
        "session_info_",
        stringr::str_replace_all(
          time_string,
          c("-" = "", " " = "_", ":" = "\uA789")
        )
      ),
      ext = "txt"
    )
  ) %>%
    cat(sep = "\n")

  invisible(
    list(
      time = time,
      session_info = session_info
    )
  )
}


#' Compress a project folder
#'
#' Creates a compressed file out of a user-specified project folder for sharing.
#'
#' Currently, this function uses \code{zip::\link[zip]{zipr}()}.
#'
#' @param project Project \code{id} or unambiguous substring of the project name
#'   from the \code{\link{projects}()} table.
#' @param zipfile Desired file path of the resulting compressed folder file,
#'   including the file's desired name and file extension. See the
#'   \code{zipfile} argument for the \code{zip::\link[zip]{zipr}()} function.
#' @param include_hidden Logical indicating whether or not to include hidden
#'   folders and files (e.g., those with names that begin with a period).
#'   Defaults to \code{FALSE}.
#' @param exclude Character vector of exact names of first-level subdirectories
#'   of the project folder to exclude from the resulting compressed folder file.
#'
#' @return The name of the created zip file, invisibly.
#'
#' @export
export_project <- function(project,
                           zipfile,
                           include_hidden = FALSE,
                           exclude = NULL) {

  exclude <- as.character(exclude)

  if (!all(nchar(exclude) > 0L)) {
    stop("Each element of exclude must have at least 1 character.")
  }

  project_dir <-
    validate_unique_entry(
      x     = project,
      table = projects_internal(),
      what  = "project"
    )$path

  files <-
    fs::dir_ls(project_dir, all = include_hidden) %>%
    `[`(!(fs::path_file(.) %in% exclude))

  zip::zipr(zipfile, files = files, recurse = TRUE)
}

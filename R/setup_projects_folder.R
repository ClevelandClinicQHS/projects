#' @export
setup_projects_folder <- function(path, overwrite = FALSE) {

  path <- fs::path_tidy(path)
  
  if(tolower(fs::path_file(path)) != "projects") {
    path <- fs::path(path, "projects")
  }
  
  #old_path           <- projects_path(check = FALSE)
  home_Renviron_path <- fs::path(Sys.getenv("HOME"), ".Renviron")
  
  # If overwite == TRUE, function will run no matter what, overwriting any
  # pre-existing value of PROJECTS_FOLDER_PATH in the home .Renviron file.
  #
  # If overwrite == FALSE, function will still run UNLESS a
  # PROJECTS_FOLDER_PATH value already exists and does not match up with the
  # user-specified path.
  if(!overwrite && projects_path != path && projects_path != "") {
    stop('An .Renviron file (probably at ', home_Renviron_path,
         ') indicates that a "projects" folder already exists at ',
         projects_path, '. Rerun with that path OR set overwrite = TRUE')
  }
  
  home_Renviron_file <- paste0("PROJECTS_FOLDER_PATH='", path, "'")

  # If a home .Renviron file already exists, it is overwritten with its original
  # contents, minus any old values of PROJECTS_FOLDER_PATH, plus the new value
  # of PROJECTS_FOLDER_PATH (i.e., the user-specified path, which could
  # actually be the same as the old value).
  if(fs::file_exists(home_Renviron_path)) {
    old_home_Renviron  <- readr::read_lines(home_Renviron_path)
    home_Renviron_file <-
      c(home_Renviron_file,

        # Contents of the old .Renviron file, excluding any pre-existing
        # PROJECTS_FOLDER_PATH value
        old_home_Renviron[!grepl("PROJECTS_FOLDER_PATH", old_home_Renviron)])
  }
  readr::write_lines(home_Renviron_file, path = home_Renviron_path)

  fs::dir_create(fs::path(path, c("archive", "templates")))
  
  fs::file_copy(
    path      = system.file("extdata", "pXXXX_protocol.docx",
                            package = "projects"),
    new_path  = fs::path(path, "templates", "pXXXX_protocol", ext = "docx"),
    overwrite = TRUE)

  project_list_path <- fs::path(path, "project_list", ext = "rds")
  
  if(!fs::file_exists(project_list_path)) {
    project_list <- tibble::tibble(number = integer(0), path = character(0))
    saveRDS(project_list, project_list_path)
  }
  
  # project_list_path <- fs::path(path, "project_list", ext = "csv")
  # readr::write_csv(data.frame(number = integer(0), path = character(0)),
  #                  path   = project_list_path,
  #                  append = fs::file_exists(project_list_path))

  readRenviron(home_Renviron_path)
  #requireNamespace("projects", quietly = TRUE)
  assign("projects_path", path, asNamespace("projects"))
  
  message('"projects" folder created at ', path)
}

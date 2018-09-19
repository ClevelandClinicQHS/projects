#' @export
setup_projects_folder <- function(path, overwrite = FALSE) {

  path <- path %>% fs::path_tidy()
  
  if(tolower(fs::path_file(path)) != "projects") {
    path <- fs::path(path, "projects")
  }
  
  old_path           <- projects_path()
  home_Renviron_path <- fs::path(Sys.getenv("HOME"), ".Renviron")
  
  # If overwite == TRUE, function will run no matter what, overwriting any
  # pre-existing value of PROJECTS_FOLDER_PATH in the home .Renviron file.
  #
  # If overwrite == FALSE, function will still run UNLESS a
  # PROJECTS_FOLDER_PATH value already exists and does not match up with the
  # user-specified path.
  if(!overwrite && old_path != path && old_path != "") {
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
      c(home_Renviron_file,

        # Contents of the old .Renviron file, excluding any pre-existing
        # PROJECTS_FOLDER_PATH value
        old_home_Renviron[!grepl("PROJECTS_FOLDER_PATH", old_home_Renviron)])
  }
  readr::write_lines(home_Renviron_file, path = home_Renviron_path)
  
  readRenviron(home_Renviron_path)

  p <- projects(path, rds_paths_only = TRUE)
  
  fs::dir_create(fs::path(path, c("archive", "templates")))
  
  fs::file_copy(
    path      = system.file("extdata", "pXXXX_protocol.docx",
                            package = "projects"),
    new_path  = fs::path(path, "templates", "pXXXX_protocol", ext = "docx"),
    overwrite = TRUE)

  if(isFALSE(fs::file_exists(p$list_path))) {
    tibble::tibble(number        = integer(),   title    = character(),
                   current_owner = character(), PI       = list(),
                   investigators = list(),      creator  = character(),
                   stage         = character(), path     = character(),
                   deadline_type = character(), deadline = as.Date(character()),
                   status        = character()) %>% 
      saveRDS(file = p$list_path)
  }
  
  if(isFALSE(fs::file_exists(p$authors_path))) {
    tibble::tibble(last_name = character(), given_names  = character(),
                   title     = character(), affiliations = list(),
                   degree    = character(), email        = character()) %>% 
      saveRDS(file = p$authors_path)
  }
  
  if(isFALSE(fs::file_exists(p$affiliations_path))) {
    tibble::tibble(id               = integer(),
                   department_name  = character(),
                   institution_name = character(),
                   address          = character()) %>% 
      saveRDS(file = p$affiliations_path)
  }
  
  message('"projects" folder created at ', path)
}

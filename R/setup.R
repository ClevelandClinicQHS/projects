#' @importFrom tibble tibble
#' @export
#'
setup_projects <- function(path, overwrite = FALSE) {

  path <- fs::path_tidy(path)

  if(tolower(fs::path_file(path)) != "projects") {
    path <- fs::path(path, "projects")
  }

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
           tibble(id          = integer(),         title         = character(),
                  short_title = character(),       current_owner = integer(),
                  creator     = integer(),         corresp_auth  = integer(),
                  stage       = character(),       deadline_type = character(),
                  deadline    = as.Date(character()),
                  status      = character(),       path          = character()),
           # authors
           tibble(id          = integer(),   last_name = character(),
                  given_names = character(), title     = character(),
                  degree      = character(), email     = character(),
                  phone       = character(), default   = logical()),
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

  message('"projects" folder created at\n', path, '\nAdd affiliations with ',
          'new_affiliation(), then add authors with new_author(), then create ',
          'projects with new_project()')

}

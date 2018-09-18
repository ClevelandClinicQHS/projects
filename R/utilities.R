make_project_name <- function(project_number) {
  paste0("p",
         stringr::str_pad(project_number, width = 4, side = "left", pad = "0"))
}

check_projects_path <- function(error = FALSE) {
  if(projects_path == "") {
    check_message <-
      '"projects" folder not found. Please run setup_projects_folder()'
    if(error) {
      stop(check_message)
    }
    return(check_message)
  }
  return(paste0('"projects" folder located at ', projects_path))
}
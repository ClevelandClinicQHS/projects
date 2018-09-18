make_project_name <- function(project_number) {
  paste0("p",
         stringr::str_pad(project_number, width = 4, side = "left", pad = "0"))
}



projects_path <- function(check = TRUE) {

  projects_path <- Sys.getenv("PROJECTS_FOLDER_PATH")

  if(check && projects_path == "") {
    stop('"projects" folder not found. Please run setup_projects_folder()')
  }
  else {
    return(projects_path)
  }
}

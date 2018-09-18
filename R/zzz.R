.onAttach <- function(libname, pkgname) {
  if(projects_path(check = FALSE) == "") {
    packageStartupMessage(
      paste0('"projects" folder not found. Please run setup_projects_folder()'))
  }
  else {
    packageStartupMessage(paste0('"projects" folder located at ',
                                 projects_path(check = FALSE)))
  }
}

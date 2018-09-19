
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(check_projects_path(error = FALSE))
}
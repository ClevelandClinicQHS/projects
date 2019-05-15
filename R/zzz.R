
.onAttach <- function(libname, pkgname) {

  p_path <- get_p_path(error = FALSE)

  packageStartupMessage('projects_folder() location:\n', p_path)
}

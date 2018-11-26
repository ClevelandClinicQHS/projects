
.onAttach <- function(libname, pkgname) {
  packageStartupMessage('projects_folder() location:\n',
                        p_path_internal(error = FALSE))
}

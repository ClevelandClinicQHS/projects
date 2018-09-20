
.onAttach <- function(libname, pkgname) {
  packageStartupMessage('"projects" folder location: ',
                        p_path_internal(error = FALSE))
}

.onAttach <- function(libname, pkgname) {

  p_path <- get_p_path(error = FALSE)

  packageStartupMessage('projects_folder() location:\n', p_path)

  # projects_path <- make_rds_path(rds_name = "projects", p_path = p_path)
  #
  # if (fs::file_exists(projects_path)) {
  #   if (!up_to_date(readRDS(projects_path))) {
  #     packageStartupMessage(
  #       "\nYour project metadata needs to be updated.\n",
  #       "Please run update_metadata() to continue using this package."
  #     )
  #   }
  # } else if (fs::is_absolute_path(p_path)) {
  #   packageStartupMessage(
  #     "\nProject metadata not found within the above folder.",
  #     "\nRestore it or rerun setup_projects()"
  #   )
  # }
}

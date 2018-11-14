#' @importFrom rlang .data
#' @export
archive_project <- function(project) {

  p_path          <- p_path_internal()

  projects_path   <- make_rds_path("projects", p_path)
  projects_tibble <- get_rds(projects_path)

  project         <- validate_entry(x          = project,
                                    what       = "project",
                                    max.length = 1,
                                    rds_tibble = projects_tibble)

  project_row     <- dplyr::filter(projects_tibble, .data$id == project)

  if(!fs::dir_exists(project_row$path)) {
    print(project_row)
    stop("The above project not found at\n", project_row$path,
         "\nRestore project folder to this location first.")
  }

  archive_folder <- fs::path(p_path, "archive")
  new_path       <- fs::path(archive_folder, fs::path_file(project_row$path))

  user_prompt(
    msg   = paste0("\nAre you sure you want to archive this project folder",
                   " so that its new file path is\n", new_path, "\n\n? (y/n)"),
    n_msg = paste0('\nArchiving not completed. To archive this project, try ',
                   'again and enter "y".'))

  if(!fs::file_exists(archive_folder)) {
    fs::dir_create(archive_folder)
  }
  fs::file_move(path = project_row$path, new_path = archive_folder)

  projects_tibble$status[projects_tibble$id == project] <- "archived"
  projects_tibble$path[projects_tibble$id == project]   <- new_path

  saveRDS(object = projects_tibble, file = projects_path)

  print(dplyr::filter(projects_tibble, .data$id == project))
  message("\nThe above project was archived and has the file path\n", new_path)
}






clear_special_author <- function(author, projects_path, projects_tibble) {

  is.na(projects_tibble[c("current_owner", "creator", "corresp_auth")]) <-
    projects_tibble[c("current_owner", "creator", "corresp_auth")] == author

  saveRDS(object = projects_tibble, file = projects_path)

  return(projects_tibble)
}





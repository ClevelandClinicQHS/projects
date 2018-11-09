
#' @export
delete_project <- function(project) {
  
  p_path          <- p_path_internal()
  
  projects_path   <- make_rds_path("projects", p_path)
  projects_tibble <- get_rds(projects_path)
  
  project         <- validate_entry(project,
                                    what       = "project",
                                    max.length = 1,
                                    rds_tibble = projects_tibble)
  
  pa_assoc_path   <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_tibble <- get_rds(pa_assoc_path)
  
  project_row     <- dplyr::filter(projects_tibble, .data$id == project)
  
  print(project_row)
  
  if(!fs::dir_exists(project_row$path)) {
    user_prompt(
      msg   = paste0("Project folder not found at\n", project_row$path,
                     "\nDelete only its metadata? (y/n)"),
      # y_msg = paste0(""),
      n_msg = paste0("Deletion not completed. Restore folder to ",
                     project_row$path, ' or rerun this command, inputting "y" ',
                     'instead of "n" when asked whether or not to continue.'))
  }
  else {
    user_prompt(
      msg   = paste0("Are you sure you want to delete the above project and ",
                     "its entire folder, which is located at\n",
                     project_row$path, "\n? (y/n)"),
      n_msg = paste0('Deletion not completed. If deletion is desired, try ',
                     'again and enter "y".'))
    fs::dir_delete(path = project_row$path)
  }
  
  change_table(action     = "delete",        rds_path   = projects_path,
               rds_tibble = projects_tibble, id         = project)
  
  change_assoc(assoc_path   = pa_assoc_path, assoc_tibble = pa_assoc_tibble,
               new          = FALSE,         id1          = project)
  
  print(project_row)
  message("The above project was deleted.")
  return(invisible(project_row))
}



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
    msg   = paste0("Are you sure you want to archive this project folder",
                   " so that its new file path is\n", new_path, "\n? (y/n)"),
    n_msg = paste0('Archiving not completed. To archive this project, try ',
                   'again and enter "y".'))
  
  fs::file_move(path = project_row$path, new_path = archive_folder)
  
  projects_tibble$status[projects_tibble$id == project] <- "archived"
  
  saveRDS(object = projects_tibble, file = projects_path)
  
  print(dplyr::filter(projects_tibble, .data$id == project))
  message("The above project was archived and has the file path\n", new_path)
}

#' @export
move_project <- function(project, path, make_directories = FALSE) {
  
  
  
  path <- fs::path(p_path, path) %>% unclass()
  
  print(project_row)
  
  if(fs::dir_exists(path)) {
    user_prompt(
      msg   = paste0("Are you sure you want to move this project folder so ",
                     "that its new file path is\n",
                     fs::path(path, fs::path_file(project_row$path)),
                     "\n? (y/n)"),
      n_msg = paste0('Move not completed. To move this project, try again ',
                     'and enter "y".')
    )
  } 
  else {
    if(make_directories) {
      user_prompt(
        msg   = paste0("Directory not found at ", project_row$path,
                       ".\nWould you like to create it and move the project ",
                       "folder there, so that its new file path will be\n",
                       fs::path(path, fs::path_file(project_row$path)),
                       "\n? (y/n)"),
        # y_msg = paste0(""),
        n_msg = paste0("Move not completed. To move this project, try again ",
                       'and enter "y"'))
    }
    else {
      stop("Target directory not found at\n", path, "\nCreate it with ",
           "setup_projects_folder() or try this function again with ",
           "make_directories = TRUE")
    }
  }
  
  fs::file_move(path = project_row$path, new_path = path)
}




#' @importFrom rlang .data
#' @export
delete_author <- function(author) {
  
  p_path          <- p_path_internal()
  
  projects_path   <- make_rds_path("projects", p_path)
  projects_tibble <- get_rds(projects_path)
  
  authors_path    <- make_rds_path("authors", p_path)
  authors_tibble  <- get_rds(authors_path)
  
  author          <- validate_entry(author,
                                    what       = "author",
                                    max.length = 1,
                                    rds_tibble = authors_tibble)
  
  pa_assoc_path   <- make_rds_path("project_author_assoc", p_path)
  pa_assoc_tibble <- get_rds(pa_assoc_path)
  
  aa_assoc_path   <- make_rds_path("author_affiliation_assoc", p_path)
  aa_assoc_tibble <- get_rds(aa_assoc_path)
  
  author_row      <- dplyr::filter(authors_tibble, .data$id == author)
  
  print(author_row)
  user_prompt(
    msg   = "Are you sure you want to delete the above author? (y/n)",
    n_msg = paste0('Deletion not completed. If deletion is desired, ',
                   'input "y" next time.'))
  
  change_table(action     = "delete",
               rds_path   = authors_path,
               rds_tibble = authors_tibble,
               id         = author)
  
  clear_special_author(author          = author,
                       projects_path   = projects_path,
                       projects_tibble = projects_tibble)
  
  change_assoc(assoc_path   = pa_assoc_path,
               assoc_tibble = pa_assoc_tibble,
               new          = FALSE,
               id2          = author)
  
  change_assoc(assoc_path   = aa_assoc_path,
               assoc_tibble = aa_assoc_tibble,
               new          = FALSE,
               id1          = author)
  
  print(author_row)
  message("The above author was deleted.")
}



clear_special_author <- function(author, projects_path, projects_tibble) {
  
  is.na(projects_tibble[c("current_owner", "creator", "corresp_auth")]) <-
    projects_tibble[c("current_owner", "creator", "corresp_auth")] == author
  
  saveRDS(object = projects_tibble, path = projects_path)
  
  return(projects_tibble)
}



#' @export
delete_affiliation <- function(affiliation) {
  
  p_path              <- p_path_internal()
  
  affiliations_path   <- make_rds_path("affiliations", p_path)
  affiliations_tibble <- get_rds(affiliations_path)
  
  aa_assoc_path       <- make_rds_path("author_affiliation_assoc", p_path)
  aa_assoc_tibble     <- get_rds(aa_assoc_path)
  
  affiliation         <- validate_entry(affiliation,
                                        what       = "affiliation",
                                        max.length = 1,
                                        rds_tibble = affiliations_tibble)
  
  affiliation_row     <- dplyr::filter(affiliations_tibble,
                                       .data$id == affiliation)
  
  print(affiliation_row)
  user_prompt(
    msg   = "Are you sure you want to delete the above affiliation? (y/n)",
    n_msg = paste0('Deletion not completed. If deletion is desired, ',
                   'input "y" next time.'))
  
  change_table(action     = "delete",
               rds_path   = affiliations_path,
               rds_tibble = affiliations_tibble,
               id         = affiliation)
  
  change_assoc(assoc_path   = aa_assoc_path,
               assoc_tibble = aa_assoc_tibble,
               new          = FALSE,
               id2          = affiliation)
  
  print(affiliation_row)
  message("The above affiliation was deleted.")
}
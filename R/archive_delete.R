#' @export
delete_affiliation <- function(affiliation) {
  
  p_path              <- p_path_internal()
  
  affiliations_path   <- make_rds_path("affiliations", p_path)
  affiliations_tibble <- get_rds(affiliations_path)
  
  affiliation         <- validate_entry(affiliation,
                                        what       = "affiliation",
                                        max.length = 1,
                                        rds_tibble = affiliations_tibble)
  
  deleted_affiliation <- change_table(rds_name   = "affiliations",
                                      rds_path   = affiliations_path,
                                      rds_tibble = affiliations_tibble,
                                      action     = "delete",
                                      id         = affiliation)
  
  change_assoc(assoc_name = "author_affiliation_assoc",
               p_path     = p_path,
               new        = FALSE,
               id2        = affiliation)
  
  message("The following affiliation was deleted:")
  return(deleted_affiliation)
}

#' @export
delete_author <- function(author) {
  
  p_path         <- p_path_internal()
  
  authors_path   <- make_rds_path("authors", p_path)
  authors_tibble <- get_rds(authors_path)
  
  author         <- validate_entry(author,
                                   what       = "author",
                                   max.length = 1,
                                   rds_tibble = authors_tibble)
  
  deleted_author <- change_table(rds_name   = "authors",
                                 rds_path   = authors_path,
                                 rds_tibble = authors_tibble,
                                 action     = "delete",
                                 id         = author)
  
  change_assoc(assoc_name = "author_affiliation_assoc",
               p_path     = p_path,
               new        = FALSE,
               id1        = author)
  
  change_assoc(assoc_name = "project_PI_assoc",
               p_path     = p_path,
               new        = FALSE,
               id2        = author)
  
  change_assoc(assoc_name = "project_investigator_assoc",
               p_path     = p_path,
               new        = FALSE,
               id2        = author)
  
  message("The following author was deleted:")
  return(deleted_author)
}




#' @export
delete_project <- function(project, archived = FALSE) {
  
  p_path          <- p_path_internal()
  
  projects_path   <- make_rds_path("projects", p_path)
  projects_tibble <- get_rds(projects_path)
  
  project         <- validate_entry(project,
                                    what       = "project",
                                    max.length = 1,
                                    rds_tibble = projects_tibble)
  
  deleted_project <- change_table(rds_name   = "projects",
                                  rds_path   = projects_path,
                                  rds_tibble = projects_tibble,
                                  action     = "delete",
                                  id         = project)
  
  change_assoc(assoc_name = "project_PI_assoc",
               p_path     = p_path,
               new        = FALSE,
               id1        = project)
  
  change_assoc(assoc_name = "project_investigator_assoc",
               p_path     = p_path,
               new        = FALSE,
               id1        = project)
  
  pXXXX_path <- 
    project %>% 
    make_project_name() %>% 
    make_project_path(p_path) %>% 
    fs::path()
  
  if(archived) {
    pXXXX_path <- fs::path(pXXXX_path, "archived")
  }
  
  fs::dir_delete(path = pXXXX_path)
  
  message("The following ", ifelse(archived, "archived ", ""),
          "project was deleted:")
  return(deleted_project)
}



#' @export
archive_project <- function(project) {
  
  p_path <- p_path_internal()
  
  projects_path   <- make_rds_path("projects", p_path)
  projects_tibble <- get_rds(projects_path)
  
  project <- validate_entry(x = project,
                            what = "project",
                            max.length = 1,
                            rds_tibble = projects_tibble)
  
  archived_project <- change_table(rds_name      = "projects",
                                   p_path        = p_path,
                                   action        = "edit",
                                   id            = project,
                                   title         = NA,
                                   current_owner = NA,
                                   creator       = NA,
                                   stage         = NA,
                                   deadline_type = NA,
                                   deadline      = as.Date(NA),
                                   status        = "archived")
  
  pXXXX_path <-
    project %>% 
    make_project_name() %>% 
    make_project_path(p_path)
  
  fs::file_move(path     = pXXXX_path,
                new_path = fs::path(p_path, "archive"))
  
  message("The following project was archived:")
  return(archived_project)
}

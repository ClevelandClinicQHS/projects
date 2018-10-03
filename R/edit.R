################################################################################
#' @export
edit_affiliation <- function(affiliation,           department_name  = NA,
                             institution_name = NA, address          = NA) {
  
  p_path              <- p_path_internal()
  
  affiliations_path   <- make_rds_path("affiliations", p_path)
  
  affiliations_tibble <- get_rds(affiliations_path)
  
  affiliation         <- validate_entry(affiliation,
                                        what       = "affiliation",
                                        max.length = 1,
                                        rds_tibble        = affiliations_tibble)
  
  message("Edited affiliation:")
  
  change_table(rds_name         = "affiliations",
               rds_path         = affiliations_path,
               rds_tibble       = affiliations_tibble,
               action           = "edit",
               id               = affiliation,
               department_name  = department_name,
               institution_name = institution_name,
               address          = address)
}
################################################################################



################################################################################
#' @importFrom rlang .data
#' @export
edit_author <- function(author,           last_name = NA,
                        given_names = NA, title     = NA,
                        degree      = NA, email     = NA,
                        add_affiliation,  remove_affiliation) {
  
  p_path             <- p_path_internal()
  
  authors_path       <- make_rds_path("authors", p_path)
  
  authors_tibble     <- get_rds(authors_path)
  
  author             <- validate_entry(author,         what = "author",
                                       max.length = 1, rds_tibble  = authors_tibble)
  
  affiliations_tibble <- get_rds(make_rds_path("affiliations", p_path))
  
  if(!missing(add_affiliation)) {
    add_affiliation    <- validate_entry(add_affiliation,
                                         what        = "affiliation",
                                         max.length  = 9999,
                                         any.missing = FALSE,
                                         rds_tibble         = affiliations_tibble)
  }
  
  if(!missing(remove_affiliation)) {
    remove_affiliation <- validate_entry(remove_affiliation,
                                         what        = "affiliation",
                                         max.length  = 9999,               
                                         any.missing = FALSE,
                                         rds_tibble         = affiliations_tibble)
  }
  
  new_author_row <- change_table(rds_name           = "authors", 
                                 rds_path           = authors_path,
                                 rds_tibble         = authors_tibble,
                                 action             = "edit",
                                 id                 = author,
                                 last_name          = last_name,
                                 given_names        = given_names,
                                 title              = title,
                                 degree             = degree,
                                 email              = email)
  
  if(missing(add_affiliation) && missing(remove_affiliation)) {
    author_affiliation_assoc <-
      "author_affiliation_assoc" %>% 
      make_rds_path(p_path) %>% 
      get_rds %>% 
      dplyr::filter(.data$id1 == new_author_row$id)
  }
  else {
    if(!missing(add_affiliation)) {
      author_affiliation_assoc <-
        change_assoc(assoc_name = "author_affiliation_assoc",
                     p_path     = p_path,
                     new        = TRUE,
                     id1        = author,
                     id2        = add_affiliation)
    }
    if(!missing(remove_affiliation)) {
      author_affiliation_assoc <-
        change_assoc(assoc_name = "author_affiliation_assoc",
                     p_path     = p_path,
                     new        = FALSE,
                     id1        = author,
                     id2        = remove_affiliation)
    }
  }
  
  message("Edited author:")
  print(new_author_row)

  message("\nEdited author's affiliations:")
  if(nrow(author_affiliation_assoc) == 0) {
    print("None")
  }
  else {
    print(author_affiliation_assoc %>%
            dplyr::left_join(affiliations_tibble,
                             by = c("id2" = "id")) %>%
            dplyr::select(-.data$id1) %>% 
            dplyr::rename(affiliation_id = id2))
  }
  
}
################################################################################



################################################################################
#' @export
edit_project <- function(project,                        title         = NA,
                         current_owner = NA,             creator       = NA,   
                         stage         = NA,             deadline_type = NA,   
                         deadline      = as.Date(NA),
                         status        = "just created",
                         
                         add_PI,                         remove_PI,
                         add_investigator,               remove_investigator,
                         
                         checklist = c("STROBE", "CONSORT", "PRISMA")) {
  
  p_path          <- p_path_internal()
  
  projects_path   <- make_rds_path("projects", p_path)
  
  projects_tibble <- get_rds(projects_path)
  
  project         <- validate_entry(project, what = "project", max.length = 1,
                                    rds_tibble = projects_tibble)
  
  authors_tibble  <- "authors" %>% make_rds_path(p_path) %>% get_rds
  
  if(!missing(add_PI)) {
    add_PI              <- validate_entry(add_PI,
                                          what        = "PI",
                                          rds_tibble  = authors_tibble)
  }
  if(!missing(remove_PI)) {
    remove_PI           <- validate_entry(x           = remove_PI,
                                          what        = "PI",
                                          rds_tibble  = authors_tibble)
  }
  if(!missing(add_investigator)) {
    add_investigator    <- validate_entry(x           = add_investigator,
                                          what        = "investigator",
                                          rds_tibble  = authors_tibble)
  }
  if(!missing(remove_investigator)) {
    remove_investigator <- validate_entry(x           = remove_investigator,
                                          what        = "investigator",
                                          rds_tibble  = authors_tibble)
  }
  
  new_project_row <- change_table(rds_name      = "projects",
                                  rds_path      = projects_path,
                                  rds_tibble    = projects_tibble,
                                  action        = "edit",
                                  id            = project,
                                  title         = title,
                                  current_owner = current_owner,
                                  creator       = creator,
                                  stage         = stage,
                                  deadline_type = deadline_type,
                                  deadline      = deadline,
                                  status        = status)
  
  if(missing(add_PI) && missing(remove_PI)) {
    project_PI_assoc <-
      "project_PI_assoc" %>% 
      make_rds_path(p_path) %>% 
      get_rds %>% 
      dplyr::filter(.data$id1 == new_project_row$id)
  } 
  else {
    if(!missing(add_PI)) {
      project_PI_assoc <-
        change_assoc(assoc_name = "project_PI_assoc",
                     p_path     = p_path,
                     new        = TRUE,
                     id1        = project,
                     id2        = add_PI)
    }
    if(!missing(remove_PI)) {
      project_PI_assoc <-
        change_assoc(assoc_name = "project_PI_assoc",
                     p_path     = p_path,
                     new        = FALSE,
                     id1        = project,
                     id2        = remove_PI)
    }
  }
  
  
  if(missing(add_investigator) && missing(remove_investigator)) {
    project_investigator_assoc <-
      "project_investigator_assoc" %>% 
      make_rds_path(p_path) %>% 
      get_rds %>% 
      dplyr::filter(.data$id1 == new_project_row$id)
  }
  else {
    if(!missing(add_investigator)) {
      project_investigator_assoc <-
        change_assoc(assoc_name = "project_investigator_assoc",
                     p_path     = p_path,
                     new        = TRUE,
                     id1        = project,
                     id2        = add_investigator)
    }
    
    if(!missing(remove_investigator)) {
      project_investigator_assoc <-
        change_assoc(assoc_name = "project_investigator_assoc",
                     p_path     = p_path,
                     new        = FALSE,
                     id1        = project,
                     id2        = remove_investigator)
    }
  }
  
  
  message("Edited project info:")
  print(new_project_row)
  
  message("\nEdited project's PI(s):")
  if(nrow(project_PI_assoc) == 0) {
    print("None")
  }
  else {
    print(project_PI_assoc %>% 
            dplyr::left_join(authors_tibble,
                             by = c("id2" = "id")) %>% 
            dplyr::select(-.data$id1) %>% 
            dplyr::rename(PI_id = id2))
  }
  
  message("\nEdited project's investigators:")
  if(nrow(project_investigator_assoc) == 0) {
    print("None")
  }
  else {
    print(project_investigator_assoc %>% 
            dplyr::left_join(authors_tibble,
                             by = c("id2" = "id")) %>% 
            dplyr::select(-.data$id1) %>% 
            dplyr::rename(investigator_id = id2))
  }
}

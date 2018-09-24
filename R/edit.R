################################################################################
#' @export
edit_affiliation <- function(id,                    department_name  = NA,
                             institution_name = NA, address          = NA) {
  
  message("Edited affiliation:")
  
  test_id_entry(id, what = "affiliation", max.length = 1)
  
  change_table(rds_name         = "affiliations",
               action           = "edit",
               id               = id,
               department_name  = department_name,
               institution_name = institution_name,
               address          = address)
}
################################################################################



################################################################################
#' @importFrom rlang .data
#' @export
edit_author <- function(id,               last_name = NA,
                        given_names = NA, title     = NA,
                        degree      = NA, email     = NA,
                        add_affiliation,  remove_affiliation) {
  
  p_path         <- p_path_internal()
  
  test_id_entry(id = id, what = "author", max.length = 1)
  
  if(!missing(add_affiliation)) {
    test_id_entry(id         = add_affiliation, what        = "affiliation",
                  max.length = 9999,            any.missing = FALSE)
  }
  
  if(!missing(remove_affiliation)) {
    test_id_entry(id         = remove_affiliation, what        = "affiliation",
                  max.length = 9999,               any.missing = FALSE)
  }
  
  new_author_row <- change_table(rds_name           = "authors", 
                                 p_path             = p_path,
                                 action             = "edit",
                                 id                 = id,
                                 last_name          = last_name,
                                 given_names        = given_names,
                                 title              = title,
                                 degree             = degree,
                                 email              = email)
  
  # if(!is.na(affiliations) && length(affiliations) > 0) {
  #   author_affiliation_assoc <- 
  #     change_table(rds_name       = "author_affiliation_assoc",
  #             edit           = TRUE,
  #             p_path         = p_path,
  #             author_id      = new_author_row$id,
  #             affiliation_id = affiliations)
  # }
  # else {
  #   author_affiliation_assoc <-
  #     get_rds(make_rds_path("author_affiliations_assoc", p_path)) %>% 
  #     dplyr::filter(.data$author_id == id)
  # }
  
  affiliation_tibble <- get_rds(make_rds_path("affiliations", p_path))
  
  if(!missing(add_affiliation)) {
    change_assoc(assoc_name = "author_affiliation_assoc",
                 p_path     = p_path,
                 new        = TRUE,
                 id1        = id,
                 id2        = add_affiliation)
  }
  if(!missing(remove_affiliation)) {
    change_assoc(assoc_name = "author_affiliation_assoc",
                 p_path     = p_path,
                 new        = FALSE,
                 id1        = id,
                 id2        = remove_affiliation)
  }
  
  author_affiliation_assoc <-
    "author_affiliation_assoc" %>% 
    make_rds_path(p_path) %>% 
    get_rds
  
  message("Edited author:")
  print(new_author_row)

  message("\nEdited author's affiliations:")
  print(author_affiliation_assoc %>%
          dplyr::filter(.data$id1 == new_author_row$id) %>%
          dplyr::left_join(affiliation_tibble,
                           by = c("id2" = "id")) %>%
          dplyr::select(-.data$id1) %>% 
          dplyr::rename(affiliation_id = id2))
}
################################################################################



################################################################################
#' @export
edit_project <- function(id,                             title         = NA,
                         current_owner = NA,             creator       = NA,   
                         stage         = NA,             deadline_type = NA,   
                         deadline      = as.Date(NA),
                         status        = "just created",
                         
                         add_PI,                         remove_PI,
                         add_investigator,               remove_investigator,
                         
                         checklist = c("STROBE", "CONSORT", "PRIMA")) {
  
  p_path <- p_path_internal()
  
  test_id_entry(id = id, what = "project", max.length = 1)
  
  if(!missing(add_PI)) {
    test_id_entry(id = add_PI, what = "PI")
  }
  if(!missing(remove_PI)) {
    test_id_entry(id = remove_PI, what = "PI")
  }
  if(!missing(add_investigator)) {
    test_id_entry(id = add_investigator, what = "investigator")
  }
  if(!missing(remove_investigator)) {
    test_id_entry(id = remove_investigator, what ="investigator")
  }
  
  new_project_row <- change_table(rds_name      = "projects",
                                  p_path        = p_path,
                                  action        = "edit",
                                  id            = id,
                                  title         = title,
                                  current_owner = current_owner,
                                  creator       = creator,
                                  stage         = stage,
                                  deadline_type = deadline_type,
                                  deadline      = deadline,
                                  status        = status)
  
  if(!missing(add_PI)) {
    change_assoc(assoc_name = "project_PI_assoc",
                 p_path     = p_path,
                 new        = TRUE,
                 id1        = id,
                 id2        = add_PI)
  }
  
  if(!missing(remove_PI)) {
    change_assoc(assoc_name = "project_PI_assoc",
                 p_path     = p_path,
                 new        = FALSE,
                 id1        = id,
                 id2        = remove_PI)
  }
  
  project_PI_assoc <-
    "project_PI_assoc" %>% 
    make_rds_path(p_path) %>% 
    get_rds
  
  if(!missing(add_investigator)) {
    change_assoc(assoc_name = "project_investigator_assoc",
                 p_path     = p_path,
                 new        = TRUE,
                 id1        = id,
                 id2        = add_investigator)
  }
  
  if(!missing(remove_investigator)) {
    change_assoc(assoc_name = "project_investigator_assoc",
                 p_path     = p_path,
                 new        = FALSE,
                 id1        = id,
                 id2        = remove_investigator)
  }
  
  project_investigator_assoc <-
    "project_investigator_assoc" %>% 
    make_rds_path(p_path) %>% 
    get_rds
  
  author_tibble <-
    "authors" %>% 
    make_rds_path(p_path) %>% 
    get_rds
  
  message("Edited project info:")
  print(new_project_row)
  
  message("\nEdited project's PI(s):")
  print(project_PI_assoc %>% 
          dplyr::filter(.data$id1 == new_project_row$id) %>% 
          dplyr::left_join(author_tibble,
                           by = c("id2" = "id")) %>% 
          dplyr::select(-.data$id1) %>% 
          dplyr::rename(PI_id = id2))
  
  message("\nEdited project's investigators:")
  print(project_investigator_assoc %>% 
          dplyr::filter(.data$id1 == new_project_row$id) %>%
          dplyr::left_join(author_tibble,
                           by = c("id2" = "id")) %>% 
          dplyr::select(-.data$id1) %>% 
          dplyr::rename(investigator_id = id2))
}
################################################################################



################################################################################
################################################################################
# The internal function that adds a row to one of the rds objects

# set_rds <- function(rds_name, p_path = p_path_internal(), ...) {
#   
#   new_row    <- tibble::as_tibble(list(...))
#   
#   rds_path   <- make_rds_path(rds_name, p_path)
#   rds_tibble <- get_rds(rds_path)
#   
#   ###########################################################################
#   # This section is entirely dedicated to creating an id for a new row in the
#   # projects, authors, or affiliations tibbles
#   if(rds_name == "projects") {
#     if(is.na(new_row$id)) {
#       
#       # If there are no projects, id will be 1.
#       # Otherwise, id will be 1 + the highest existing project id.
#       # HOWEVER: if project id 9999 is taken, id will be the
#       # lowest available id in 1:9999.
#       max_id <- max(rds_tibble$id, 0L)
#       if(max_id < 9999L) {
#         new_row$id <- max_id + 1L
#       }
#       else if(!all(1L:9999L %in% rds_tibble$id)) {
#         new_row$id <- min(setdiff(1L:9999L, rds_tibbleid))
#       }
#       else {
#         stop("projects folder is full. Delete or archive one or more of them.")
#       }
#     }
#     else {
#       
#       if(isFALSE(checkmate::test_integerish(new_row$id,
#                                             lower = 1L, upper = 9999L,
#                                             any.missing = FALSE, len = 1L))) {
#         stop("id must be an integer; not ")
#       }
#       
#       new_row$id <- as.integer(new_row$id)
#       
#       if(new_row$id %in% rds_tibble$id) {
#         stop('id already taken. Try a different one or leave the ',
#              'argument blank for automatic selection.')
#       }
#     }
#   }
#   else if(rds_name %in% c("authors", "affiliations")) {
#     new_row$id <- sample(x = setdiff(10:99, rds_tibble$id), size = 1)
#   }
#   #
#   ###########################################################################
#   
#   rds_tibble <- dplyr::bind_rows(new_row, rds_tibble)
#   
#   saveRDS(rds_tibble, rds_path)
#   
#   # This returns all rows in which the first id column's ids match its first
#   # value.
#   # In effect, this returns all rows associated with the newly created item
#   return(rds_tibble[rds_tibble[[1]] == rds_tibble[[1, 1]],])
# }

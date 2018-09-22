################################################################################
#' @export
edit_affiliation <- function(id,                    department_name  = NA,
                             institution_name = NA, address          = NA) {
  
  checkmate::assert_int(id)
  
  message("Edited affiliation:")
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
edit_author <- function(id,                last_name = NA,
                        given_names  = NA, title     = NA,
                        affiliations = NA, degree    = NA,
                        email        = NA) {
  
  checkmate::assert_int(id)
  
  p_path         <- p_path_internal()
  new_author_row <- change_table(rds_name     = "authors", 
                            p_path       = p_path,
                            edit         = TRUE,
                            id           = id,
                            last_name    = last_name,
                            given_names  = given_names,
                            title        = title,
                            degree       = degree,
                            email        = email)
  
  if(!is.na(affiliations) && length(affiliations) > 0) {
    author_affiliation_assoc <- 
      change_table(rds_name       = "author_affiliation_assoc",
              edit           = TRUE,
              p_path         = p_path,
              author_id      = new_author_row$id,
              affiliation_id = affiliations)
  }
  else {
    author_affiliation_assoc <-
      get_rds(make_rds_path("author_affiliations_assoc", p_path)) %>% 
      dplyr::filter(.data$author_id == id)
  }
  
  affiliation_tibble <- get_rds(make_rds_path("affiliations", p_path))
  
  message("Edited author:")
  print(new_author_row)

  message("\nEdited author's affiliations:")
  print(author_affiliation_assoc %>%
          dplyr::filter(.data$author_id == new_author_row$id) %>%
          dplyr::left_join(affiliation_tibble,
                           by = c("affiliation_id" = "id")) %>%
          dplyr::select(-.data$author_id))
}
################################################################################



################################################################################
#' @export
edit_project <- function(title         = NA,   current_owner = NA,   
                         PI            = NA,   investigators = NA,   
                         creator       = NA,   stage         = NA,   
                         deadline_type = NA,   deadline      = as.Date(NA),
                         id            = NA,   status        = "just created",
                        
                        checklist = c("STROBE", "CONSORT", "PRIMA")) {
  
  p_path          <- p_path_internal()
  new_project_row <- change_table(rds_name      = "projects",
                             p_path        = p_path,
                             edit          = TRUE,
                             id            = id,
                             title         = title,
                             current_owner = current_owner,
                             creator       = creator,
                             stage         = stage,
                             deadline_type = deadline_type,
                             deadline      = deadline,
                             status        = status)
  
  project_PI_assoc <-
    change_table(rds_name         = "project_PI_assoc",
            p_path           = p_path,
            edit             = TRUE,
            project_id       = new_project_row$id,
            PI_id            = PI)
  
  project_investigator_assoc <-
    change_table(rds_name        = "project_investigator_assoc",
            p_path          = p_path,
            edit            = TRUE,
            project_id      = new_project_row$id,
            investigator_id = investigators)
  
  author_tibble <- get_rds(make_rds_path("authors", p_path))
  
  pXXXX_name     <- make_project_name(new_project_row$id)
  pXXXX_path     <- make_project_path(pXXXX_name, p_path)
  
  fs::dir_create(fs::path(pXXXX_path, c("data", "progs", "manuscript",
                                        "figures")))
  
  fs::file_copy(path     = system.file("extdata", "pXXXX_protocol.docx",
                                       package = "projects"),
                new_path = fs::path(pXXXX_path, paste0(pXXXX_name, "_protocol"),
                                    ext = "docx"))
  
  readr::write_lines(Rproj_template, # Rproj_template is in sysdata.Rda
                     fs::path(pXXXX_path, pXXXX_name, ext = "Rproj"))
  
  message("Edited project info:")
  print(new_project_row)
  
  message("\nEdited project's PI(s):")
  print(project_PI_assoc %>% 
          dplyr::filter(.data$project_id == new_project_row$id) %>% 
          dplyr::left_join(author_tibble,
                           by = c("PI_id" = "id")) %>% 
          dplyr::select(-.data$project_id))
  
  message("\nEdited project's investigators:")
  print(project_investigator_assoc %>% 
          dplyr::filter(.data$project_id == new_project_row$id) %>%
          dplyr::left_join(author_tibble,
                           by = c("investigator_id" = "id")) %>% 
          dplyr::select(-.data$project_id))
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

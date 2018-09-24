#' @export
delete_affiliation <- function(id) {
  
  p_path <- p_path_internal()
  
  test_id_entry(id = id, what = "affiliation", max.length = 1)
  
  deleted_affiliation <- change_table(rds_name = "affiliations",
                                      p_path   = p_path,
                                      action   = "delete",
                                      id       = id)
  
  # rds_path    <- make_rds_path(assoc_name, p_path)
  # assoc_table <- get_rds(rds_path)
  
  change_assoc(assoc_name = "author_affiliation_assoc",
               p_path     = p_path,
               new        = FALSE,
               id2        = id)
  
  message("The following affiliation was deleted:")
  return(deleted_affiliation)
}

#' @export
delete_author <- function(id) {
  
  p_path         <- p_path_internal()
  
  test_id_entry(id = id, what = "author", max.length = 1)
  
  deleted_author <- change_table(rds_name = "authors",
                                 p_path   = p_path,
                                 action   = "delete",
                                 id       = id)
  
  change_assoc(assoc_name = "author_affiliation_assoc",
               p_path     = p_path,
               new        = FALSE,
               id1        = id)
  
  change_assoc(assoc_name = "project_PI_assoc",
               p_path     = p_path,
               new        = FALSE,
               id2        = id)
  
  change_assoc(assoc_name = "project_investigator_assoc",
               p_path     = p_path,
               new        = FALSE,
               id2        = id)
  
  message("The following author was deleted:")
  return(deleted_author)
}




#' @export
delete_project <- function(id, archived = FALSE) {
  
  p_path <- p_path_internal()
  
  test_id_entry(id = id, what = "project", max.length = 1)
  
  deleted_project <- change_table(rds_name = "projects",
                                  p_path   = p_path,
                                  action   = "delete",
                                  id       = id)
  
  change_assoc(assoc_name = "project_PI_assoc",
               p_path     = p_path,
               new        = FALSE,
               id1        = id)
  
  change_assoc(assoc_name = "project_investigator_assoc",
               p_path     = p_path,
               new        = FALSE,
               id1        = id)
  
  pXXXX_path <- 
    id %>% 
    make_project_name() %>% 
    make_project_path(p_path) %>% 
    fs::path()
  
  if(archived) {
    fs::path(pXXXX_path, "archived")
  }
  
  fs::dir_delete(path = pXXXX_path)
  
  message("The following ", ifelse(archived, "archived ", ""),
          "project was deleted:")
  return(deleted_project)
}




archive_project <- function(id) {
  
  p_path <- p_path_internal()
  
  test_id_entry(id = id, what = "project", max.length = 1)
  
  change_table(rds_name      = "projects",
               p_path        = p_path,
               action        = "edit",
               id            = id,
               title         = NA,
               current_owner = NA,
               creator       = NA,
               stage         = NA,
               deadline_type = NA,
               deadline      = as.Date(NA),
               status        = "archived")
  
  pXXXX_path <-
    id %>% 
    make_project_name() %>% 
    make_project_path(p$folder_path)
  
  fs::file_move(path     = pXXXX_path,
                new_path = fs::path(p_path, "archive"))
  
  
}


# # @export
# archive_project <- function(id_to_archive) {
#   message(paste0(archive_delete(id_to_archive, archive = TRUE),
#                  " archived. "))
# }
# 
# # @export
# delete_project <- function(id_to_delete) {
#   message(paste0(archive_delete(id_to_delete, archive = FALSE),
#                  " deleted. "))
# }
# 
# # Needs to be fixed so it doesn't depend on project_data()
# # @importFrom rlang .data
# archive_delete <- function(id, archive) {
# 
#   p <- project_data()
#   
#   if(isFALSE(checkmate::test_integerish(id, lower = 1L, upper = 9999L,
#                                         any.missing = FALSE, unique = TRUE,
#                                         min.len = 1L, max.len = 9999L))) {
#     stop("id must be a vector of unique integers between 1 and 9999")
#   }
#   
#   if(!all(id %in% p$list$id)) {
#     stop("At least one project id not present in projects.rds at ",
#          p$list_path)
#   }
# 
#   pXXXX_path <-
#     id %>% 
#     make_project_name() %>% 
#     make_project_path(p$folder_path)
#   
#   
#   if(archive) {
#     fs::file_move(path     = pXXXX_path,
#                   new_path = fs::path(p$folder_path, "archive"))
#   }
#   else {
#     fs::dir_delete(path = pXXXX_path)
#   }
# 
#   #p$list <-
#   p$list %>%
#     dplyr::filter(!(.data$id %in% id)) %>% 
#     saveRDS(p$list_path)
#   
#   return(paste0("Project ", id))
# }

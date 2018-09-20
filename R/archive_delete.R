#' @export
archive_project <- function(id_to_archive) {
  message(paste0(archive_delete(id_to_archive, archive = TRUE),
                 " archived. "))
}


#' @export
delete_project <- function(id_to_delete) {
  message(paste0(archive_delete(id_to_delete, archive = FALSE),
                 " deleted. "))
}



#' @importFrom rlang .data
archive_delete <- function(id, archive) {

  p <- project_data()
  
  if(isFALSE(checkmate::test_integerish(id, lower = 1L, upper = 9999L,
                                        any.missing = FALSE, unique = TRUE,
                                        min.len = 1L, max.len = 9999L))) {
    stop("id must be a vector of unique integers between 1 and 9999")
  }
  
  if(!all(id %in% p$list$id)) {
    stop("At least one project id not present in projects.rds at ",
         p$list_path)
  }

  pXXXX_path <-
    id %>% 
    make_project_name() %>% 
    make_project_path(p$folder_path)
  
  
  if(archive) {
    fs::file_move(path     = pXXXX_path,
                  new_path = fs::path(p$folder_path, "archive"))
  }
  else {
    fs::dir_delete(path = pXXXX_path)
  }

  #p$list <-
  p$list %>%
    dplyr::filter(!(.data$id %in% id)) %>% 
    saveRDS(p$list_path)
  
  return(paste0("Project ", id))
}

#' @export
archive_project <- function(numbers_to_archive) {
  message(paste0(archive_delete(numbers_to_archive, archive = TRUE),
                 " archived. "))
}


#' @export
delete_project <- function(numbers_to_delete) {
  message(paste0(archive_delete(numbers_to_delete, archive = FALSE),
                 " deleted. "))
}



#' @importFrom rlang .data
archive_delete <- function(numbers, archive) {

  p <- projects()
  
  if(isFALSE(checkmate::test_integerish(numbers, lower = 1L, upper = 9999L,
                                        any.missing = FALSE, unique = TRUE,
                                        min.len = 1L, max.len = 9999L))) {
    stop("numbers must be a vector of unique integers between 1 and 9999")
  }
  
  if(!all(numbers %in% p$list$number)) {
    stop("At least one project number not present in project_list.csv at ",
         p$list_path)
  }

  pXXXX <- make_project(numbers, p$folder_path)
  
  if(archive) {
    fs::file_move(path = pXXXX$path,
                  new_path = fs::path(p$folder_path, "archive"))
  }
  else {
    fs::dir_delete(path = pXXXX$path)
  }

  #p$list <-
  p$list %>%
    dplyr::filter(!(.data$number %in% numbers)) %>% 
    saveRDS(p$list_path)
  
  return(paste0("Project ", numbers))
}

fs::path_file("dir/file.zip")
fs::path_dir("dir/file.zip")
fs::path_ext("dir/file.zip")
fs::path_ext("file.tar.gz")
fs::path_ext_remove("file.tar.gz")

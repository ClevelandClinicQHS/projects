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

  if(isFALSE(checkmate::test_integerish(numbers, lower = 1L, upper = 9999L,
                                        any.missing = FALSE, unique = TRUE,
                                        min.len = 1L, max.len = 9999L))) {
    stop("numbers must be a vector of unique integers between 1 and 9999")
  }
  
  #projects_path     <- projects_path(check = TRUE)
  check_projects_path(error = TRUE)
  
  #project_list_path <- fs::path(projects_path, "project_list", ext = "csv")
  project_list_path <- fs::path(projects_path, "project_list", ext = "csv")
  
  #project_list      <- readr::read_csv(project_list_path, col_types = "ic")
  project_list      <- readRDS(project_list_path)

  if(!all(numbers %in% project_list$number)) {
    stop("At least one project number not present in project_list.csv at ",
         project_list_path)
  }

  path <- fs::path(projects_path, make_project_name(numbers))
  
  if(archive) {
    fs::file_move(path = path, new_path = fs::path(projects_path, "archive"))
  }
  else {
    fs::dir_delete(path = path)
  }

  project_list <- dplyr::filter(project_list,
                                !(.data$number %in% numbers))

  readr::write_csv(project_list, path = project_list_path)
  
  return(paste0("Project ", numbers))
}

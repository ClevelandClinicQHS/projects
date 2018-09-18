#' @importFrom rlang .data
#' @export
new_project <- function(project_number) {

  #projects_path     <- projects_path(check = TRUE)
  check_projects_path(error = TRUE)
  
  #project_list_path <- file.path(projects_path, "project_list.csv")
  project_list_path <- fs::path(projects_path, "project_list", ext = "rds")

  if(!fs::file_exists(project_list_path)) {
    stop("project_list.csv file not detected at ", project_list_path, ". ",
         "Restore this file at this location OR run setup_project_folder()")
  }

  #project_list <- readr::read_csv(file = project_list_path, col_types = "ic")
  project_list <- readRDS(project_list_path)

  if(missing(project_number)) {
    
    # If there are no projects, project_number will be 1.
    # Otherwise, project_number will be 1 + the highest existing project number.
    # HOWEVER: if project number 9999 is taken, project_number will be the
    # lowest available number in 1:9999.
    if(nrow(project_list) == 0) {
      project_number <- 1L
    }
    else if(max(project_list$number) < 9999L) {
      project_number <- max(project_list$number) + 1L
    }
    else if(!all(1L:9999L %in% project_list$number)){
      project_number <- min(setdiff(1L:9999L, project_list$number))
    }
    else {
      stop("projects folder is full. Delete or archive one or more of them.")
    }
  }
  else {
    
    if(isFALSE(checkmate::test_integerish(project_number, lower = 1L, upper = 9999L,
                                 any.missing = FALSE, len = 1L))) {
      stop("project_number must be an integer")
    }
    
    project_number <- as.integer(project_number)
    
    if(project_number %in% project_list$number) {
      stop('project_number already taken. Try a different one or leave the ',
           'argument blank for automatic selection.')
    }
  }

  pXXXX      <- make_project_name(project_number)

  pXXXX_path <- unclass(fs::path(projects_path, pXXXX))

  fs::dir_create(file.path(pXXXX_path, c("data", "progs", "reports",
                                         "manuscript", "figures")))
  
  fs::file_copy(path     = system.file("extdata", "pXXXX_protocol.docx",
                                       package = "projects"),
                new_path = fs::path(pXXXX_path,
                                    paste0(pXXXX, "_protocol"), ext = "docx"))

  readr::write_lines(Rproj_template, fs::path(pXXXX_path, pXXXX, ext = "Rproj"))

  project_list <-
    dplyr::arrange(
      dplyr::bind_rows(project_list,
                       tibble::tibble(number = project_number,
                                      path = pXXXX_path)),
      .data$number)

  #readr::write_csv(project_list, path = project_list_path)
  saveRDS(project_list, file = project_list_path)

  message("Project ", pXXXX, " has been created at ", pXXXX_path)
}

#' @export
projects_path <- function() {
  Sys.getenv("PROJECTS_FOLDER_PATH")
}

#' @export
project_list <- function(path = make_rds_path("project_list")) {
  rds_getter("project_list", path)
}

#' @export
authors <- function(path = make_rds_path("authors_list")) {
  rds_getter("authors_list", path)
}

#' @export
affiliations <- function(path = make_rds_path("affiliations_list")) {
  rds_getter("affiliations_list", path)
}

rds_getter <- function(rds_name, rds_path) {
  if(fs::file_exists(rds_path)) {
    return(readRDS(rds_path))
  }
  else {
    stop(rds_name, ".rds not found in ", rds_path, ". Please restore the file ",
         "or [re]run setup_projects_folder()")
  }
}

make_rds_path <- function(rds_name, p_path = projects_path()) {
  fs::path(p_path, rds_name, ext = "rds")
}
  

check_projects_path <- function(path = projects_path(), error = TRUE) {
  if(path == "") {
    message <- '"projects" folder not found. Please run setup_projects_folder()'
    if(error) {
      stop(message)
    }
  }
  else {
    message <- paste0('"projects" folder located at ', path)
  }
  
  invisible(message)
}


projects <- function(p_path = projects_path(), rds_paths_only = FALSE) {
  
  check_projects_path(path = p_path, error = TRUE)
  
  output <- 
    list(list_path         = make_rds_path("project_list", p_path),
         authors_path      = make_rds_path("authors_list", p_path),
         affiliations_path = make_rds_path("affiliations_list", p_path))
  
  if(rds_paths_only) {
    return(output)
  }
  else {
    return(c(output,
             list(folder_path  = p_path,
                  list         = project_list(output$list_path),
                  authors      = authors(output$authors_path),
                  affiliations = affiliations(output$affiliations_path))))
  }
}



# When given a project number it returns a list of two character strings
make_project <- function(project_number, path = projects_path()) {
  
  tibble::lst(name = paste0("p", stringr::str_pad(project_number, width = 4,
                                                  side = "left", pad = "0")),
              path = fs::path(path, name) %>% unclass())
}


make_project_name <- function(project_id) {
  paste0("p", stringr::str_pad(project_id, width = 4, side = "left", pad = "0"))
}

make_project_path <- function(project_name, path = p_path_internal()) {
  fs::path(path, project_name) %>% unclass()
}


check_all_rds <- function() {
  purrr::walk(c("affiliations", "author_affiliation_assoc", "authors",
                "project_author_assoc", "projects"),
              function(x) {
                print(x)
                print(readRDS(fs::path(projects_folder(), "metadata", x,
                                       ext = "rds")))
              })
}

user_prompt <- function(msg, y_msg, n_msg, error = TRUE) {
  prompt <- NULL
  while(is.null(prompt) || !(prompt %in% c("y", "n"))) {
    if(!is.null(prompt)) {
      message("\nInvalid input.\n")
    }
    message(msg)
    prompt <- tolower(readLines(con = stdin(), n = 1L))
  }
  if(prompt == "n") {
    if(error) {
      stop(n_msg)
    }
    else if(!missing(n_msg)) {
      message(n_msg)
    }
  }
  else if(!missing(y_msg)) {
    msg(y_msg)
  }
  return(prompt == "y")
}


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
                print(readRDS(fs::path(projects_folder(), ".metadata", x,
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


#' @importFrom rlang .data
fix_metadata <- function(path = ".metadata") {
  p_path <- p_path_internal()
  purrr::walk(
    .x = c("projects", "authors", "affiliations",
           "project_author_assoc", "author_affiliation_assoc"),
    .f = 
      function(x) {
        tibble <- readRDS(file = fs::path(p_path, path, x, ext = "rds"))
        if(x == "projects") {
          tibble$id <- as.integer(tibble$id)
          
          tibble$deadline <- as.Date(tibble$deadline)
          
          if(is.null(tibble$creator)) {
            tibble$creator <- NA_integer_
          }
          else {
            tibble$creator <- as.integer(tibble$creator)
          }
          
          if(is.null(tibble$current_owner)) {
            tibble$current_owner <- NA_integer_
          }
          else {
            tibble$current_owner <- as.integer(tibble$current_owner)
          }
          
          if(is.null(tibble$corresp_auth)) {
            tibble$corresp_auth <- NA_integer_
          }
          else {
            tibble$corresp_auth <- as.integer(tibble$corresp_auth)
          }
          
          if(is.null(tibble$creator)) {
            tibble$creator <- NA_integer_
          }
          else {
            tibble$creator <- as.integer(tibble$creator)
          }
          
          if(is.null(tibble$path)) {
            tibble <-
              dplyr::mutate(
                tibble,
                path = fs::path(p_path, make_project_name(.data$id)))
          }
          
          if(is.null(tibble$short_title)) {
            tibble$short_title <- NA_character_
          }
          
          tibble <- dplyr::select(
            tibble,
            id, title, short_title, current_owner, creator, corresp_auth,
            stage, deadline_type, deadline, status, path)
        }
        
        if(x == "authors") {
          tibble$id <- as.integer(tibble$id)
          if(is.null(tibble$phone)) {
            tibble$phone <- NA_character_
          }
          if(is.null(tibble$default)) {
            tibble$default <- FALSE
          }
          
          tibble <-
            dplyr::select(
              tibble,
              id, given_names, last_name, title, degree, email, phone, default)
        }
        
        if(x == "affiliations") {
          tibble$id <- as.integer(tibble$id)
        }
        
        if(x == "project_author_assoc") {
          
          if(fs::file_exists(fs::path(p_path, path, "project_PI_assoc",
                                      ext = "rds"))) {
            PI <-
              readRDS(file = fs::path(p_path, path, "project_PI_assoc",
                                      ext = "rds")) %>%
              dplyr::mutate(id1 = as.integer(id1), id2 = as.integer(id2))
            
            tibble <- dplyr::mutate(id1 = as.integer(id1), id2 = as.integer(id2))
            tibble <- dplyr::bind_rows(PI, tibble)
          }
  
        }
        
        if(x == "author_affiliation_assoc") {
          tibble <- dplyr::mutate(tibble, 
                                  id1 = as.integer(id1), id2 = as.integer(id2))
        }
        
        saveRDS(tibble, file = fs::path(p_path, ".metadata", x, ext = "rds"))
      })
}
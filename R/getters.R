################################################################################
################################################################################
# This is a user-friendly wrapper for p_path_internal() below it
#' @export
projects_folder <- function() {
  p_path_internal(error = FALSE)
}

p_path_internal <- function(error = TRUE) {
  
  path <- Sys.getenv("PROJECTS_FOLDER_PATH")
  
  if(fs::dir_exists(path)) {
    return(path)
  }
  else {
    notice <- '"projects" folder not found. Please run setup_projects_folder()'
    if(error) {
      stop(notice)
    }
    else {
      return(notice)
    }
  }
}
################################################################################
################################################################################
# This is a user-friendly wrapper for get_default_author_internal() below it
#' @export
get_default_author <- function() {
  get_default_author_internal()
}


get_default_author_internal <- function(error_piece    = "",
                                        authors_tibble = authors()) {
  
  default_author <- Sys.getenv("PROJECTS_DEFAULT_AUTHOR")

  if(default_author == "") {
    stop(error_piece, "Default user not found. Please run set_default_author()",
         " using an id or last_name found in the table produced by authors().")
  }
  
  default_author <- validate_entry(x          = as.numeric(default_author),
                                 what       = "author",
                                 max.length = 1L,
                                 rds_tibble = authors_tibble)
  
  return(authors_tibble[authors_tibble$id == default_author,])
}

################################################################################
################################################################################

#' @importFrom rlang .data
#' @export
affiliations <- function(affiliation, authors = FALSE) {
  
  p_path              <- p_path_internal()
  affiliations_tibble <- 
    "affiliations" %>% 
    make_rds_path(p_path) %>% 
    get_rds()
  
  if(!missing(affiliation)) {
    affiliation <- validate_entry(affiliation,
                                  what = "affiliation",
                                  rds_tibble  = affiliations_tibble)
    
    affiliations_tibble <-
      affiliations_tibble %>% dplyr::filter(.data$id %in% affiliation)
    
    # id <- rlang::enquo(id)
    # authors_tibble <- authors_tibble %>% dplyr::filter(.data$id %in% !!id)
  }
  
  if(authors == TRUE) {
    authors_tibble <-
      "authors" %>% 
      make_rds_path(p_path) %>% 
      get_rds() %>% 
      dplyr::select("id", "last_name", "given_names")
    
    author_affiliation_assoc <-
      "author_affiliation_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds() %>% 
      dplyr::filter(.data$id2 %in% affiliations_tibble$id) %>% 
      dplyr::arrange(.data$id2, .data$id1) 
    
    author_affiliation_assoc %>% 
      dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>% 
      dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>% 
      dplyr::rename(id = id2, author_id = id1) %>% 
      dplyr::full_join(affiliations_tibble) %>% 
      dplyr::select(id,        department_name:address,
                    author_id, last_name:given_names) %>% 
      suppressMessages()
  }
  else {
    affiliations_tibble
  }
}




#' @importFrom rlang .data
#' @export
authors <- function(author, affiliations = FALSE) {
  
  p_path         <- p_path_internal()
  authors_tibble <-
    "authors" %>% 
    make_rds_path(p_path) %>% 
    get_rds() %>%
    dplyr::arrange(.data$last_name, .data$given_names)
  
  if(!missing(author)) {
    author         <- validate_entry(author,
                                     what       = "author",
                                     rds_tibble = authors_tibble)
    
    authors_tibble <- authors_tibble %>% dplyr::filter(.data$id %in% author)
  }
  
  if(affiliations == TRUE) {
    affiliations_tibble <-
      "affiliations" %>% 
      make_rds_path(p_path) %>% 
      get_rds() %>% 
      dplyr::select("id", "department_name", "institution_name")
    
    author_affiliation_assoc <-
      "author_affiliation_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds() %>% 
      dplyr::filter(.data$id1 %in% authors_tibble$id) %>% 
      dplyr::arrange(.data$id1, .data$id2)
    
    author_affiliation_assoc %>% 
      dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>% 
      dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>% 
      dplyr::rename(id = id1, affiliation_id = id2) %>% 
      dplyr::full_join(authors_tibble) %>% 
      dplyr::select(id,             last_name:email,
                    affiliation_id, department_name:institution_name) %>% 
      suppressMessages
  }
  else {
    authors_tibble
  }
}




#' @importFrom rlang .data
#' @export
projects <- function(project, PI = FALSE, authors = FALSE) {
  
  p_path          <- p_path_internal()
  projects_tibble <- 
    "projects" %>% 
    make_rds_path(p_path) %>% 
    get_rds() %>%
    dplyr::arrange(.data$id)
  
  if(!missing(project)) {
    
    project         <- validate_entry(project,
                                      what       = "project",
                                      rds_tibble = projects_tibble)
    
    projects_tibble <- projects_tibble %>% dplyr::filter(.data$id %in% project)
  }
  
  if(PI == TRUE || authors == TRUE) {
    
    authors_tibble <- 
      "authors" %>% 
      make_rds_path(p_path) %>% 
      get_rds() %>% 
      dplyr::select("id", "last_name", "given_names")
    
    if(PI == TRUE) {
      project_PI_assoc <-
        "project_PI_assoc" %>%
        make_rds_path(p_path) %>%
        get_rds() %>% 
        dplyr::filter(.data$id1 %in% projects_tibble$id) %>% 
        dplyr::arrange(.data$id1, .data$id2)
      
      projects_tibble <- 
        project_PI_assoc %>% 
        dplyr::left_join(projects_tibble, by = c("id1" = "id")) %>% 
        dplyr::left_join(authors_tibble, by = c("id2" = "id")) %>% 
        dplyr::rename(id             = id1,
                      PI_id          = id2,
                      PI_last_name   = last_name,
                      PI_given_names = given_names) %>% 
        dplyr::full_join(projects_tibble) %>%
        dplyr::select(.data$id, .data$title:.data$status, .data$PI_id,
                      .data$PI_last_name:.data$PI_given_names) %>% 
        suppressMessages
    }
    
    if(authors == TRUE) {
      project_author_assoc <-
        "project_author_assoc" %>%
        make_rds_path(p_path) %>%
        get_rds() %>% 
        dplyr::filter(.data$id1 %in% projects_tibble$id) %>% 
        dplyr::arrange(.data$id1, .data$id2)
      
      projects_tibble <-
        project_author_assoc %>%
        dplyr::left_join(projects_tibble, by = c("id1" = "id")) %>% 
        dplyr::mutate(tmp = TRUE) %>% 
        dplyr::left_join(authors_tibble, by = c("id2" = "id")) %>% 
        dplyr::rename(id                   = id1,
                      investig_id          = id2,
                      investig_last_name   = last_name,
                      investig_given_names = given_names) %>% 
        dplyr::full_join(projects_tibble) %>%
        dplyr::select(.data$id, .data$title:.data$tmp, .data$investig_id,
                      .data$investig_last_name:.data$investig_given_names) %>% 
        dplyr::select(-.data$tmp) %>% 
        dplyr::arrange(.data$id) %>% 
        suppressMessages
    }
  }
  
  return(projects_tibble)
}

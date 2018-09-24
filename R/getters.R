################################################################################
################################################################################
# This is a user-friendly wrapper for p_path_internal() below.
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



################################################################################
################################################################################
# User .rds-getting functions

#' @importFrom rlang .data
#' @export
projects <- function(id, PI = FALSE, investigators = FALSE) {
  
  p_path          <- p_path_internal()
  projects_tibble <- "projects" %>% make_rds_path(p_path) %>% get_rds()
  
  if(!missing(id)) {
    
    test_id_entry(id = id, what = "project")
    
    if(!all(id %in% projects_tibble$id)) {
      stop("At least one project id not found")
    }
    id <- rlang::enquo(id)
    projects_tibble <- projects_tibble %>% dplyr::filter(.data$id %in% !!id)
  }
  
  if(PI == TRUE || investigators == TRUE) {
    
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
        dplyr::filter(.data$id1 %in% projects_tibble$id)
      
      projects_tibble <- 
        project_PI_assoc %>% 
        dplyr::left_join(projects_tibble, by = c("id1" = "id")) %>% 
        dplyr::left_join(authors_tibble, by = c("id2" = "id")) %>% 
        dplyr::rename(id            = "id1",
                      PI_last_name  = "last_name",
                      PI_given_names = "given_names")
    }
    
    if(investigators == TRUE) {
      project_investigator_assoc <-
        "project_investigator_assoc" %>%
        make_rds_path(p_path) %>%
        get_rds() %>% 
        dplyr::filter(.data$id1 %in% projects_tibble$id)
      
      projects_tibble <-
        project_investigator_assoc %>%
        dplyr::left_join(projects_tibble, by = c("id1" = "id")) %>% 
        dplyr::left_join(authors_tibble, by = c("id2" = "id")) %>% 
        dplyr::rename(id                   = "id1",
                      investig_last_name   = "last_name",
                      investig_given_names = "given_names")
    }
  }
  return(projects_tibble)
}

#' @importFrom rlang .data
#' @export
authors <- function(id, affiliations = FALSE) {
  
  p_path         <- p_path_internal()
  authors_tibble <- "authors" %>% make_rds_path(p_path) %>% get_rds()
  
  if(!missing(id)) {
    test_id_entry(id = id, what = "author")
    
    if(!all(id %in% authors_tibble$id)) {
      stop("At least one author id not found")
    }
    
    id <- rlang::enquo(id)
    authors_tibble <- authors_tibble %>% dplyr::filter(.data$id %in% !!id)
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
      dplyr::filter(.data$id1 %in% authors_tibble)
    
    author_affiliation_assoc %>% 
      dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>% 
      dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>% 
      dplyr::rename(id = id1, affiliation_id = id2) %>% 
      dplyr::select(id, last_name:email, affiliation_id, department_name,
                    institution_name)
  }
  else {
    authors_tibble
  }
}

#' @export
affiliations <- function(id) {
  
  p_path              <- p_path_internal()
  affiliations_tibble <- "affiliations" %>% make_rds_path(p_path) %>% get_rds()
  
  if(!missing(id)) {
    test_id_entry(id = id, what = "affiliation")
    
    if(!all(id %in% authors_tibble$id)) {
      stop("At least one affiliation id not found")
    }
    
    id <- rlang::enquo(id)
    authors_tibble <- authors_tibble %>% dplyr::filter(.data$id %in% !!id)
  }
  "affiliations" %>% make_rds_path(p_path) %>% get_rds()
}
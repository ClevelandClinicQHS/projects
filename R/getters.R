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


#' @export
affiliations <- function(id, authors = FALSE) {
  
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
                    author_id, last_name:given_names)
  }
  else {
    affiliations_tibble
  }
}




#' @importFrom rlang .data
#' @export
authors <- function(id, affiliations = FALSE) {
  
  p_path         <- p_path_internal()
  authors_tibble <-
    "authors" %>%
    make_rds_path(p_path = p_path) %>%
    get_rds() %>% 
    dplyr::arrange(last_name, given_names)
  
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
      dplyr::filter(.data$id1 %in% authors_tibble$id) %>% 
      dplyr::arrange(.data$id1, .data$id2)
    
    author_affiliation_assoc %>% 
      dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>% 
      dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>% 
      dplyr::rename(id = id1, affiliation_id = id2) %>% 
      dplyr::full_join(authors_tibble) %>% 
      dplyr::select(id,             last_name:email,
                    affiliation_id, department_name:institution_name)
  }
  else {
    authors_tibble
  }
}




#' @importFrom rlang .data
#' @export
projects <- function(id, PI = FALSE, investigators = FALSE) {
  
  p_path          <- p_path_internal()
  projects_tibble <- 
    "projects" %>% 
    make_rds_path(p_path) %>%
    get_rds() %>% 
    dplyr::arrange(id)
  
  if(!missing(id)) {
    
    test_id_entry(id = id, what = "project")
    
    if(!all(id %in% projects_tibble$id)) {
      stop("At least one project id not found")
    }
    id              <- rlang::enquo(id)
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
                      .data$PI_last_name:.data$PI_given_names)
    }
    
    if(investigators == TRUE) {
      project_investigator_assoc <-
        "project_investigator_assoc" %>%
        make_rds_path(p_path) %>%
        get_rds() %>% 
        dplyr::filter(.data$id1 %in% projects_tibble$id) %>% 
        dplyr::arrange(.data$id1, .data$id2)
      
      projects_tibble <-
        project_investigator_assoc %>%
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
        dplyr::arrange(.data$id)
    }
  }
  
  return(projects_tibble)
}

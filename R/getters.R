################################################################################
################################################################################
# This is a user-friendly wrapper for p_path_internal() below.
#' @export
projects_folder <- function() {
  p_path_internal(error = FALSE)
}
################################################################################
################################################################################


################################################################################
################################################################################
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
  projects_tibble <- "projects" %>% make_rds_path() %>% get_rds()
  
  if(!missing(id)) {
    checkmate::assert_integerish(id, lower = 1, upper = 9999,
                                 any.missing = FALSE)
    if(!all(id %in% projects_tibble$id)) {
      stop("At least one id not found")
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
        dplyr::filter(.data$project_id %in% projects_tibble$id)
      
      projects_tibble <- 
        project_PI_assoc %>% 
        dplyr::left_join(projects_tibble, by = c("project_id" = "id")) %>% 
        dplyr::left_join(authors_tibble, by = c("PI_id" = "id")) %>% 
        dplyr::rename(id            = "project_id",
                      PI_last_name  = "last_name",
                      PI_given_names = "given_names")
    }
    
    if(investigators == TRUE) {
      project_investigator_assoc <-
        "project_investigator_assoc" %>%
        make_rds_path(p_path) %>%
        get_rds() %>% 
        dplyr::filter(.data$project_id %in% projects_tibble$id)
      
      projects_tibble <-
        project_investigator_assoc %>%
        dplyr::left_join(projects_tibble, by = c("project_id" = "id")) %>% 
        dplyr::left_join(authors_tibble, by = c("investigator_id" = "id")) %>% 
        dplyr::rename(id                   = "project_id",
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
    checkmate::assert_integerish(x = id, lower = 1, upper = 9999)
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
      dplyr::filter(.data$author_id %in% authors_tibble)
    
    author_affiliation_assoc %>% 
      dplyr::left_join(authors_tibble, by = c("author_id" = "id")) %>% 
      dplyr::left_join(affiliations_tibble, by = c("affiliation_id" = "id")) %>% 
      dplyr::rename(id = "author_id")
  }
  else {
    authors_tibble
  }
}

#' @export
affiliations <- function() {
  "affiliations" %>% make_rds_path() %>% get_rds()
}
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
    get_rds() %>% 
    dplyr::arrange(.data$department_name, .data$institution_name)
  
  if(!missing(affiliation)) {
    affiliation         <- validate_entry(affiliation,
                                          what        = "affiliation",
                                          rds_tibble  = affiliations_tibble)
    affiliations_tibble <-
      dplyr::filter(affiliations_tibble, .data$id %in% affiliation)
  }
  
  if(authors) {
    authors_tibble <-
      "authors" %>% 
      make_rds_path(p_path) %>% 
      get_rds() #%>% 
      #dplyr::select("id", "last_name", "given_names")
    
    author_affiliation_assoc <-
      "author_affiliation_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds() #%>% 
      #dplyr::filter(.data$id2 %in% affiliations_tibble$id) %>% 
      #dplyr::arrange(.data$id2, .data$id1) 
    
    affiliations_tibble <- 
      affiliations_tibble %>% 
      dplyr::left_join(author_affiliation_assoc, by = c("id" = "id2")) %>% 
      dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>% 
      dplyr::rename("author_id" = "id1")
    
        # dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>% 
        # dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>% 
        # dplyr::rename(id = id2, author_id = id1) %>% 
        # dplyr::full_join(affiliations_tibble) %>% 
        # dplyr::select(id,        department_name:address,
        #               author_id, last_name:given_names) %>% 
        # suppressMessages())
  }
  
  return(affiliations_tibble)
}




#' @importFrom rlang .data
#' @export
authors <- function(author, affiliations = FALSE, projects = FALSE) {
  
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
    
    authors_tibble <- dplyr::filter(authors_tibble, .data$id %in% author)
  }
  
  if(affiliations) {
    affiliations_tibble <-
      "affiliations" %>% 
      make_rds_path(p_path) %>% 
      get_rds() #%>% 
      #dplyr::select("id", "department_name", "institution_name")
    
    author_affiliation_assoc <-
      "author_affiliation_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds() #%>% 
      # dplyr::filter(.data$id1 %in% authors_tibble$id) %>% 
      # dplyr::arrange(.data$id1, .data$id2)
    
    authors_tibble <-
      authors_tibble %>% 
      dplyr::left_join(author_affiliation_assoc, by = c("id" = "id1")) %>% 
      dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>% 
      dplyr::rename("affiliation_id" = "id2")
      
      # author_affiliation_assoc %>% 
      # dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>% 
      # dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>% 
      # dplyr::rename(id = id1, affiliation_id = id2) %>% 
      # dplyr::full_join(authors_tibble) %>% 
      # dplyr::select(id,             last_name:email,
      #               affiliation_id, department_name:institution_name) %>% 
      # suppressMessages
  }
  
  if(projects) {
    projects_tibble <-
      "projects" %>% 
      make_rds_path(p_path) %>% 
      get_rds()
    
    project_author_assoc <-
      "project_author_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds()
    
    authors_tibble <-
      authors_tibble %>% 
      dplyr::left_join(project_author_assoc, by = c("id" = "id2")) %>% 
      dplyr::left_join(projects_tibble, by = c("id1" = "id"),
                        suffix = c("_of_author", "_of_project")) %>% 
      dplyr::rename("project_id" = "id1")
      
      # project_author_assoc %>% 
      # dplyr::left_join(authors_tibble, by = c("id1" = "id")) %>% 
      # dplyr::left_join(affiliations_tibble, by = c("id2" = "id")) %>% 
      # dplyr::rename(id = id1, affiliation_id = id2) %>% 
      # dplyr::full_join(authors_tibble) %>% 
      # dplyr::select(id,             last_name:email,
      #               affiliation_id, department_name:institution_name) %>% 
      # suppressMessages
  }
  
  return(authors_tibble)
}




#' @importFrom rlang .data
#' @export
projects <- function(project, authors = FALSE) {
  
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
    
    projects_tibble <- dplyr::filter(projects_tibble, .data$id %in% project)
  }
  
  if(authors) {
    
    authors_tibble <- 
      "authors" %>% 
      make_rds_path(p_path) %>% 
      get_rds() #%>% 
    #dplyr::select("id", "last_name", "given_names")
    
    project_author_assoc <-
      "project_author_assoc" %>%
      make_rds_path(p_path) %>%
      get_rds() #%>% 
    # dplyr::filter(.data$id1 %in% projects_tibble$id) %>% 
    # dplyr::arrange(.data$id1, .data$id2)
    
    projects_tibble <-
      projects_tibble %>% 
      dplyr::left_join(project_author_assoc, by = c("id" = "id1")) %>% 
      dplyr::left_join(authors_tibble, by = c("id2" = "id"),
                       suffix = c("_of_project", "_of_author")) %>%
      dplyr::rename("author_id" = "id2")
    
    # project_author_assoc %>%
    # dplyr::left_join(projects_tibble, by = c("id1" = "id")) %>% 
    # dplyr::mutate(tmp = TRUE) %>% 
    # dplyr::left_join(authors_tibble, by = c("id2" = "id")) %>% 
    # dplyr::rename(id                   = id1,
    #               investig_id          = id2,
    #               investig_last_name   = last_name,
    #               investig_given_names = given_names) %>% 
    # dplyr::full_join(projects_tibble) %>%
    # dplyr::select(.data$id, .data$title:.data$tmp, .data$investig_id,
    #               .data$investig_last_name:.data$investig_given_names) %>% 
    # dplyr::select(-.data$tmp) %>% 
    # dplyr::arrange(.data$id) %>% 
    # suppressMessages
  }
  
  return(projects_tibble)
}



#' @importFrom rlang .data
#' @export
print_header <- function(project) {
  
  p_path  <- p_path_internal()
  
  project <- validate_entry(project,
                            what       = "project",
                            max.length = 1,
                            rds_tibble = get_rds(make_rds_path("projects",
                                                               p_path)))
  
  projects_tibble <-
    "projects" %>% 
    make_rds_path(p_path) %>% 
    get_rds()
  
  authors_tibble      <- 
    "authors" %>% 
    make_rds_path(p_path) %>% 
    get_rds()
  
  affiliations_tibble <- 
    "affiliations" %>% 
    make_rds_path(p_path) %>% 
    get_rds()
  
  project_author_assoc <- 
    "project_author_assoc" %>% 
    make_rds_path(p_path) %>% 
    get_rds()
  
  author_affiliation_assoc <-
    "author_affiliation_assoc" %>% 
    make_rds_path(p_path) %>% 
    get_rds()
  
  title  <- dplyr::filter(projects_tibble, .data$id == project)$title
  header <- authors_affils_header(project,
                                  authors_tibble,
                                  affiliations_tibble,
                                  project_author_assoc,
                                  author_affiliation_assoc)
  
  taa_to_console(title, header)
}



authors_affils_header <- function(project_id,
                                  authors_tibble,
                                  affiliations_tibble,
                                  project_author_assoc,
                                  author_affiliation_assoc) {
  
  # The left_join/select/rename combo was used instead of semi_join so that the
  # order in project_author_assoc would be preserved
  project_authors <-
    project_author_assoc %>% 
    dplyr::filter(.data$id1 == project_id) %>% 
    dplyr::left_join(authors_tibble, by = c("id2" = "id")) %>% 
    dplyr::select(-"id1") %>% 
    dplyr::rename("id" = "id2")
  
  # In effect, this is author_affiliations_assoc (1) filtered to only include
  # authors on the project who have at least one affiliation and (2) all
  # affiliation information filled in.
  # It is constructed using the command sequence below in order to preserve (1)
  # the order of authors on the project and (2) the order of the affiliations of
  # each author.
  aa_assoc_complete <-
    project_authors %>% 
    dplyr::select("id1" = "id") %>% 
    dplyr::inner_join(author_affiliation_assoc, by = "id1") %>% 
    dplyr::left_join(affiliations_tibble, by = c("id2" = "id"))
  
  
  ############################################################
  # Construction of affiliations line to go in 01_protocol.Rmd
  
  if(nrow(aa_assoc_complete) > 0) {
    
    # A tibble of the unique affiliations associated with the project, with a
    # superscript assigned to each
    unique_affiliations <-
      aa_assoc_complete %>% 
      dplyr::select(-"id1") %>%
      dplyr::distinct() %>% 
      dplyr::mutate(superscript = 1:nrow(.))
    
    # In effect this adds the superscripts created in the previous command to
    # aa_assoc_complete
    aa_assoc_complete <- 
      unique_affiliations %>% 
      dplyr::select(.data$id2, .data$superscript) %>% 
      dplyr::right_join(aa_assoc_complete, by = "id2")
    
    
    affiliations_lines <- character()
    for(a in 1:nrow(unique_affiliations)) {
      
      affiliation_line <- paste0("^", a, "^ ",
                                 unique_affiliations$department_name[a])
      
      if(!is.na(unique_affiliations$institution_name[a])) {
        affiliation_line <- paste0(affiliation_line, ", ",
                                   unique_affiliations$institution_name[a])
      }
      
      if(!is.na(unique_affiliations$address[a])) {
        affiliation_line <- paste0(affiliation_line, ", ",
                                   unique_affiliations$address[a])
      }
      
      affiliations_lines <- append(affiliations_lines, c("", affiliation_line))
    }
  }
  else {
    affiliations_lines <- ""
  }
  ######################################################
  ######################################################
  
  
  
  ######################################################
  # Construction of author line to go in 01_protocol.Rmd
  author_line         <- "**_"
  
  for(x in 1:nrow(project_authors)) {
    
    if(x != 1) {
      author_line <- paste0(author_line, " ")
      
      if(x == nrow(project_authors) && x > 1) {
        author_line <- paste0(author_line, "and ")
      }
    }
    
    first_piece <- TRUE
    
    if(!is.na(project_authors$given_names[x])) {
      author_line <- paste0(author_line, project_authors$given_names[x])
      first_piece <- FALSE
    }
    
    if(!is.na(project_authors$last_name[x])) {
      if(!first_piece) {
        author_line <- paste0(author_line, " ")
      }
      
      author_line <- paste0(author_line, project_authors$last_name[x])
    }
    
    if(!is.na(project_authors$degree[x])) {
      author_line <- paste0(author_line, ", ", project_authors$degree[x])
    }
    
    if(x != nrow(project_authors) && nrow(project_authors) > 2) {
      author_line <- paste0(author_line, "; ")
    }
    
    x_affiliations <- dplyr::filter(aa_assoc_complete, 
                                    .data$id1 == project_authors$id[x])
    
    if(nrow(x_affiliations) > 0) {
      author_line <- paste0(author_line,
                            "^",
                            paste(sort(x_affiliations$superscript),
                                  collapse = ","),
                            "^")
    }
  }
  
  author_line <- paste0(author_line, "_**")
  ######################################################
  ######################################################
  
  return(c(author_line, affiliations_lines))
}


taa_to_console <- function(title, header) {
  cat('\ntitle: "', title, '"', '\n\n', sep = "")
  cat(header, sep = '\n')
}
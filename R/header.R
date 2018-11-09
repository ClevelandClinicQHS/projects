#' @importFrom rlang .data
#' @export
print_header <- function(project) {
  
  p_path  <- p_path_internal()
  
  projects_tibble <- get_rds(make_rds_path("projects", p_path))
  
  project <- validate_entry(project,
                            what       = "project",
                            max.length = 1,
                            rds_tibble = projects_tibble)
  
  print_header_internal(project_id  = project,
                        p_path      = p_path,
                        project_row = dplyr::filter(projects_tibble,
                                                    .data$id == project))
}


print_header_internal <- function(
  project_id,
  p_path                   = p_path_internal(),
  project_row              = dplyr::filter(get_rds(make_rds_path("projects",
                                                                 p_path)),
                                           .data$id == project_id),
  authors_tibble           = get_rds(make_rds_path("authors", p_path)),
  affiliations_tibble      = get_rds(make_rds_path("affiliations", p_path)),
  project_author_assoc     = get_rds(make_rds_path("project_author_assoc",
                                                  p_path)),
  author_affiliation_assoc = get_rds(make_rds_path("author_affiliation_assoc",
                                                  p_path)))
{
  taa_to_console(
    title  = project_row$title,
    header =
      authors_affils_header(
        project_id               = project_id,
        corresp_auth             = project_row$corresp_auth,
        authors_tibble           = authors_tibble,
        affiliations_tibble      = affiliations_tibble,
        project_author_assoc     = project_author_assoc,
        author_affiliation_assoc = author_affiliation_assoc))
}


authors_affils_header <- function(project_id,
                                  corresp_auth,
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
    
    
    affiliations_lines <- ""
    for(a in 1:nrow(unique_affiliations)) {
      
      affiliation_line <- paste0("| ^", a, "^ ",
                                 unique_affiliations$department_name[a])
      
      if(!is.na(unique_affiliations$institution_name[a])) {
        affiliation_line <- paste0(affiliation_line, ", ",
                                   unique_affiliations$institution_name[a])
      }
      
      if(!is.na(unique_affiliations$address[a])) {
        affiliation_line <- paste0(affiliation_line, ", ",
                                   unique_affiliations$address[a])
      }
      
      affiliations_lines <- append(affiliations_lines, affiliation_line)
    }
  }
  else {
    affiliations_lines <- character()
  }
  ######################################################
  ######################################################
  
  
  
  ######################################################
  # Construction of author line to go in 01_protocol.Rmd
  if(nrow(project_authors) > 0) {
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
        author_line <- paste0(author_line, ";")
      }
      
      x_affiliations <- dplyr::filter(aa_assoc_complete, 
                                      .data$id1 == project_authors$id[x])
      
      if(nrow(x_affiliations) > 0) {
        author_line <-
          paste0(author_line,
                 "^",
                 paste(sort(x_affiliations$superscript), collapse = ","),
                 # ifelse(
                 #   test = isTRUE(project_authors$id[x] == corresp_auth),
                 #   yes  = "\\*",
                 #   no   = ""),
                 "^")
      }
      
      if(isTRUE(project_authors$id[x] == corresp_auth)) {
        author_line <- paste0(author_line, "\\*")
      }
    }
    
    author_line <- paste0(author_line, "_**")
  }
  else {
    author_line <- character()
  }
  
  if(is.na(corresp_auth)) {
    corresp_lines <- character()
  }
  else {
    corresp_auth_row <- dplyr::filter(project_authors, .data$id == corresp_auth)
    corresp_affils   <- dplyr::filter(aa_assoc_complete,
                                      .data$id1 == corresp_auth)
    corresp_lines    <- c(ifelse(length(affiliations_lines) == 0, "", "|"),
                          "| ^\\*^ Corresponding author")
    
    if(nrow(corresp_affils) > 0) {
      corresp_lines <- append(corresp_lines,
                              paste0("|   ", corresp_affils$address[1]))
    }
    
    if(!is.na(corresp_auth_row$phone)) {
      corresp_lines <- append(corresp_lines,
                              paste0("|   ", corresp_auth_row$phone))
    }
    
    if(!is.na(corresp_auth_row$email)) {
      corresp_lines <- append(corresp_lines,
                              paste0("|   ", corresp_auth_row$email))
    }
  }
  
  ######################################################
  ######################################################
  
  return(c(author_line, affiliations_lines, corresp_lines))
}


taa_to_console <- function(title, header) {
  cat('\ntitle: "', title, '"', '\n\n\n', sep = "")
  cat(header, sep = '\n')
}
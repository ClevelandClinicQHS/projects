################################################################################
#' @export
new_affiliation <- function(department_name  = NA, institution_name = NA,
                            address          = NA, id               = NA) {
  
  test_id_entry(id, what = "affiliation", max.length = 1, any.missing = TRUE)
  
  message("New affiliation:")
  
  change_table(rds_name         = "affiliations",
               action           = "new",
               id               = id,
               department_name  = department_name,
               institution_name = institution_name,
               address          = address)
}
################################################################################



################################################################################
#' @importFrom rlang .data
#' @export
new_author <- function(last_name = NA, given_names  = NA,
                       title     = NA, affiliations,
                       degree    = NA, email        = NA,
                       id        = NA) {
  
  p_path         <- p_path_internal()
  
  test_id_entry(id = id, what = "author", max.length = 1, any.missing = TRUE)
  
  affiliation_tibble <- get_rds(make_rds_path("affiliations", p_path))
  
  if(!missing(affiliations)) {
    test_id_entry(id   = affiliations,
                  what = "affiliation",
                  set  = affiliation_tibble$id)
  }
  
  new_author_row <- change_table(rds_name     = "authors", 
                                 p_path       = p_path,
                                 action       = "new",
                                 id           = id,
                                 last_name    = last_name,
                                 given_names  = given_names,
                                 title        = title,
                                 degree       = degree,
                                 email        = email)
  
  message("New author:")
  print(new_author_row)
  
  if(!missing(affiliations)) {
    new_author_affiliations <- 
      change_assoc(assoc_name = "author_affiliation_assoc",
                   p_path     = p_path,
                   new        = TRUE,
                   id1        = new_author_row$id,
                   id2        = affiliations)
    
    message("\nNew author's affiliations:")
    print(new_author_affiliations %>%
            dplyr::filter(.data$id1 == new_author_row$id) %>%
            dplyr::left_join(affiliation_tibble,
                             by = c("id2" = "id")) %>%
            dplyr::select(-.data$id1) %>% 
            dplyr::rename(affiliation_id = "id2"))
  }
}
################################################################################



################################################################################
#' @importFrom tibble tibble
#' @export
new_project <- function(title         = NA,   current_owner = NA,   
                        PI,                   investigators,   
                        creator       = NA,   stage         = NA,   
                        deadline_type = NA,   deadline      = as.Date(NA),
                        id            = NA,   status        = "just created",
                        
                        checklist = c("STROBE", "CONSORT")) {
  
  p_path    <- p_path_internal()
  
  checklist <- match.arg(checklist)
  
  test_id_entry(id = id, what = "project", max.length = 1, any.missing = TRUE)
  
  author_tibble  <- get_rds(make_rds_path("authors", p_path))
  
  if(!missing(PI)) {
    test_id_entry(id   = PI,
                  what = "PI",
                  set  = author_tibble$id)
    all_authors <- PI
  }
  else {
    all_authors <- integer()
  }
  
  if(!missing(investigators)) {
    test_id_entry(id   = investigators,
                  what = "investigator",
                  set  = author_tibble$id)
    all_authors <- all_authors %>% append(investigators) %>% unique
  }
  
  new_project_row <- change_table(rds_name      = "projects",
                                  p_path        = p_path,
                                  action        = "new",
                                  id            = id,
                                  title         = title,
                                  current_owner = current_owner,
                                  creator       = creator,
                                  stage         = stage,
                                  deadline_type = deadline_type,
                                  deadline      = deadline,
                                  status        = status)
  
  if(!missing(PI)) {
    new_project_PI_assoc <-
      change_assoc(assoc_name = "project_PI_assoc",
                   p_path     = p_path,
                   new        = TRUE,
                   id1        = new_project_row$id,
                   id2        = PI)
  }
  if(!missing(investigators)) {
    new_project_investigator_assoc <-
      change_assoc(assoc_name = "project_investigator_assoc",
                   p_path     = p_path,
                   new        = TRUE,
                   id1        = new_project_row$id,
                   id2        = investigators)
  }
  
  
  if(checklist == "STROBE") {
    protocol <- STROBE_template
  }
  else if(checklist == "CONSORT") {
    protocol <- CONSORT_template
  }
  
  if(length(all_authors > 0)) {
    
    author_line        <- ""
    affiliations_lines <- character()
    
    author_affiliation_assoc <-
      get_rds(make_rds_path("author_affiliation_assoc", p_path))
    
    affiliations_tibble <-
      get_rds(make_rds_path("affiliations", p_path))
    
    project_authors <-
      purrr::map_dfr(all_authors,
                     function(i)
                       dplyr::filter(author_tibble, .data$id == i))
    
    project_affiliations <-
      purrr::map_dfr(all_authors,
                     function(i)
                       author_affiliation_assoc %>% 
                       dplyr::filter(.data$id1 == i) %>% 
                       dplyr::arrange(.data$id2)
                     ) %>% 
      dplyr::left_join(affiliations_tibble, by = c("id2" = "id"))
    
    if(nrow(project_affiliations) > 0) {
      
      unique_affiliations <-
        project_affiliations %>% 
        dplyr::select(-.data$id1) %>%
        dplyr::distinct() %>% 
        dplyr::mutate(superscript = 1:nrow(.))
      
      project_affiliations <- 
        unique_affiliations %>% 
        dplyr::select(id2, superscript) %>% 
        dplyr::right_join(project_affiliations, by = "id2")
      
      for(a in 1:nrow(unique_affiliations)) {
        
        affiliation_line <- paste0("^", a, "^ ",
                                   unique_affiliations$department_name[a])
        if(!is.na(project_affiliations$institution_name[a])) {
          affiliation_line <- paste0(affiliation_line, ", ",
                                     unique_affiliations$institution_name[a])
        }
        if(!is.na(project_affiliations$address[a])) {
          affiliation_line <- paste0(affiliation_line, ", ",
                                    unique_affiliations$address[a])
        }
        affiliations_lines <- append(affiliations_lines, affiliation_line)
      }
    }
    
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
      
      x_affiliations <- 
        project_affiliations %>% 
        dplyr::filter(.data$id1 == project_authors$id[x])
      
      if(nrow(x_affiliations) > 0) {
        author_line <- paste0(author_line,
                              "^",
                              paste(x_affiliations$superscript, collapse = ","),
                              "^")
      }
    }
    
    header <- c("", title, "", author_line, "", affiliations_lines)
    
    protocol <- append(protocol, header,
                       after = grep("---", protocol, fixed = TRUE)[2])
  }
  
  pXXXX_name <- make_project_name(new_project_row$id)
  pXXXX_path <- make_project_path(pXXXX_name, p_path)
  
  fs::dir_create(fs::path(pXXXX_path, c("data", "progs", "manuscript",
                                        "figures")))
  
  readr::write_lines(protocol,
                     fs::path(pXXXX_path, "progs/01_protocol", ext = "Rmd"))
  
  readr::write_lines("Bibliography",
                     fs::path(pXXXX_path, pXXXX_name, ext = "bib"))
  
  readr::write_lines(Rproj_template, # Rproj_template is in sysdata.Rda
                     fs::path(pXXXX_path, pXXXX_name, ext = "Rproj"))
  
  
  message("Project ", new_project_row$id, " has been created at ", pXXXX_path)
  print(new_project_row)
  
  if(!missing(PI)) {
    message("\nNew project's PI(s):")
    print(new_project_PI_assoc %>% 
            dplyr::left_join(author_tibble,
                             by = c("id2" = "id")) %>% 
            dplyr::select(-.data$id1) %>% 
            dplyr::rename(PI_id = id2))
  }
  
  if(!missing(investigators)) {
    message("\nNew project's investigators:")
    print(new_project_investigator_assoc %>% 
            dplyr::left_join(author_tibble,
                             by = c("id2" = "id")) %>% 
            dplyr::select(-.data$id1) %>% 
            dplyr::rename(investigator_id = id2))
  }
}
################################################################################



################################################################################
################################################################################
# The internal function that prints associations
# print_association <- function(main_tibble_name, main_tibble,
#                               assoc_name, assoc_tibble) {
# 
#   message("New ", main_tibble_name, ":")
#   print(main_tibble)
#   
#   message("\nNew ", main_tibble_name, "'s ", assoc_name, "s:")
#   print(assoc_tibble %>%
#           dplyr::filter(parse(text = paste0(".data$",
#                                             main_tibble_name, "_id %in% ",
#                                             main_tibble, "$id"))) %>%
#           dplyr::left_join(parse(text = paste0(assoc_tibble, ", by = c(\"",
#                                                assoc_name,
#                                                "_id\" = \"id\")"))) %>%
#           dplyr::select(parse(text = paste0("-.data$",
#                                             main_tibble_name, "_id"))))
# }

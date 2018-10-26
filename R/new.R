################################################################################
#' @export
new_affiliation <- function(department_name  = NA,
                            institution_name = NA,
                            address          = NA,
                            id               = NA_integer_) {
  
  validate_entry(id, what = "affiliation", max.length = 1, any.missing = TRUE)
  
  message("New affiliation:")
  
  change_table(rds_name         = "affiliations",
               p_path           = p_path_internal(),
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
                       degree    = NA,
                       email     = NA,
                       id        = NA_integer_) {
  
  p_path         <- p_path_internal()
  
  validate_entry(id, what = "author", max.length = 1, any.missing = TRUE)
  
  affiliation_tibble <- get_rds(make_rds_path("affiliations", p_path))
  
  if(!missing(affiliations)) {
    affiliations <- validate_entry(affiliations,
                                   what        = "affiliation",
                                   rds_tibble  = affiliation_tibble)
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
new_project <- function(title         = NA,         authors,
                        current_owner = NA,         creator  = NA,
                        corresp_auth  = NA,         stage    = NA,   
                        deadline_type = NA,         deadline = as.Date(NA),
                        id            = NA_integer_,
                        status        = "just created",
                        protocol      = c("STROBE", "CONSORT")) {
  
  p_path    <- p_path_internal()
  
  validate_entry(id, what = "project", max.length = 1, any.missing = TRUE)
  
  
  
  ######################
  # Validate all authors
  authors_tibble  <- get_rds(make_rds_path("authors", p_path))
  
  # If creator is left blank, the default author is used. If the default author
  # is not set, an error is thrown.
  if(is.na(creator)) {
    creator <- get_default_author_internal(
      "creator field left blank. Attempted to fill it with default user. ")$id
  }
  else {
    creator <- validate_entry(creator,
                              what       = "author",
                              max.length = 1,
                              rds_tibble = authors_tibble)
  }
  
  if(!is.na(current_owner)) {
    current_owner <- validate_entry(current_owner,
                                    what       = "author",
                                    max.length = 1,
                                    rds_tibble = authors_tibble)
  }
  
  # If authors was left blank, it is populated with the current_owner. If the 
  # current_owner was also left blank, both are populated with the creator.
  if(missing(authors)) {
    if(is.na(current_owner)) {
      current_owner <- creator
    }
    authors <- current_owner
  }
  # If there is at least one author but the current_owner is blank, it is 
  # populated with the first author.
  else {
    authors <- validate_entry(authors,
                              what       = "author",
                              rds_tibble = authors_tibble)
    if(is.na(current_owner)) {
      current_owner <- authors[1]
    }
  }
  
  # If the corresponding author was left blank, it is filled with the first
  # author.
  if(is.na(corresp_auth)) {
    corresp_auth <- authors[1]
  }
  # If the corresponding author was populated but not included in authors, it is
  # added on to the end of the authors.
  else {
    corresp_auth <- validate_entry(corresp_auth,
                                   what       = "author",
                                   max.length = 1,
                                   rds_tibble = authors_tibble)
    if(!(corresp_auth %in% authors)) {
      authors <- append(authors, corresp_auth)
    }
  }
  ############################
  ############################


  ############################
  # Protocol choice processer.
  
  protocol_upper   <- toupper(protocol)
  protocol_choices <- eval(formals()[["protocol"]])
  protocol_matches <- pmatch(protocol_upper, protocol_choices)
  
  # If the user did not leave protocol blank or choose "STROBE" or "CONSORT"
  if(identical(protocol_matches, NA_integer_)) {
    
    template_path <- fs::path(p_path, "templates", protocol)
    
    if(!fs::file_exists(template_path)) {
      stop("protocol does not match ", paste(protocol_choices, collapse = ", "), 
           ", and no custom template found at the file path ", protocol,
           " (check the case, and don't forget file extension).")
    }
    
    protocol  <- readr::read_lines(template_path)
  }
  
  else {
    protocol <- protocol_choices[protocol_matches[1]]
    
         if(protocol == "STROBE")  protocol <- STROBE_template
    else if(protocol == "CONSORT") protocol <- CONSORT_template
  }
  
  yaml_bounds <- grep("^---$", protocol)
  if(length(yaml_bounds) < 2) {
    stop("Custom template must have a yaml header. (Check that there are no ",
         "spaces before or after each ---)")
  }
  ###########################
  ###########################
  
  new_project_row <- change_table(rds_name      = "projects",
                                  p_path        = p_path,
                                  action        = "new",
                                  id            = id,
                                  title         = title,
                                  current_owner = current_owner,
                                  creator       = creator,
                                  corresp_auth  = corresp_auth,
                                  stage         = stage,
                                  deadline_type = deadline_type,
                                  deadline      = deadline,
                                  status        = status)
  
  new_project_author_assoc <-
    change_assoc(assoc_name = "project_author_assoc",
                 p_path     = p_path,
                 new        = TRUE,
                 id1        = new_project_row$id,
                 id2        = authors)
  
  project_authors_tbl <- dplyr::filter(authors_tibble, .data$id %in% authors)
  
  author_affiliation_assoc <-
    get_rds(make_rds_path("author_affiliation_assoc", p_path))
  
  affiliations_tibble <-
    get_rds(make_rds_path("affiliations", p_path))
  
  project_affiliations <-
    dplyr::filter(author_affiliation_assoc, .data$id1 %in% authors) %>% 
    dplyr::left_join(affiliations_tibble, by = c("id2" = "id"))
  
  
  ############################################################
  # Construction of affiliations line to go in 01_protocol.Rmd
  
  if(nrow(project_affiliations) > 0) {
    
    affiliations_lines <- character()
    
    unique_affiliations <-
      project_affiliations %>% 
      dplyr::select(-.data$id1) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(superscript = 1:nrow(.))
    
    project_affiliations <- 
      unique_affiliations %>% 
      dplyr::select(.data$id2, .data$superscript) %>% 
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
  
  for(x in 1:nrow(project_authors_tbl)) {
    
    if(x != 1) {
      author_line <- paste0(author_line, " ")
      
      if(x == nrow(project_authors_tbl) && x > 1) {
        author_line <- paste0(author_line, "and ")
      }
    }
    
    first_piece <- TRUE
    
    if(!is.na(project_authors_tbl$given_names[x])) {
      author_line <- paste0(author_line, project_authors_tbl$given_names[x])
      first_piece <- FALSE
    }
    
    if(!is.na(project_authors_tbl$last_name[x])) {
      if(!first_piece) {
        author_line <- paste0(author_line, " ")
      }
      
      author_line <- paste0(author_line, project_authors_tbl$last_name[x])
    }
    
    if(!is.na(project_authors_tbl$degree[x])) {
      author_line <- paste0(author_line, ", ", project_authors_tbl$degree[x])
    }
    
    if(x != nrow(project_authors_tbl) && nrow(project_authors_tbl) > 2) {
      author_line <- paste0(author_line, ";")
    }
    
    x_affiliations <- 
      project_affiliations %>% 
      dplyr::filter(.data$id1 == project_authors_tbl$id[x])
    
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
  
  
  ######################################################
  # Write title and/or header to progs/01_protocol.Rmd
  # 
  header   <- c("", author_line, affiliations_lines, "", "\\pagebreak", "")
  protocol <- append(protocol, header, after = yaml_bounds[2])
  
  title    <- tools::toTitleCase(title)
  protocol <- append(protocol,
                     paste0('title: "', title, '"'),
                     after = yaml_bounds[1])
  ######################################################
  ######################################################
  
  
  
  ######################################################
  # Actual creation of the folders and files
  pXXXX_name <- make_project_name(new_project_row$id)
  pXXXX_path <- make_project_path(pXXXX_name, p_path)
  
  fs::dir_create(fs::path(pXXXX_path, c("data", "data_raw", "progs",
                                        "manuscript", "figures")))
  
  readr::write_lines(protocol,
                     fs::path(pXXXX_path, "progs/01_protocol", ext = "Rmd"))
  
  readr::write_lines("Bibliography",
                     fs::path(pXXXX_path, pXXXX_name, ext = "bib"))
  
  readr::write_lines(Rproj_template, # Rproj_template is in sysdata.Rda
                     fs::path(pXXXX_path, pXXXX_name, ext = "Rproj"))
  ######################################################
  ######################################################
  
  
  message("Project ", new_project_row$id, " has been created at ", pXXXX_path)
  print(tibble::as_tibble(new_project_row) %>% 
          dplyr::select(-.data$current_owner, -.data$creator,
                        -.data$corresp_auth))
  
  message("\nNew project's authors:")
  print(new_project_author_assoc %>% 
          dplyr::left_join(authors_tibble,
                           by = c("id2" = "id")) %>% 
          dplyr::select(-.data$id1) %>% 
          dplyr::rename(author_id = id2))
  
  message("\nCurrent owner:")
  print(dplyr::filter(authors_tibble, .data$id == current_owner))
  
  message("\nCreator:")
  print(dplyr::filter(authors_tibble, .data$id == creator))
  
  message("\nCorresponding author:")
  print(dplyr::filter(authors_tibble, .data$id == corresp_auth))
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

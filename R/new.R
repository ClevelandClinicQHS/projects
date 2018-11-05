


################################################################################
#' @importFrom tibble tibble
#' @export
new_project <- function(title         = NA,         authors,
                        current_owner = NA,         creator  = NA,
                        corresp_auth  = NA,         stage    = NA,   
                        deadline_type = NA,         deadline = NA,
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
    
    authors <- validate_entry(all.vars(authors),
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
  
  
  # Add new row to project list
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
                                  deadline      = as.Date(deadline),
                                  status        = status)
  
  # Add row(s) to project-author association table
  project_author_assoc <-
    change_assoc(assoc_path   = make_rds_path("project_author_assoc", p_path),
                 new          = TRUE,
                 id1          = new_project_row$id,
                 id2          = authors)
  
  
  affiliations_tibble <-
    get_rds(make_rds_path("affiliations", p_path))
  
  author_affiliation_assoc <-
    get_rds(make_rds_path("author_affiliation_assoc", p_path))
  
  
  header <-
    authors_affils_header(project_id               = new_project_row$id,
                          authors_tibble           = authors_tibble,
                          affiliations_tibble      = affiliations_tibble,
                          project_author_assoc     = project_author_assoc,
                          author_affiliation_assoc = author_affiliation_assoc)
  
  ######################################################
  # Write title and/or header to progs/01_protocol.Rmd
  #
  header   <- c("", header, "", "\\pagebreak", "")
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
  print(project_author_assoc %>% 
          dplyr::filter(.data$id1 == new_project_row$id) %>% 
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
#' @importFrom rlang .data
#' @export
new_author <- function(last_name = NA, given_names  = NA,
                       title     = NA, affiliations,
                       degree    = NA,
                       email     = NA,
                       id        = NA_integer_) {
  
  p_path         <- p_path_internal()
  
  validate_entry(id, what = "author", max.length = 1, any.missing = TRUE)
  
  if(!missing(affiliations)) {
    affiliation_tibble <- get_rds(make_rds_path("affiliations", p_path))
    affiliations       <- validate_entry(affiliations,
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
  
  authors(author = new_author_row$id, affiliations = TRUE)
  
  message("New author:")
  print(new_author_row)
  
  if(!missing(affiliations)) {
    new_author_affiliations <- 
      change_assoc(#assoc_name = "author_affiliation_assoc",
                   #p_path     = p_path,
                   assoc_path = make_rds_path("author_affiliation_assoc",
                                              p_path),
                   new        = TRUE,
                   id1        = new_author_row$id,
                   id2        = affiliations) %>% 
      dplyr::filter(.data$id1 == new_author_row$id)
    
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
